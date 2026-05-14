package scala.meta.metals.maven

import java.nio.file.Files
import java.nio.file.Path

import scala.jdk.CollectionConverters._

import org.apache.maven.project.MavenProject

private[maven] object JavaHomeResolver {

  import MavenPluginSupport._

  def resolve(
      javacOptions: List[String],
      project: MavenProject,
      isTest: Boolean,
      mojo: MbtMojo,
  ): Option[String] =
    fromForkExecutable(project, isTest)
      .orElse(fromCompilerJdkToolchain(project, isTest, mojo))
      .orElse(fromMavenSessionToolchain(mojo))
      .orElse(fromToolchainsXml(project, javacOptions, mojo))
      .orElse(sys.props.get("java.home").filter(_.nonEmpty))

  private[maven] def selectJavaHome(
      project: MavenProject,
      candidates: List[String],
  ): String = {
    val requirement = javaRequirement(project)
    val javaHome =
      if (requirement.isEmpty) candidates.headOption
      else candidates.find(javaHomeSupports(_, requirement))
    javaHome.orNull
  }

  // Returns the JDK toolchain already selected by maven-toolchains-plugin in this session.
  private def fromMavenSessionToolchain(mojo: MbtMojo): Option[String] = {
    val tm = mojo.getToolchainManager
    if (tm == null) return None
    Option(tm.getToolchainFromBuildContext("jdk", mojo.getSession))
      .flatMap(tc => Option(tc.findTool("java")))
      .flatMap(javaExe =>
        Option(Path.of(javaExe).getParent).flatMap(p => Option(p.getParent))
      )
      .map(_.toString)
  }

  private[maven] def fromForkExecutable(
      project: MavenProject,
      isTest: Boolean,
  ): Option[String] = {
    val plugins = effectivePlugins(project)
    for {
      plugin <- Option(plugins.get(JavaCompilerPlugin))
      cfg <- compilerPluginConfiguration(plugin, isTest)
      _ <- childText(cfg, "fork").filter(_.equalsIgnoreCase("true"))
      rawExecutable <- childText(cfg, "executable").filter(_.nonEmpty)
      interpolated = interpolatePath(rawExecutable, project)
      // Skip bare command names like "javac" — only useful if it looks like a path
      if interpolated.contains("/") || interpolated.contains(
        java.io.File.separator
      )
      executable = absolutePath(interpolated, project)
      path = java.nio.file.Path.of(executable)
      if path.isAbsolute
      parent <- Option(path.getParent)
      grandparent <- Option(parent.getParent)
    } yield grandparent.toString
  }

  // Deprecated compat overload for tests that don't pass isTest
  private[maven] def resolveFromForkExecutable(
      project: MavenProject
  ): Option[String] =
    fromForkExecutable(project, isTest = false)

  private[maven] def resolveFromForkExecutable(
      project: MavenProject,
      isTest: Boolean,
  ): Option[String] = fromForkExecutable(project, isTest)

  private def fromCompilerJdkToolchain(
      project: MavenProject,
      isTest: Boolean,
      mojo: MbtMojo,
  ): Option[String] = {
    val plugins = effectivePlugins(project)
    for {
      plugin <- Option(plugins.get(JavaCompilerPlugin))
      cfg <- compilerPluginConfiguration(plugin, isTest)
      jdkToolchain <- Option(cfg.getChild("jdkToolchain"))
      home <- {
        val reqs = jdkToolchain.getChildren.flatMap { child =>
          Option(child.getValue).filter(_.nonEmpty).map(v => child.getName -> v)
        }.toMap
        if (reqs.isEmpty) None
        else toolchainHomes(reqs, mojo).headOption
      }
    } yield home
  }

  // Looks up toolchains by requirements from maven-toolchains-plugin config,
  // project properties, or --release / -source from javacOptions (last resort hint).
  private def fromToolchainsXml(
      project: MavenProject,
      javacOptions: List[String],
      mojo: MbtMojo,
  ): Option[String] = {
    val reqs = toolchainsPluginRequirements(project)
      .orElse(propertyRequirements(project))
      .orElse(javacOptionsRequirements(javacOptions))
    reqs.flatMap(r => toolchainHomes(r, mojo).headOption)
  }

  private[maven] def javacOptionsRequirements(
      javacOptions: List[String]
  ): Option[Map[String, String]] = {
    val version = javacOptions.sliding(2).collectFirst {
      case Seq("--release", v) => v
      case Seq("-source", v) => v
    }
    version.map(v => Map("version" -> v))
  }

  private def toolchainsPluginRequirements(
      project: MavenProject
  ): Option[Map[String, String]] = {
    val plugins = effectivePlugins(project)
    val plugin = Option(plugins.get(MavenToolchainsPlugin)).orNull
    if (plugin == null) return None
    val cfgs = plugin.getExecutions.asScala
      .flatMap(e =>
        Option(e.getConfiguration)
          .map(_.asInstanceOf[org.codehaus.plexus.util.xml.Xpp3Dom])
      )
      .toList
    val topCfg = Option(plugin.getConfiguration)
      .map(_.asInstanceOf[org.codehaus.plexus.util.xml.Xpp3Dom])
    val reqs = (cfgs ++ topCfg.toList)
      .flatMap { cfg =>
        Option(cfg.getChild("toolchains")).toList.flatMap { toolchains =>
          Option(toolchains.getChild("jdk")).toList.map { jdk =>
            jdk.getChildren.flatMap { child =>
              Option(child.getValue)
                .filter(_.nonEmpty)
                .map(v => child.getName -> v)
            }.toMap
          }
        }
      }
      .filter(_.nonEmpty)
    reqs.headOption
  }

  private def propertyRequirements(
      project: MavenProject
  ): Option[Map[String, String]] = {
    val versions = requiredJdkVersions(project)
    val vendors = requiredJdkVendors(project)
    if (versions.isEmpty && vendors.isEmpty) None
    else
      Some(
        Map.empty[String, String] ++
          versions.headOption.map("version" -> _) ++
          vendors.headOption.map("vendor" -> _)
      )
  }

  private case class JavaRequirement(
      major: Option[Int],
      vendors: List[String],
  ) {
    def isEmpty: Boolean = major.isEmpty && vendors.isEmpty
    override def toString: String = {
      val parts = major.map(v => s"version>=$v").toList ++
        (if (vendors.nonEmpty) List(s"vendor in [${vendors.mkString(", ")}]")
         else Nil)
      parts.mkString(", ")
    }
  }

  private def javaRequirement(project: MavenProject): JavaRequirement = {
    val versions = requiredJdkVersions(project)
    JavaRequirement(
      major = versions.flatMap(versionMajor).reduceOption(_ max _),
      vendors = requiredJdkVendors(project),
    )
  }

  private def requiredJdkVersions(project: MavenProject): List[String] = {
    val props = project.getProperties
    List(
      "air.java.version",
      "java.version",
      "jdk.version",
      "project.build.targetJdk",
    ).flatMap(key => Option(props.getProperty(key)).filter(_.nonEmpty))
  }

  private def requiredJdkVendors(project: MavenProject): List[String] =
    Option(effectivePlugins(project).get(MavenEnforcerPlugin)).toList
      .flatMap(mergedPluginConfiguration)
      .flatMap { cfg =>
        for {
          rules <- Option(cfg.getChild("rules")).toList
          requireVendor <- Option(rules.getChild("requireJavaVendor")).toList
          includes <- Option(requireVendor.getChild("includes")).toList
          include <- includes.getChildren("include").toList
          vendor <- Option(include.getValue).filter(_.nonEmpty)
        } yield vendor
      }
      .distinct

  private def toolchainHomes(
      reqs: Map[String, String],
      mojo: MbtMojo,
  ): List[String] = {
    val tm = mojo.getToolchainManager
    if (tm == null) return Nil
    val tcs = tm.getToolchains(mojo.getSession, "jdk", reqs.asJava)
    Option(tcs).toList
      .flatMap(_.asScala.toList)
      .flatMap { tc =>
        Option(tc.findTool("java")).flatMap { javaExe =>
          Option(Path.of(javaExe).getParent)
            .flatMap(p => Option(p.getParent))
            .map(_.toString)
        }
      }
  }

  private def javaHomeSupports(
      javaHome: String,
      requirement: JavaRequirement,
  ): Boolean = {
    val release = javaRelease(javaHome)
    val versionOk =
      requirement.major.forall(major =>
        release.flatMap(_.version).exists(_ >= major)
      )
    val vendorOk =
      requirement.vendors.isEmpty ||
        release.flatMap(_.implementor).exists { actual =>
          requirement.vendors
            .exists(required => vendorMatches(actual, required))
        }
    versionOk && vendorOk
  }

  private def vendorMatches(actual: String, required: String): Boolean = {
    val actualAliases = vendorAliases(actual)
    val requiredAliases = vendorAliases(required)
    actualAliases.exists(requiredAliases.contains)
  }

  private def vendorAliases(value: String): Set[String] = {
    val normalized = normalizeVendor(value)
    val base = Set(normalized)
    val aliases = normalized match {
      case v if v.contains("azul") || v.contains("zulu") =>
        Set("azul", "zulu", "azulsystemsinc")
      case v if v.contains("adoptium") || v.contains("temurin") =>
        Set("adoptium", "eclipseadoptium", "temurin")
      case v if v.contains("oracle") =>
        Set("oracle", "oraclecorporation")
      case v if v.contains("graalvm") =>
        Set("graalvm")
      case _ => Set.empty[String]
    }
    base ++ aliases
  }

  private def normalizeVendor(value: String): String =
    Option(value).getOrElse("").toLowerCase.replaceAll("[^a-z0-9]", "")

  private case class JavaRelease(
      version: Option[Int],
      implementor: Option[String],
  )

  private def javaRelease(javaHome: String): Option[JavaRelease] = {
    val home = Path.of(javaHome)
    val releaseFiles =
      List(
        Some(home.resolve("release")),
        Option(home.getParent).map(_.resolve("release")),
      ).flatten
    releaseFiles
      .find(Files.isRegularFile(_))
      .map { releaseFile =>
        val text = Files.readString(releaseFile)
        val JavaVersion = """(?m)^JAVA_VERSION="([^"]+)"""".r
        val Implementor = """(?m)^IMPLEMENTOR="([^"]+)"""".r
        JavaRelease(
          version = JavaVersion
            .findFirstMatchIn(text)
            .map(_.group(1))
            .flatMap(versionMajor),
          implementor = Implementor.findFirstMatchIn(text).map(_.group(1)),
        )
      }
  }

  private def versionMajor(version: String): Option[Int] =
    version match {
      case null => None
      case v if v.startsWith("1.") =>
        v.drop(2).takeWhile(_.isDigit).toIntOption
      case v =>
        v.takeWhile(_.isDigit).toIntOption
    }
}
