package scala.meta.metals.maven

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.{util => ju}

import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.matching.Regex

import com.google.gson.GsonBuilder
import org.apache.maven.artifact.Artifact
import org.apache.maven.model.Dependency
import org.apache.maven.project.MavenProject
import org.codehaus.plexus.util.xml.Xpp3Dom
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.resolution.ArtifactRequest
import org.eclipse.aether.resolution.ArtifactResolutionException

object MbtMojoImpl {

  private val ScalaMavenPlugin = "net.alchim31.maven:scala-maven-plugin"
  private val JavaCompilerPlugin =
    "org.apache.maven.plugins:maven-compiler-plugin"
  private val MavenEnforcerPlugin =
    "org.apache.maven.plugins:maven-enforcer-plugin"
  private val Antlr4MavenPlugin = "org.antlr:antlr4-maven-plugin"
  private val BuildHelperMavenPlugin =
    "org.codehaus.mojo:build-helper-maven-plugin"

  private def emit(msg: String): Unit = { println(msg); System.out.flush() }

  def run(mojo: MbtMojo): Unit = {
    val log = mojo.getLog
    val projects = mojo.getReactorProjects.asScala
      .filterNot(_.getPackaging == "pom")
      .toList

    emit(s"[metals-maven] exporting ${projects.size} module(s)")

    val reactorByCoords: Map[(String, String, String), MavenProject] =
      projects
        .map(p => (p.getGroupId, p.getArtifactId, p.getVersion) -> p)
        .toMap

    val localRepoBase = mojo.getLocalRepositoryBasedir

    val allExternalCoords: Set[(String, String, String)] = projects.flatMap {
      project =>
        project.getArtifacts.asScala
          .map(a => (a.getGroupId, a.getArtifactId, a.getVersion))
          .filterNot(reactorByCoords.keySet.contains)
          .filter(_._3 != null)
    }.toSet

    val sourcesCache: Map[String, File] =
      if (mojo.isDownloadSources) {
        emit(
          s"[metals-maven] resolving sources JARs for ${allExternalCoords.size} dependencies..."
        )
        val result =
          resolveSourcesJarsBatch(allExternalCoords, localRepoBase, mojo, log)
        emit(
          s"[metals-maven] sources resolved: ${result.size}/${allExternalCoords.size}"
        )
        result
      } else {
        val result = resolveLocalSourcesOnly(allExternalCoords, localRepoBase)
        emit(
          s"[metals-maven] sources from local cache: ${result.size}/${allExternalCoords.size}"
        )
        result
      }

    val artifactFiles = resolveDependencyJars(
      projects,
      reactorByCoords.keySet,
      localRepoBase,
      mojo,
      log,
    )

    val depModuleMap = new ju.LinkedHashMap[String, DepModuleEntry]()
    val namespaces = new ju.LinkedHashMap[String, NamespaceJson]()

    for (project <- projects) {
      val cc = extractCompilerConfig(project)
      val javaHome = resolveJavaHome(cc.javacOptions, project, mojo)
      val mainName =
        s"${project.getGroupId}:${project.getArtifactId}:${project.getVersion}"
      emit(s"[metals-maven] namespace: $mainName")
      namespaces.put(
        mainName,
        buildNamespace(
          project,
          mainName,
          isTest = false,
          reactorByCoords,
          depModuleMap,
          cc,
          javaHome,
          localRepoBase,
          artifactFiles,
          sourcesCache,
          mojo,
          log,
        ),
      )
      namespaces.put(
        s"$mainName:test",
        buildNamespace(
          project,
          mainName,
          isTest = true,
          reactorByCoords,
          depModuleMap,
          cc,
          javaHome,
          localRepoBase,
          artifactFiles,
          sourcesCache,
          mojo,
          log,
        ),
      )
    }

    val build = MbtBuildJson(
      dependencyModules = new ju.ArrayList[DepModuleEntry](depModuleMap.values),
      namespaces = namespaces,
    )

    val out = mojo.getOutputFile.toPath
    Option(out.getParent).foreach(p => Files.createDirectories(p))
    val gson = new GsonBuilder().serializeNulls().setPrettyPrinting().create()
    Files.writeString(out, gson.toJson(build))
    emit(
      s"[metals-maven] wrote ${namespaces.size()} namespaces, ${depModuleMap.size()} dependencies → $out"
    )
  }

  private case class CompilerConfig(
      javacOptions: List[String],
      scalacOptions: List[String],
      scalaVersion: String,
  )

  private def extractCompilerConfig(project: MavenProject): CompilerConfig = {
    val plugins = project.getBuild.getPluginsAsMap
    val props = project.getProperties

    def prop(key: String): Option[String] =
      Option(props.getProperty(key)).filter(_.nonEmpty)

    val javacOpts = extractJavacOptions(plugins, prop)
    val (scalacOpts, scalaVer) = extractScalaOptions(plugins)

    CompilerConfig(javacOpts, scalacOpts, scalaVer)
  }

  private def extractJavacOptions(
      plugins: ju.Map[String, org.apache.maven.model.Plugin],
      prop: String => Option[String],
  ): List[String] = {
    val args = List.newBuilder[String]

    val cfgOpt: Option[Xpp3Dom] =
      Option(plugins.get(JavaCompilerPlugin)).flatMap { plugin =>
        val pluginCfg =
          Option(plugin.getConfiguration).map(_.asInstanceOf[Xpp3Dom])
        val execCfgs = plugin.getExecutions.asScala
          .flatMap(e => Option(e.getConfiguration).map(_.asInstanceOf[Xpp3Dom]))
        (execCfgs.toSeq ++ pluginCfg).reduceOption(Xpp3Dom.mergeXpp3Dom)
      }

    val release =
      cfgOpt
        .flatMap(childText(_, "release"))
        .orElse(prop("maven.compiler.release"))
    val source =
      cfgOpt
        .flatMap(childText(_, "source"))
        .orElse(prop("maven.compiler.source"))
    val target =
      cfgOpt
        .flatMap(childText(_, "target"))
        .orElse(prop("maven.compiler.target"))

    release match {
      case Some(v) => args += "--release" += v
      case None =>
        source.foreach(v => args += "-source" += v)
        target.foreach(v => args += "-target" += v)
    }

    val encoding =
      cfgOpt
        .flatMap(childText(_, "encoding"))
        .orElse(prop("project.build.sourceEncoding"))
        .orElse(prop("maven.compiler.encoding"))
    encoding.foreach(v => args += "-encoding" += v)

    val enablePreview =
      cfgOpt
        .flatMap(childText(_, "enablePreview"))
        .orElse(prop("air.compiler.enable-preview"))
    if (enablePreview.contains("true")) args += "--enable-preview"

    val parameters = cfgOpt.flatMap(childText(_, "parameters"))
    if (parameters.contains("true")) args += "-parameters"

    cfgOpt.foreach { cfg =>
      Option(cfg.getChild("compilerArgs")).foreach(
        _.getChildren.foreach(arg => args += arg.getValue)
      )
      childText(cfg, "compilerArg").foreach(args += _)
    }

    args.result()
  }

  private def extractScalaOptions(
      plugins: ju.Map[String, org.apache.maven.model.Plugin]
  ): (List[String], String) = {
    val cfgOpt = for {
      plugin <- Option(plugins.get(ScalaMavenPlugin))
      pluginCfg = Option(plugin.getConfiguration).map(_.asInstanceOf[Xpp3Dom])
      execCfgs = plugin.getExecutions.asScala
        .flatMap(e => Option(e.getConfiguration).map(_.asInstanceOf[Xpp3Dom]))
      cfg <- (execCfgs.toSeq ++ pluginCfg).reduceOption(Xpp3Dom.mergeXpp3Dom)
    } yield cfg

    cfgOpt match {
      case None => (Nil, null)
      case Some(cfg) =>
        val scalacArgs = Option(cfg.getChild("args"))
          .map(_.getChildren.map(_.getValue).toList)
          .getOrElse(Nil)
        // <addScalacArgs>-flag1 -flag2</addScalacArgs> — space-separated extra args
        val addScalacArgs = childText(cfg, "addScalacArgs")
          .map(_.split("\\|").map(_.trim).filter(_.nonEmpty).toList)
          .getOrElse(Nil)
        val scalaVersion = childText(cfg, "scalaVersion")
          .orElse(childText(cfg, "scalaCompatVersion"))
          .orNull
        (scalacArgs ++ addScalacArgs, scalaVersion)
    }
  }

  private def childText(dom: Xpp3Dom, name: String): Option[String] =
    Option(dom.getChild(name)).map(_.getValue).filter(_.nonEmpty)

  private def resolveJavaHome(
      javacOptions: List[String],
      project: MavenProject,
      mojo: MbtMojo,
  ): String = {
    val requirement = javaRequirement(javacOptions, project)
    val requestedVersions =
      (jdkVersionFromOptions(javacOptions).toList ++ requiredJdkVersions(
        project
      )).distinct
    val candidates =
      requestedVersions.flatMap(ver => toolchainHome(ver, mojo)) :::
        List(
          Option(System.getenv("JAVA_HOME")).filter(_.nonEmpty),
          Option(System.getProperty("java.home")).filter(_.nonEmpty),
        ).flatten

    val javaHome =
      if (requirement.isEmpty) candidates.headOption
      else candidates.find(javaHomeSupports(_, requirement))

    javaHome.orNull
  }

  private case class JavaRequirement(
      major: Option[Int],
      vendors: List[String],
  ) {
    def isEmpty: Boolean = major.isEmpty && vendors.isEmpty
  }

  private def javaRequirement(
      javacOptions: List[String],
      project: MavenProject,
  ): JavaRequirement = {
    val versions =
      jdkVersionFromOptions(javacOptions).toList ++ requiredJdkVersions(project)
    JavaRequirement(
      major = versions.flatMap(versionMajor).reduceOption(_ max _),
      vendors = requiredJdkVendors(project),
    )
  }

  private def jdkVersionFromOptions(
      javacOptions: List[String]
  ): Option[String] = {
    def after(flag: String): Option[String] = {
      val i = javacOptions.indexOf(flag)
      if (i >= 0 && i + 1 < javacOptions.size) Some(javacOptions(i + 1))
      else None
    }
    after("--release").orElse(after("-source")).orElse(after("-target"))
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
    Option(project.getBuild.getPluginsAsMap.get(MavenEnforcerPlugin)).toList
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

  private def toolchainHome(version: String, mojo: MbtMojo): Option[String] = {
    val tm = mojo.getToolchainManager
    if (tm == null) return None
    val reqs = Map("version" -> version).asJava
    val tcs = tm.getToolchains(mojo.getSession, "jdk", reqs)
    Option(tcs)
      .flatMap(_.asScala.headOption)
      .flatMap(tc => Option(tc.findTool("java")))
      .flatMap { javaExe =>
        // /path/to/jdk/bin/java  →  /path/to/jdk
        Option(Path.of(javaExe).getParent)
          .flatMap(p => Option(p.getParent))
          .map(_.toString)
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
        release.flatMap(_.implementor).exists(requirement.vendors.contains)
    versionOk && vendorOk
  }

  private case class JavaRelease(
      version: Option[Int],
      implementor: Option[String],
  )

  private def javaVersionMajor(javaHome: String): Option[Int] = {
    javaRelease(javaHome).flatMap(_.version)
  }

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

  private def buildNamespace(
      project: MavenProject,
      mainName: String,
      isTest: Boolean,
      reactorByCoords: Map[(String, String, String), MavenProject],
      depModuleMap: ju.LinkedHashMap[String, DepModuleEntry],
      cc: CompilerConfig,
      javaHome: String,
      localRepoBase: File,
      artifactFiles: Map[ArtifactKey, File],
      sourcesCache: Map[String, File],
      mojo: MbtMojo,
      log: org.apache.maven.plugin.logging.Log,
  ): NamespaceJson = {
    val sources = existingSources(
      if (isTest) project.getTestCompileSourceRoots
      else project.getCompileSourceRoots,
      project,
      isTest,
    )

    val depModuleIds = collectDeps(
      project,
      isTest,
      reactorByCoords,
      depModuleMap,
      localRepoBase,
      artifactFiles,
      sourcesCache,
      log,
    )

    val dependsOn =
      computeDependsOn(project, mainName, isTest, reactorByCoords, mojo)

    NamespaceJson(
      sources = sources.asJava,
      scalacOptions = cc.scalacOptions.asJava,
      javacOptions = cc.javacOptions.asJava,
      dependencyModules = depModuleIds.asJava,
      scalaVersion = cc.scalaVersion,
      javaHome = javaHome,
      dependsOn = dependsOn.asJava,
    )
  }

  private def computeDependsOn(
      project: MavenProject,
      mainName: String,
      isTest: Boolean,
      reactorByCoords: Map[(String, String, String), MavenProject],
      mojo: MbtMojo,
  ): List[String] = {
    val graphUpstream: Option[List[MavenProject]] = Try {
      mojo.getSession.getProjectDependencyGraph
        .getUpstreamProjects(project, false)
        .asScala
        .toList
    }.toOption

    val artifactUpstream: List[MavenProject] =
      project.getArtifacts.asScala
        .flatMap(a =>
          reactorByCoords.get((a.getGroupId, a.getArtifactId, a.getVersion))
        )
        .toList
        .distinct

    val upstream = graphUpstream.getOrElse(artifactUpstream)

    val filteredUpstream: List[MavenProject] =
      if (isTest) upstream.filter(_.getPackaging != "pom")
      else {
        val compileReactorCoords: Set[(String, String, String)] =
          project.getArtifacts.asScala
            .filter(a => CompileScopes.contains(a.getScope))
            .flatMap(a =>
              reactorByCoords.get((a.getGroupId, a.getArtifactId, a.getVersion))
            )
            .map(p => (p.getGroupId, p.getArtifactId, p.getVersion))
            .toSet
        upstream.filter(p =>
          p.getPackaging != "pom" &&
            compileReactorCoords.contains(
              (p.getGroupId, p.getArtifactId, p.getVersion)
            )
        )
      }

    val mainDepsOn = filteredUpstream.map(p =>
      s"${p.getGroupId}:${p.getArtifactId}:${p.getVersion}"
    )

    val testJarDepsOn: List[String] =
      if (!isTest) Nil
      else
        project.getDependencies.asScala
          .filter(d =>
            d.getType == "test-jar" || Option(d.getClassifier).contains("tests")
          )
          .flatMap { d =>
            val v = resolveVersion(d, project)
            reactorByCoords.get((d.getGroupId, d.getArtifactId, v))
          }
          .map(p => s"${p.getGroupId}:${p.getArtifactId}:${p.getVersion}:test")
          .toList

    val selfMain = if (isTest) List(mainName) else Nil

    (selfMain ++ mainDepsOn ++ testJarDepsOn).distinct
  }

  private def existingSources(
      roots: ju.List[String],
      project: MavenProject,
      isTest: Boolean,
  ): List[String] = {
    val declared = roots.asScala
      .map(new File(_))
      .filter(_.exists())
      .map(_.getAbsolutePath)
      .toList

    val scalaDir = new File(
      project.getBasedir,
      if (isTest) "src/test/scala" else "src/main/scala",
    )
    val scalaFallback =
      if (scalaDir.isDirectory && !declared.contains(scalaDir.getAbsolutePath))
        List(scalaDir.getAbsolutePath)
      else Nil

    val configuredGenerated =
      configuredGeneratedSourceRoots(project, isTest)

    val generatedParent = new File(
      project.getBasedir,
      if (isTest) "target/generated-test-sources"
      else "target/generated-sources",
    )
    val generated =
      if (generatedParent.isDirectory)
        generatedSourceRootsFromDirectory(
          generatedParent,
          isTest,
          declared.toSet,
        )
      else Nil

    (declared ++ scalaFallback ++ configuredGenerated ++ generated).distinct
  }

  private def generatedSourceRootsFromDirectory(
      generatedParent: File,
      isTest: Boolean,
      declared: Set[String],
  ): List[String] = {
    val children = Option(generatedParent.listFiles()).toList.flatten
      .filter(_.isDirectory)
      .map(_.getAbsolutePath)
      .filterNot(declared)

    if (isTest) generatedParent.getAbsolutePath :: children
    else children
  }

  private def configuredGeneratedSourceRoots(
      project: MavenProject,
      isTest: Boolean,
  ): List[String] = {
    val plugins = project.getBuild.getPluginsAsMap
    val antlrRoots =
      if (!isTest && plugins.containsKey(Antlr4MavenPlugin)) {
        val plugin = plugins.get(Antlr4MavenPlugin)
        val outputDirectory =
          mergedPluginConfiguration(plugin)
            .flatMap(childText(_, "outputDirectory"))
            .map(interpolatePath(_, project))
            .getOrElse(
              new File(
                project.getBuild.getDirectory,
                "generated-sources/antlr4",
              ).getPath
            )
        List(absolutePath(outputDirectory, project))
      } else Nil

    val buildHelperRoots =
      Option(plugins.get(BuildHelperMavenPlugin)).toList.flatMap { plugin =>
        val goal = if (isTest) "add-test-source" else "add-source"
        plugin.getExecutions.asScala.toList
          .filter(_.getGoals.asScala.contains(goal))
          .flatMap(e => Option(e.getConfiguration).map(_.asInstanceOf[Xpp3Dom]))
          .flatMap(buildHelperSources(_, project))
      }

    (antlrRoots ++ buildHelperRoots).distinct
  }

  private def mergedPluginConfiguration(
      plugin: org.apache.maven.model.Plugin
  ): Option[Xpp3Dom] = {
    val pluginCfg =
      Option(plugin.getConfiguration).map(_.asInstanceOf[Xpp3Dom])
    val execCfgs = plugin.getExecutions.asScala
      .flatMap(e => Option(e.getConfiguration).map(_.asInstanceOf[Xpp3Dom]))
    (execCfgs.toSeq ++ pluginCfg).reduceOption(Xpp3Dom.mergeXpp3Dom)
  }

  private def buildHelperSources(
      cfg: Xpp3Dom,
      project: MavenProject,
  ): List[String] =
    Option(cfg.getChild("sources")).toList.flatMap { sources =>
      sources.getChildren("source").toList.map { source =>
        absolutePath(interpolatePath(source.getValue, project), project)
      }
    }

  private def absolutePath(path: String, project: MavenProject): String = {
    val file = new File(path)
    if (file.isAbsolute) file.getAbsolutePath
    else new File(project.getBasedir, path).getAbsolutePath
  }

  private val PropertyRef: Regex = """\$\{([^}]+)\}""".r

  private def interpolatePath(value: String, project: MavenProject): String =
    PropertyRef.replaceAllIn(
      value,
      m =>
        Regex.quoteReplacement(
          propertyValue(m.group(1), project).getOrElse(m.matched)
        ),
    )

  private def propertyValue(
      name: String,
      project: MavenProject,
  ): Option[String] =
    name match {
      case "basedir" | "project.basedir" =>
        Some(project.getBasedir.getAbsolutePath)
      case "build.directory" | "project.build.directory" =>
        Some(project.getBuild.getDirectory)
      case _ =>
        Option(project.getProperties.getProperty(name))
    }

  private val CompileScopes = Set(
    Artifact.SCOPE_COMPILE,
    Artifact.SCOPE_PROVIDED,
    Artifact.SCOPE_SYSTEM,
    null,
  )

  private case class ArtifactKey(
      groupId: String,
      artifactId: String,
      version: String,
      classifier: Option[String],
      extension: String,
  )

  private def resolveDependencyJars(
      projects: List[MavenProject],
      reactorCoords: Set[(String, String, String)],
      localRepoBase: File,
      mojo: MbtMojo,
      log: org.apache.maven.plugin.logging.Log,
  ): Map[ArtifactKey, File] = {
    val keys = projects
      .flatMap { project =>
        project.getArtifacts.asScala.flatMap { artifact =>
          val coords =
            (artifact.getGroupId, artifact.getArtifactId, artifact.getVersion)
          Option.when(!reactorCoords.contains(coords))(artifactKey(artifact))
        }
      }
      .filter(key => key.version != null && key.extension == "jar")
      .distinct

    val localFiles = keys.flatMap { key =>
      Option(
        localArtifactPath(
          localRepoBase,
          key.groupId,
          key.artifactId,
          key.version,
          key.classifier,
          key.extension,
        )
      ).map(key -> _)
    }.toMap

    val missing = keys.filterNot(localFiles.contains)
    if (missing.isEmpty) localFiles
    else {
      emit(
        s"[metals-maven] resolving ${missing.size} dependency JAR(s) missing from local cache..."
      )
      localFiles ++ resolveDependencyJarsBatch(missing, mojo, log)
    }
  }

  private def resolveDependencyJarsBatch(
      keys: List[ArtifactKey],
      mojo: MbtMojo,
      log: org.apache.maven.plugin.logging.Log,
  ): Map[ArtifactKey, File] = {
    val remoteRepos =
      mojo.getSession.getCurrentProject.getRemoteProjectRepositories
    val requests = keys.map { key =>
      val req = new ArtifactRequest()
      req.setArtifact(
        new DefaultArtifact(
          key.groupId,
          key.artifactId,
          key.classifier.orNull,
          key.extension,
          key.version,
        )
      )
      req.setRepositories(remoteRepos)
      req
    }

    try {
      mojo.getRepoSystem
        .resolveArtifacts(mojo.getRepositorySession, requests.asJava)
        .asScala
        .flatMap { result =>
          for {
            art <- Option(result.getArtifact)
            file <- Option(art.getFile).filter(_.isFile)
          } yield artifactKey(art) -> file
        }
        .toMap
    } catch {
      case e: ArtifactResolutionException =>
        e.getResults.asScala.flatMap { result =>
          for {
            art <- Option(result.getArtifact)
            file <- Option(art.getFile).filter(_.isFile)
          } yield artifactKey(art) -> file
        }.toMap
      case _: Exception =>
        log.debug("metals-maven-plugin: batch dependency JAR resolution failed")
        Map.empty
    }
  }

  private def collectDeps(
      project: MavenProject,
      isTest: Boolean,
      reactorByCoords: Map[(String, String, String), MavenProject],
      depModuleMap: ju.LinkedHashMap[String, DepModuleEntry],
      localRepoBase: File,
      artifactFiles: Map[ArtifactKey, File],
      sourcesCache: Map[String, File],
      log: org.apache.maven.plugin.logging.Log,
  ): List[String] = {
    val depModuleIds = List.newBuilder[String]

    def registerExternal(
        g: String,
        a: String,
        v: String,
        classifier: String,
        file: File,
    ): Unit = {
      if (v == null || v.isEmpty || file == null) return
      val id = artifactId(g, a, v, classifier)
      if (!depModuleMap.containsKey(id)) {
        if (file.exists()) {
          if (!isJar(file)) {
            log.debug(s"metals-maven-plugin: $id is not a JAR, skipping")
            return
          }
          // sources lookup always uses the base g:a:v key (no classifier)
          val sourcesJar = sourcesCache.getOrElse(s"$g:$a:$v", null)
          depModuleMap.put(
            id,
            DepModuleEntry(
              id = id,
              jar = file.toURI.toString,
              sources =
                if (sourcesJar != null) sourcesJar.toURI.toString else null,
            ),
          )
        } else {
          log.debug(s"metals-maven-plugin: $id not found, skipping")
          return
        }
      }
      depModuleIds += id
    }

    // getArtifacts() is populated by requiresDependencyCollection with the full
    // transitive tree (coordinates + scopes). getFile() may be null because
    // requiresDependencyResolution=NONE means JARs are not downloaded. We fall
    // back to localJarPath() to find JARs already present in the local M2 cache.
    val allArtifacts = project.getArtifacts.asScala

    if (allArtifacts.nonEmpty) {
      val scopeFilter: Artifact => Boolean =
        if (!isTest) a => CompileScopes.contains(a.getScope)
        else _ => true
      for (a <- allArtifacts if scopeFilter(a)) {
        val coords = (a.getGroupId, a.getArtifactId, a.getVersion)
        if (!reactorByCoords.contains(coords)) {
          val key = artifactKey(a)
          val file = Option(a.getFile)
            .filter(_.isFile)
            .orElse(
              artifactFiles.get(key)
            )
            .orNull
          registerExternal(
            a.getGroupId,
            a.getArtifactId,
            a.getVersion,
            Option(a.getClassifier).filter(_.nonEmpty).orNull,
            file,
          )
        }
      }
    } else {
      // Pure fallback: dependency collection also failed (unusual), use direct deps only
      for (dep <- project.getDependencies.asScala) {
        val scope = dep.getScope
        val inScope =
          if (!isTest)
            scope == null || scope == Artifact.SCOPE_COMPILE ||
            scope == Artifact.SCOPE_PROVIDED || scope == Artifact.SCOPE_SYSTEM
          else true
        if (inScope) {
          val v = resolveVersion(dep, project)
          val coords = (dep.getGroupId, dep.getArtifactId, v)
          if (!reactorByCoords.contains(coords))
            registerExternal(
              dep.getGroupId,
              dep.getArtifactId,
              v,
              Option(dep.getClassifier).filter(_.nonEmpty).orNull,
              localArtifactPath(
                localRepoBase,
                dep.getGroupId,
                dep.getArtifactId,
                v,
                Option(dep.getClassifier).filter(_.nonEmpty),
                dependencyExtension(dep),
              ),
            )
        }
      }
    }

    depModuleIds.result()
  }

  // Artifact ID includes classifier when present so test-jars get distinct entries.
  // Format: g:a:v  or  g:a:classifier:v
  private def artifactId(
      g: String,
      a: String,
      v: String,
      classifier: String,
  ): String =
    if (classifier == null || classifier.isEmpty) s"$g:$a:$v"
    else s"$g:$a:$classifier:$v"

  private def resolveVersion(dep: Dependency, project: MavenProject): String = {
    val v = dep.getVersion
    if (v == null || v.isEmpty) {
      val key = s"${dep.getGroupId}:${dep.getArtifactId}:jar"
      Option(project.getManagedVersionMap.get(key))
        .map(_.getBaseVersion)
        .orNull
    } else v
  }

  private def artifactKey(artifact: Artifact): ArtifactKey =
    ArtifactKey(
      groupId = artifact.getGroupId,
      artifactId = artifact.getArtifactId,
      version = artifact.getVersion,
      classifier = Option(artifact.getClassifier).filter(_.nonEmpty),
      extension = artifactExtension(artifact),
    )

  private def artifactKey(
      artifact: org.eclipse.aether.artifact.Artifact
  ): ArtifactKey =
    ArtifactKey(
      groupId = artifact.getGroupId,
      artifactId = artifact.getArtifactId,
      version = artifact.getVersion,
      classifier = Option(artifact.getClassifier).filter(_.nonEmpty),
      extension = artifact.getExtension,
    )

  private def artifactExtension(artifact: Artifact): String =
    Option(artifact.getArtifactHandler)
      .flatMap(handler => Option(handler.getExtension))
      .filter(_.nonEmpty)
      .orElse(Option(artifact.getType).filter(_.nonEmpty))
      .getOrElse("jar")

  private def dependencyExtension(dep: Dependency): String =
    Option(dep.getType).filter(_.nonEmpty) match {
      case Some("test-jar") => "jar"
      case Some(other) => other
      case None => "jar"
    }

  private def localArtifactPath(
      base: File,
      g: String,
      a: String,
      v: String,
      classifier: Option[String],
      extension: String,
  ): File = {
    if (extension != "jar") return null
    val classifierSuffix = classifier.map("-" + _).getOrElse("")
    val jar = new File(
      base,
      g.replace(
        '.',
        '/',
      ) + "/" + a + "/" + v + "/" + a + "-" + v + classifierSuffix + "." + extension,
    )
    if (jar.isFile) jar else null
  }

  private def isJar(file: File): Boolean =
    file.getName.endsWith(".jar")

  private def resolveLocalSourcesOnly(
      coords: Set[(String, String, String)],
      base: File,
  ): Map[String, File] =
    coords.flatMap { case (g, a, v) =>
      val f = localSourcesJarPath(base, g, a, v)
      if (f.isFile) Some(s"$g:$a:$v" -> f) else None
    }.toMap

  // Resolve sources JARs for all external deps in a single batched Aether call.
  // Aether resolves requests concurrently, so this is much faster than one-by-one
  // on a fresh local repository.
  private def resolveSourcesJarsBatch(
      coords: Set[(String, String, String)],
      base: File,
      mojo: MbtMojo,
      log: org.apache.maven.plugin.logging.Log,
  ): Map[String, File] = {
    val remoteRepos =
      mojo.getSession.getCurrentProject.getRemoteProjectRepositories
    val (localHits, missing) = coords.partition { case (g, a, v) =>
      localSourcesJarPath(base, g, a, v).isFile
    }

    val fromLocal: Map[String, File] = localHits.map { case (g, a, v) =>
      s"$g:$a:$v" -> localSourcesJarPath(base, g, a, v)
    }.toMap

    val requests: List[ArtifactRequest] = missing.toList.map { case (g, a, v) =>
      val req = new ArtifactRequest()
      req.setArtifact(new DefaultArtifact(g, a, "sources", "jar", v))
      req.setRepositories(remoteRepos)
      req
    }

    val fromRemote: Map[String, File] =
      if (requests.isEmpty) Map.empty
      else {
        try {
          mojo.getRepoSystem
            .resolveArtifacts(mojo.getRepositorySession, requests.asJava)
            .asScala
            .flatMap { result =>
              for {
                art <- Option(result.getArtifact)
                file <- Option(art.getFile)
                id = s"${art.getGroupId}:${art.getArtifactId}:${art.getVersion}"
              } yield id -> file
            }
            .toMap
        } catch {
          case e: ArtifactResolutionException =>
            // partial results are still available on the exception
            e.getResults.asScala.flatMap { result =>
              for {
                art <- Option(result.getArtifact)
                file <- Option(art.getFile)
                id = s"${art.getGroupId}:${art.getArtifactId}:${art.getVersion}"
              } yield id -> file
            }.toMap
          case _: Exception =>
            log.debug("metals-maven-plugin: batch sources resolution failed")
            Map.empty
        }
      }

    fromLocal ++ fromRemote
  }

  private def localSourcesJarPath(
      base: File,
      g: String,
      a: String,
      v: String,
  ): File =
    new File(
      base,
      g.replace(
        '.',
        '/',
      ) + "/" + a + "/" + v + "/" + a + "-" + v + "-sources.jar",
    )
}

private[maven] case class MbtBuildJson(
    dependencyModules: ju.Collection[DepModuleEntry],
    namespaces: ju.Map[String, NamespaceJson],
)

private[maven] case class DepModuleEntry(
    id: String,
    jar: String,
    sources: String,
)

private[maven] case class NamespaceJson(
    sources: ju.List[String],
    scalacOptions: ju.List[String],
    javacOptions: ju.List[String],
    dependencyModules: ju.List[String],
    scalaVersion: String,
    javaHome: String,
    dependsOn: ju.List[String],
)
