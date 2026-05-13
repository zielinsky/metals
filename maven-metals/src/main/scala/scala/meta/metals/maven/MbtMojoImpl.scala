package scala.meta.metals.maven

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.{util => ju}

import scala.jdk.CollectionConverters._

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

  def run(mojo: MbtMojo): Unit = {
    val log = mojo.getLog
    val projects = mojo.getReactorProjects.asScala
      .filterNot(_.getPackaging == "pom")
      .toList

    log.info(s"metals-maven-plugin: exporting ${projects.size} module(s)")

    val reactorCoords =
      projects.map(p => (p.getGroupId, p.getArtifactId, p.getVersion)).toSet

    val localRepoBase = mojo.getLocalRepositoryBasedir

    // Collect all unique external dep coordinates across all projects upfront,
    // then resolve their sources JARs in one batched Aether call (parallel I/O).
    val allExternalCoords: Set[(String, String, String)] = projects.flatMap {
      project =>
        val resolved = project.getArtifacts.asScala.collect {
          case a if a.getFile != null =>
            (a.getGroupId, a.getArtifactId, a.getVersion)
        }.toSet
        val fallback = project.getDependencies.asScala.map { dep =>
          (dep.getGroupId, dep.getArtifactId, resolveVersion(dep, project))
        }
        (if (resolved.nonEmpty) resolved else fallback.toSet)
          .filterNot(reactorCoords.contains)
          .filter(_._3 != null)
    }.toSet

    // Always check local M2 cache (free — file existence only).
    // Only hit the network when downloadSources=true.
    val sourcesCache: Map[String, File] =
      if (mojo.isDownloadSources)
        resolveSourcesJarsBatch(allExternalCoords, localRepoBase, mojo, log)
      else
        resolveLocalSourcesOnly(allExternalCoords, localRepoBase)

    val depModuleMap = new ju.LinkedHashMap[String, DepModuleEntry]()
    val namespaces = new ju.LinkedHashMap[String, NamespaceJson]()

    for (project <- projects) {
      val cc = extractCompilerConfig(project)
      val javaHome = resolveJavaHome(cc.javacOptions, mojo)
      val mainName = project.getArtifactId
      namespaces.put(
        mainName,
        buildNamespace(
          project,
          mainName,
          isTest = false,
          reactorCoords,
          depModuleMap,
          cc,
          javaHome,
          localRepoBase,
          sourcesCache,
          log,
        ),
      )
      namespaces.put(
        s"$mainName:test",
        buildNamespace(
          project,
          mainName,
          isTest = true,
          reactorCoords,
          depModuleMap,
          cc,
          javaHome,
          localRepoBase,
          sourcesCache,
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
    log.info(s"metals-maven-plugin: wrote $out")
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

    // Merge plugin-level config with execution-level configs (same as bloop-maven-plugin)
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

    // --release takes precedence; if set, skip source/target
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
    // Merge plugin-level config with execution-level configs (same as bloop-maven-plugin)
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
        val scalaVersion = childText(cfg, "scalaVersion")
          .orElse(childText(cfg, "scalaCompatVersion"))
          .orNull
        (scalacArgs, scalaVersion)
    }
  }

  private def childText(dom: Xpp3Dom, name: String): Option[String] =
    Option(dom.getChild(name)).map(_.getValue).filter(_.nonEmpty)

  private def resolveJavaHome(
      javacOptions: List[String],
      mojo: MbtMojo,
  ): String =
    jdkVersionFromOptions(javacOptions)
      .flatMap(ver => toolchainHome(ver, mojo))
      .orElse(Option(System.getenv("JAVA_HOME")).filter(_.nonEmpty))
      .orElse(Option(System.getProperty("java.home")).filter(_.nonEmpty))
      .orNull

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

  private def buildNamespace(
      project: MavenProject,
      mainName: String,
      isTest: Boolean,
      reactorCoords: Set[(String, String, String)],
      depModuleMap: ju.LinkedHashMap[String, DepModuleEntry],
      cc: CompilerConfig,
      javaHome: String,
      localRepoBase: File,
      sourcesCache: Map[String, File],
      log: org.apache.maven.plugin.logging.Log,
  ): NamespaceJson = {
    val sources = existingSources(
      if (isTest) project.getTestCompileSourceRoots
      else project.getCompileSourceRoots,
      project.getBasedir,
    )
    val (depModuleIds, dependsOn) =
      collectDeps(
        project,
        compileScope = !isTest,
        reactorCoords,
        depModuleMap,
        localRepoBase,
        sourcesCache,
        log,
      )
    val allDependsOn =
      if (isTest) (mainName :: dependsOn).distinct else dependsOn
    NamespaceJson(
      sources = sources.asJava,
      scalacOptions = cc.scalacOptions.asJava,
      javacOptions = cc.javacOptions.asJava,
      dependencyModules = depModuleIds.asJava,
      scalaVersion = cc.scalaVersion,
      javaHome = javaHome,
      dependsOn = allDependsOn.asJava,
    )
  }

  private def existingSources(
      roots: ju.List[String],
      projectBaseDir: File,
  ): List[String] = {
    val declared = roots.asScala
      .map(new File(_))
      .filter(_.exists())
      .map(_.getAbsolutePath)
      .toList

    // Pick up generated source dirs that code generators add after generate-sources.
    // These directories may not be registered on MavenProject when running without
    // a prior generate-sources phase, so we probe well-known locations on disk.
    val generatedParents = List(
      new File(projectBaseDir, "target/generated-sources"),
      new File(projectBaseDir, "target/generated-test-sources"),
    )
    val generated = generatedParents
      .filter(_.isDirectory)
      .flatMap(p => Option(p.listFiles()).toList.flatten)
      .filter(_.isDirectory)
      .map(_.getAbsolutePath)
      .filterNot(declared.toSet)

    (declared ++ generated).distinct
  }

  private val CompileScopes = Set(
    Artifact.SCOPE_COMPILE,
    Artifact.SCOPE_PROVIDED,
    Artifact.SCOPE_SYSTEM,
    null,
  )

  private def collectDeps(
      project: MavenProject,
      compileScope: Boolean,
      reactorCoords: Set[(String, String, String)],
      depModuleMap: ju.LinkedHashMap[String, DepModuleEntry],
      localRepoBase: File,
      sourcesCache: Map[String, File],
      log: org.apache.maven.plugin.logging.Log,
  ): (List[String], List[String]) = {
    val depModuleIds = List.newBuilder[String]
    val dependsOn = List.newBuilder[String]

    val resolved: Map[(String, String, String), File] =
      project.getArtifacts.asScala.collect {
        case a if a.getFile != null =>
          (a.getGroupId, a.getArtifactId, a.getVersion) -> a.getFile
      }.toMap

    def addDep(g: String, a: String, v: String): Unit = {
      if (v == null || v.isEmpty) return
      val coords = (g, a, v)
      if (reactorCoords.contains(coords)) {
        dependsOn += a
      } else {
        val id = s"$g:$a:$v"
        if (!depModuleMap.containsKey(id)) {
          val jar =
            resolved.getOrElse(coords, localJarPath(localRepoBase, g, a, v))
          if (jar != null) {
            val sourcesJar = sourcesCache.getOrElse(id, null)
            depModuleMap.put(
              id,
              DepModuleEntry(
                id = id,
                jar = jar.toURI.toString,
                sources =
                  if (sourcesJar != null) sourcesJar.toURI.toString else null,
              ),
            )
            depModuleIds += id
          } else
            log.debug(s"metals-maven-plugin: $id not found, skipping")
        } else
          depModuleIds += id
      }
    }

    if (resolved.nonEmpty) {
      val scopeFilter: Artifact => Boolean =
        if (compileScope) a => CompileScopes.contains(a.getScope)
        else _ => true
      for (a <- project.getArtifacts.asScala if scopeFilter(a))
        addDep(a.getGroupId, a.getArtifactId, a.getVersion)
    } else {
      for (dep <- project.getDependencies.asScala) {
        val scope = dep.getScope
        val inScope =
          if (compileScope)
            scope == null || scope == Artifact.SCOPE_COMPILE ||
            scope == Artifact.SCOPE_PROVIDED || scope == Artifact.SCOPE_SYSTEM
          else true
        if (inScope)
          addDep(
            dep.getGroupId,
            dep.getArtifactId,
            resolveVersion(dep, project),
          )
      }
    }

    (depModuleIds.result(), dependsOn.result().distinct)
  }

  private def resolveVersion(dep: Dependency, project: MavenProject): String = {
    val v = dep.getVersion
    if (v == null || v.isEmpty) {
      val key = s"${dep.getGroupId}:${dep.getArtifactId}:jar"
      Option(project.getManagedVersionMap.get(key))
        .map(_.getBaseVersion)
        .orNull
    } else v
  }

  private def localJarPath(
      base: File,
      g: String,
      a: String,
      v: String,
  ): File = {
    val jar = new File(
      base,
      g.replace('.', '/') + "/" + a + "/" + v + "/" + a + "-" + v + ".jar",
    )
    if (jar.isFile) jar else null
  }

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
