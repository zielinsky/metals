package scala.meta.internal.metals.mbt.importer

import java.nio.file.Files
import java.nio.file.Path

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scala.meta.internal.builds.ShellRunner
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.mbt.MbtDependencyModule
import scala.meta.internal.process.ExitCodes
import scala.meta.io.AbsolutePath

import com.google.gson.Gson
import com.google.gson.JsonElement
import com.google.gson.JsonObject

/**
 * Extracts dependencies from Bazel projects using rules_jvm_external.
 *
 * This extractor parses the maven_install.json file that rules_jvm_external
 * generates to pin Maven dependencies. It also locates the actual JAR files
 * in the Bazel output base.
 */
object BazelMavenExtractor {

  private val gson = new Gson()

  def extract(
      projectDir: AbsolutePath,
      verbose: Boolean,
      shellRunner: ShellRunner,
      outputBase: Option[Path],
  )(implicit
      ec: ExecutionContext
  ): Future[Seq[MbtDependencyModule]] = {
    // Find all maven_install.json files
    val mavenInstallPaths =
      findAllMavenInstallJson(projectDir, verbose) match {
        case paths if paths.nonEmpty => Future.successful(paths)
        case _ =>
          runMavenPin(shellRunner, projectDir).map { _ =>
            findAllMavenInstallJson(projectDir, verbose)
          }
      }

    mavenInstallPaths.map { paths =>
      val existingPaths = paths.filter(_.exists)
      if (existingPaths.isEmpty) {
        scribe.error(
          "No maven_install.json found. Make sure rules_jvm_external is configured and pinned."
        )
        Seq.empty[MbtDependencyModule]
      } else
        Try {
          // Extract from all maven_install files and merge results
          val allModules = existingPaths.flatMap { path =>
            if (verbose) println(s"Processing: $path")
            val content = path.readText
            val json = gson.fromJson(content, classOf[JsonObject])
            extractArtifacts(json, outputBase, projectDir.toNIO, verbose)
          }

          // Deduplicate by id, preferring entries with sources
          allModules
            .groupBy(_.id)
            .values
            .map { modules =>
              modules.maxBy(m => if (m.sourcesURI.isDefined) 1 else 0)
            }
            .toSeq
            .sortBy(_.id)
        } match {
          case Success(modules) => modules
          case Failure(e) =>
            scribe.error(s"Bazel extraction failed: ${e.getMessage}")
            Seq.empty[MbtDependencyModule]
        }
    }
  }

  private def runMavenPin(
      shellRunner: ShellRunner,
      projectRoot: AbsolutePath,
  )(implicit ec: ExecutionContext): Future[Unit] = {
    scribe.info(
      "bazel-mbt: no maven_install.json found, running 'bazel run @maven//:pin'"
    )
    projectRoot.resolve("maven_install.json").touch()
    shellRunner
      .run(
        "bazel-maven-pin",
        List("bazel", "run", "@maven//:pin"),
        projectRoot,
        redirectErrorOutput = false,
        javaHome = None,
        processOut = line => scribe.info(s"bazel-mbt: $line"),
        processErr = line => scribe.warn(s"bazel-mbt: $line"),
      )
      .future
      .map {
        case ExitCodes.Success =>
          scribe.info("bazel-mbt: maven_install.json generated successfully")
        case code =>
          scribe.warn(
            s"bazel-mbt: 'bazel run @maven//:pin' exited with code $code"
          )
      }
  }

  /**
   * Find all maven_install.json files in various possible locations.
   * Also searches for patterns like maven_install_*.json, *_maven_install.json
   */
  private def findAllMavenInstallJson(
      projectDir: AbsolutePath,
      verbose: Boolean,
  ): Seq[AbsolutePath] = {
    val candidates = Seq(
      // Standard location
      projectDir.resolve("maven_install.json"),
      // Bzlmod repository rule output (common pattern)
      projectDir.resolve("third_party/maven_install.json"),
      // Some projects put it in a subdirectory
      projectDir.resolve("dependencies/maven_install.json"),
      // Compatibility subdirectory
      projectDir.resolve("compatibility/maven_install.json"),
    )

    // Search for maven_install*.json or *maven*install*.json patterns in project root
    val patternMatches = Try {
      projectDir.list.filter { path =>
        val name = path.filename.toLowerCase
        name.endsWith(".json") && name.contains("maven") && name.contains(
          "install"
        )
      }.toSeq
    }.getOrElse(Seq.empty)

    val allCandidates =
      (candidates ++ patternMatches).distinct
        .filter(_.exists)

    if (verbose) {
      println("Found maven_install.json files:")
      allCandidates.foreach(p => println(s"  - $p"))
    }

    allCandidates
  }

  /**
   * Extract artifacts from maven_install.json.
   *
   * Supports two formats:
   *
   * New format (rules_jvm_external v5+):
   * {
   *   "artifacts": {
   *     "com.google.guava:guava": {
   *       "version": "31.1-jre",
   *       "shasums": { "jar": "...", "sources": "..." }
   *     }
   *   }
   * }
   *
   * Legacy format (rules_jvm_external v4 and earlier):
   * {
   *   "dependency_tree": {
   *     "dependencies": [
   *       { "coord": "com.google.guava:guava:31.1-jre", "file": "v1/https/..." }
   *     ]
   *   }
   * }
   */
  private def extractArtifacts(
      json: JsonObject,
      outputBase: Option[Path],
      projectDir: Path,
      verbose: Boolean,
  ): Seq[MbtDependencyModule] = {
    val repositoryName = detectRepositoryName(json)
    if (verbose) println(s"Using repository name: $repositoryName")
    val externalDir = outputBase.map(_.resolve("external"))

    // Try new format first (artifacts key at root)
    val artifactsObj = Option(json.getAsJsonObject("artifacts"))
    if (artifactsObj.isDefined) {
      return extractFromNewFormat(
        artifactsObj.get,
        repositoryName,
        externalDir,
        projectDir,
        verbose,
      )
    }

    // Try legacy format (dependency_tree.dependencies array)
    val dependencyTree = Option(json.getAsJsonObject("dependency_tree"))
    dependencyTree.flatMap { dt =>
      Option(dt.getAsJsonArray("dependencies"))
    } match {
      case Some(deps) =>
        if (verbose) println("Using legacy dependency_tree format")
        extractFromLegacyFormat(
          deps,
          repositoryName,
          externalDir,
          projectDir,
          verbose,
        )
      case None =>
        if (verbose)
          println(
            "No 'artifacts' or 'dependency_tree.dependencies' found in maven_install.json"
          )
        Seq.empty
    }
  }

  /**
   * Extract from new format: { "artifacts": { "group:artifact": { "version": "..." } } }
   */
  private def extractFromNewFormat(
      artifacts: JsonObject,
      repositoryName: String,
      externalDir: Option[Path],
      projectDir: Path,
      verbose: Boolean,
  ): Seq[MbtDependencyModule] = {
    artifacts.entrySet().asScala.toSeq.flatMap { entry =>
      val coordKey = entry.getKey // e.g., "com.google.guava:guava"
      val artifactInfo = entry.getValue

      Try {
        parseArtifact(
          coordKey,
          artifactInfo,
          repositoryName,
          externalDir,
          projectDir,
          verbose,
        )
      }.toOption.flatten
    }
  }

  /**
   * Extract from legacy format: { "dependency_tree": { "dependencies": [...] } }
   */
  private def extractFromLegacyFormat(
      dependencies: com.google.gson.JsonArray,
      repositoryName: String,
      externalDir: Option[Path],
      projectDir: Path,
      verbose: Boolean,
  ): Seq[MbtDependencyModule] = {
    val deps = dependencies.iterator().asScala.toSeq

    // Group by base coordinate to pair JARs with sources
    val jarDeps = deps.filter { elem =>
      if (!elem.isJsonObject) false
      else {
        val coord = Option(elem.getAsJsonObject.get("coord"))
          .map(_.getAsString)
          .getOrElse("")
        !coord.contains(":jar:sources:") && !coord.contains("-sources.")
      }
    }

    jarDeps.flatMap { elem =>
      parseLegacyDependency(
        elem,
        deps,
        repositoryName,
        externalDir,
        projectDir,
        verbose,
      )
    }
  }

  private def parseLegacyDependency(
      elem: JsonElement,
      allDeps: Seq[JsonElement],
      repositoryName: String,
      externalDir: Option[Path],
      projectDir: Path,
      verbose: Boolean,
  ): Option[MbtDependencyModule] = {
    Try {
      val obj = elem.getAsJsonObject
      val coord =
        obj.get("coord").getAsString // e.g., "com.google.guava:guava:31.1-jre"
      val file = Option(obj.get("file")).map(_.getAsString)
      val url = Option(obj.get("url")).map(_.getAsString)

      // Parse coordinate: group:artifact:version
      val parts = coord.split(":")
      if (parts.length < 3) return None

      val (groupId, artifactId, version) = (parts(0), parts(1), parts(2))

      if (version.contains("SNAPSHOT")) {
        if (verbose) println(s"  Skipping SNAPSHOT: $coord")
        return None
      }

      val id = s"$groupId:$artifactId:$version"
      val groupPath = groupId.replace('.', '/')
      val jarName = s"$artifactId-$version.jar"

      // Find JAR path - try local paths first
      val jarPath = file
        .flatMap { f =>
          findJarFromLegacyPath(f, repositoryName, externalDir, projectDir)
        }
        .orElse {
          findJarPath(
            groupId,
            artifactId,
            version,
            repositoryName,
            externalDir,
            projectDir,
            verbose,
          )
        }
        .orElse {
          // Download from URL if available and not found locally
          url.flatMap { u =>
            downloadToM2Cache(
              u,
              groupPath,
              artifactId,
              version,
              jarName,
              verbose,
            )
          }
        }

      jarPath.map { jar =>
        // Look for corresponding sources in the dependencies array
        val sourcesCoord = s"$groupId:$artifactId:jar:sources:$version"
        val sourcesEntry = allDeps.find { e =>
          e.isJsonObject &&
          Option(e.getAsJsonObject.get("coord"))
            .map(_.getAsString)
            .contains(sourcesCoord)
        }

        val sourcesFile = sourcesEntry.flatMap { e =>
          Option(e.getAsJsonObject.get("file")).map(_.getAsString)
        }
        val sourcesUrl = sourcesEntry.flatMap { e =>
          Option(e.getAsJsonObject.get("url")).map(_.getAsString)
        }

        val sourcesJarName = s"$artifactId-$version-sources.jar"
        val sourcesPath = sourcesFile
          .flatMap { f =>
            findJarFromLegacyPath(f, repositoryName, externalDir, projectDir)
          }
          .orElse {
            findSourcesPath(
              groupId,
              artifactId,
              version,
              repositoryName,
              externalDir,
              projectDir,
              verbose,
            )
          }
          .orElse {
            // Download sources from URL if available
            sourcesUrl.flatMap { u =>
              downloadToM2Cache(
                u,
                groupPath,
                artifactId,
                version,
                sourcesJarName,
                verbose,
              )
            }
          }

        MbtDependencyModule(id = id, jar = jar, sources = sourcesPath.orNull)
      }
    }.toOption.flatten
  }

  /**
   * Download a JAR from URL to the local Maven cache.
   */
  private def downloadToM2Cache(
      url: String,
      groupPath: String,
      artifactId: String,
      version: String,
      jarName: String,
      verbose: Boolean,
  ): Option[String] = {
    Try {
      val m2Dir = Path.of(
        System.getProperty("user.home"),
        ".m2",
        "repository",
        groupPath,
        artifactId,
        version,
      )
      val targetPath = m2Dir.resolve(jarName)

      // Check if already exists
      if (Files.exists(targetPath)) {
        return Some(targetPath.toString)
      }

      if (verbose) println(s"  Downloading: $url")

      // Create directory
      Files.createDirectories(m2Dir)

      // Download file
      val connection = new java.net.URL(url).openConnection()
      connection.setConnectTimeout(10000)
      connection.setReadTimeout(30000)

      val in = connection.getInputStream
      try {
        Files.copy(in, targetPath)
        if (verbose) println(s"  Downloaded to: $targetPath")
        Some(targetPath.toString)
      } finally {
        in.close()
      }
    }.toOption.flatten
  }

  /**
   * Find JAR from legacy format's "file" path.
   * The file path is like: v1/https/repo1.maven.org/maven2/com/google/guava/guava/31.1-jre/guava-31.1-jre.jar
   */
  private def findJarFromLegacyPath(
      filePath: String,
      repositoryName: String,
      externalDir: Option[Path],
      projectDir: Path,
  ): Option[String] = {
    // Try direct path in external directory with repository name prefix
    val patterns = Seq(
      s"$repositoryName/$filePath",
      s"maven/$filePath",
      filePath,
    )

    val jarFromExternal = externalDir.flatMap { extDir =>
      patterns.map(p => extDir.resolve(p)).find(Files.exists(_))
    }

    if (jarFromExternal.isDefined) return jarFromExternal.map(_.toString)

    // Try bazel-<project>/external symlink
    val bazelProjectPath = Try {
      val bazelLink = projectDir.resolve(s"bazel-${projectDir.getFileName}")
      if (Files.exists(bazelLink)) {
        val extDir = bazelLink.resolve("external")
        if (Files.exists(extDir)) Some(extDir) else None
      } else None
    }.getOrElse(None)

    bazelProjectPath
      .flatMap { extDir =>
        patterns.map(p => extDir.resolve(p)).find(Files.exists(_))
      }
      .map(_.toString)
  }

  /**
   * Detect the repository name from maven_install.json.
   * It's typically "maven" but can be customized.
   */
  private def detectRepositoryName(json: JsonObject): String = {
    // Try to find it in the JSON metadata
    Option(json.get("__AUTOGENERATED_FILE_DO_NOT_MODIFY_THIS_FILE_MANUALLY"))
      .orElse(Option(json.get("version")))
      .map(_ => "maven") // Default to "maven"
      .getOrElse("maven")
  }

  private def parseArtifact(
      coordKey: String,
      artifactInfo: JsonElement,
      repositoryName: String,
      externalDir: Option[Path],
      projectDir: Path,
      verbose: Boolean,
  ): Option[MbtDependencyModule] = {
    if (!artifactInfo.isJsonObject) return None

    val info = artifactInfo.getAsJsonObject

    // Get version
    val version = Option(info.get("version"))
      .filter(_.isJsonPrimitive)
      .map(_.getAsString)

    version.flatMap { v =>
      if (v.contains("SNAPSHOT")) {
        if (verbose) println(s"  Skipping SNAPSHOT: $coordKey:$v")
        return None
      }

      val parts = coordKey.split(":")
      if (parts.length != 2) return None

      val (groupId, artifactId) = (parts(0), parts(1))
      val id = s"$groupId:$artifactId:$v"

      // Find JAR file path
      val jarPath = findJarPath(
        groupId,
        artifactId,
        v,
        repositoryName,
        externalDir,
        projectDir,
        verbose,
      )

      jarPath.map { jar =>
        val sourcesPath = findSourcesPath(
          groupId,
          artifactId,
          v,
          repositoryName,
          externalDir,
          projectDir,
          verbose,
        )

        MbtDependencyModule(
          id = id,
          jar = jar,
          sources = sourcesPath.orNull,
        )
      }
    }
  }

  /**
   * Find the JAR file in the Bazel external directory.
   *
   * Bazel stores external dependencies in different patterns:
   *
   * Bzlmod mode (Bazel 6+):
   *   {output_base}/external/rules_jvm_external~{version}~maven~{artifact_dir}_{version}/file/v1/{group_path}/{artifact}/{version}/{artifact}-{version}.jar
   *   Example: rules_jvm_external~6.2~maven~com_google_guava_guava_32_1_3_android/file/v1/com/google/guava/guava/32.1.3-android/guava-32.1.3-android.jar
   *
   * WORKSPACE mode:
   *   {output_base}/external/maven/v1/https/repo1.maven.org/maven2/{group_path}/{artifact}/{version}/{artifact}-{version}.jar
   */
  private def findJarPath(
      groupId: String,
      artifactId: String,
      version: String,
      repositoryName: String,
      externalDir: Option[Path],
      projectDir: Path,
      verbose: Boolean,
  ): Option[String] = {
    val groupPath = groupId.replace('.', '/')
    val jarName = s"$artifactId-$version.jar"

    // Build Bzlmod-style directory name: com.google.guava:guava:32.1.3-android -> com_google_guava_guava_32_1_3_android
    val bzlmodArtifactDir = toBzlmodArtifactDir(groupId, artifactId, version)

    // Try Bzlmod patterns first
    val jarFromBzlmod = externalDir.flatMap { extDir =>
      findBzlmodJar(
        extDir,
        bzlmodArtifactDir,
        groupPath,
        artifactId,
        version,
        jarName,
        verbose,
      )
    }

    if (jarFromBzlmod.isDefined) return jarFromBzlmod.map(_.toUri.toString)

    // Try WORKSPACE patterns
    val workspacePatterns = Seq(
      s"$repositoryName/v1/https/repo1.maven.org/maven2/$groupPath/$artifactId/$version/$jarName",
      s"maven/v1/https/repo1.maven.org/maven2/$groupPath/$artifactId/$version/$jarName",
      s"maven/$groupPath/$artifactId/$version/$jarName",
    )

    val jarFromWorkspace = externalDir.flatMap { extDir =>
      workspacePatterns.map(p => extDir.resolve(p)).find(Files.exists(_))
    }

    if (jarFromWorkspace.isDefined)
      return jarFromWorkspace.map(_.toUri.toString)

    // Try bazel-<project>/external symlink
    val bazelProjectPath = Try {
      val bazelLink = projectDir.resolve(s"bazel-${projectDir.getFileName}")
      if (Files.exists(bazelLink)) {
        val extDir = bazelLink.resolve("external")
        if (Files.exists(extDir)) Some(extDir) else None
      } else None
    }.getOrElse(None)

    val jarFromBazelProject = bazelProjectPath.flatMap { extDir =>
      findBzlmodJar(
        extDir,
        bzlmodArtifactDir,
        groupPath,
        artifactId,
        version,
        jarName,
        verbose,
      )
        .orElse(
          workspacePatterns.map(p => extDir.resolve(p)).find(Files.exists(_))
        )
    }

    if (jarFromBazelProject.isDefined)
      jarFromBazelProject.map(_.toUri.toString)
    else {
      // Fall back to local Maven cache
      val m2Path = Path.of(
        System.getProperty("user.home"),
        ".m2",
        "repository",
        groupPath,
        artifactId,
        version,
        jarName,
      )

      if (Files.exists(m2Path)) Some(m2Path.toUri.toString) else None
    }
  }

  /**
   * Convert artifact coordinates to Bzlmod directory name format.
   * Example: com.google.guava:guava:32.1.3-android -> com_google_guava_guava_32_1_3_android
   */
  private def toBzlmodArtifactDir(
      groupId: String,
      artifactId: String,
      version: String,
  ): String = {
    val sanitize = (s: String) =>
      s.replace('.', '_').replace('-', '_').replace(':', '_')
    s"${sanitize(groupId)}_${sanitize(artifactId)}_${sanitize(version)}"
  }

  /**
   * Find JAR in Bzlmod external directory by scanning for rules_jvm_external prefix.
   *
   * Supports multiple Bazel versions:
   * - Bazel 7.x: rules_jvm_external~{version}~maven~{artifactDir}
   * - Bazel 8.x: rules_jvm_external++maven+{artifactDir}
   */
  private def findBzlmodJar(
      extDir: Path,
      artifactDir: String,
      groupPath: String,
      artifactId: String,
      version: String,
      jarName: String,
      verbose: Boolean,
  ): Option[Path] = {
    Try {
      val entries = Files.list(extDir).iterator().asScala.toSeq

      // Look for directory matching Bazel 7.x pattern: rules_jvm_external~{version}~maven~{artifactDir}
      val bazel7Dir = entries.find { entry =>
        val name = entry.getFileName.toString
        name.startsWith("rules_jvm_external~") &&
        name.contains("~maven~") &&
        name.endsWith(s"~$artifactDir")
      }

      // Look for directory matching Bazel 8.x pattern: rules_jvm_external++maven+{artifactDir}
      val bazel8Dir = entries.find { entry =>
        val name = entry.getFileName.toString
        name.startsWith("rules_jvm_external++maven+") &&
        name.endsWith(s"+$artifactDir")
      }

      val matchingDir = bazel7Dir.orElse(bazel8Dir)

      matchingDir.flatMap { dir =>
        val jarPath =
          dir.resolve(s"file/v1/$groupPath/$artifactId/$version/$jarName")
        if (Files.exists(jarPath)) {
          if (verbose) println(s"  Found Bzlmod JAR: $jarPath")
          Some(jarPath)
        } else {
          // Also try without the nested path (some artifacts are directly in the directory)
          val altJarPath = dir.resolve(s"file/$jarName")
          if (Files.exists(altJarPath)) {
            if (verbose) println(s"  Found Bzlmod JAR (alt): $altJarPath")
            Some(altJarPath)
          } else {
            None
          }
        }
      }
    }.getOrElse(None)
  }

  /**
   * Find the sources JAR file.
   */
  private def findSourcesPath(
      groupId: String,
      artifactId: String,
      version: String,
      repositoryName: String,
      externalDir: Option[Path],
      projectDir: Path,
      verbose: Boolean,
  ): Option[String] = {
    val groupPath = groupId.replace('.', '/')
    val sourcesJarName = s"$artifactId-$version-sources.jar"

    // Build Bzlmod-style directory name for sources: group_artifact_jar_sources_version
    val bzlmodSourcesDir = toBzlmodSourcesDir(groupId, artifactId, version)

    // Try Bzlmod patterns first
    val sourcesFromBzlmod = externalDir.flatMap { extDir =>
      findBzlmodJar(
        extDir,
        bzlmodSourcesDir,
        groupPath,
        artifactId,
        version,
        sourcesJarName,
        verbose,
      )
    }

    if (sourcesFromBzlmod.isDefined)
      return sourcesFromBzlmod.map(_.toUri.toString)

    // Try WORKSPACE patterns
    val workspacePatterns = Seq(
      s"$repositoryName/v1/https/repo1.maven.org/maven2/$groupPath/$artifactId/$version/$sourcesJarName",
      s"maven/v1/https/repo1.maven.org/maven2/$groupPath/$artifactId/$version/$sourcesJarName",
    )

    val sourcesFromWorkspace = externalDir.flatMap { extDir =>
      workspacePatterns.map(p => extDir.resolve(p)).find(Files.exists(_))
    }

    if (sourcesFromWorkspace.isDefined)
      return sourcesFromWorkspace.map(_.toUri.toString)

    // Try bazel-<project>/external symlink
    val bazelProjectPath = Try {
      val bazelLink = projectDir.resolve(s"bazel-${projectDir.getFileName}")
      if (Files.exists(bazelLink)) {
        val extDir = bazelLink.resolve("external")
        if (Files.exists(extDir)) Some(extDir) else None
      } else None
    }.getOrElse(None)

    val sourcesFromBazelProject = bazelProjectPath.flatMap { extDir =>
      findBzlmodJar(
        extDir,
        bzlmodSourcesDir,
        groupPath,
        artifactId,
        version,
        sourcesJarName,
        verbose,
      )
        .orElse(
          workspacePatterns.map(p => extDir.resolve(p)).find(Files.exists(_))
        )
    }

    if (sourcesFromBazelProject.isDefined)
      return sourcesFromBazelProject.map(_.toUri.toString)

    // Fall back to local Maven cache
    val m2Path = Path.of(
      System.getProperty("user.home"),
      ".m2",
      "repository",
      groupPath,
      artifactId,
      version,
      sourcesJarName,
    )

    if (Files.exists(m2Path)) Some(m2Path.toUri.toString) else None
  }

  /**
   * Convert artifact coordinates to Bzlmod sources directory name format.
   * Example: com.google.guava:guava:32.1.3-android -> com_google_guava_guava_jar_sources_32_1_3_android
   */
  private def toBzlmodSourcesDir(
      groupId: String,
      artifactId: String,
      version: String,
  ): String = {
    val sanitize = (s: String) =>
      s.replace('.', '_').replace('-', '_').replace(':', '_')
    s"${sanitize(groupId)}_${sanitize(artifactId)}_jar_sources_${sanitize(version)}"
  }
}
