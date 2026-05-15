package scala.meta.metals.maven

import java.io.File
import java.lang.reflect.Proxy
import java.nio.file.Files
import java.util.Collections

import scala.jdk.CollectionConverters._

import com.google.gson.Gson
import org.apache.maven.artifact.Artifact
import org.apache.maven.artifact.DefaultArtifact
import org.apache.maven.artifact.handler.DefaultArtifactHandler
import org.apache.maven.execution.DefaultMavenExecutionRequest
import org.apache.maven.execution.MavenSession
import org.apache.maven.model.Build
import org.apache.maven.model.Dependency
import org.apache.maven.plugin.logging.Log
import org.apache.maven.plugin.logging.SystemStreamLog
import org.apache.maven.project.MavenProject
import org.apache.maven.toolchain.ToolchainManager
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.RepositorySystemSession
import org.eclipse.aether.resolution.ArtifactRequest
import org.eclipse.aether.resolution.ArtifactResult
import org.scalatest.funsuite.AnyFunSuite

class MbtMojoImplSuite extends AnyFunSuite {

  test("exports main and test namespaces with reactor target dependencies") {
    val workspace = Files.createTempDirectory("mbt-export")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    val core = project("core", workspace.resolve("core").toFile)
    val app = project("app", workspace.resolve("app").toFile)
    app.setArtifacts(Set(reactorArtifact(core, Artifact.SCOPE_COMPILE)).asJava)
    app.setDependencies(
      List(testJarDependency(core)).asJava
    )

    MbtMojoImpl.run(
      new TestMojo(List(core, app), output.toFile, localRepo.toFile)
    )

    val build =
      new Gson().fromJson(Files.readString(output), classOf[MbtBuildJson])
    val namespaces = build.namespaces.asScala.toMap

    assert(
      namespaces.keySet == Set(
        "com.example:core:1.0.0",
        "com.example:core:1.0.0:test",
        "com.example:app:1.0.0",
        "com.example:app:1.0.0:test",
      )
    )
    assert(
      namespaces("com.example:app:1.0.0").dependsOn.asScala.toList ==
        List("com.example:core:1.0.0")
    )
    assert(
      namespaces("com.example:app:1.0.0:test").dependsOn.asScala.toList ==
        List(
          "com.example:app:1.0.0",
          "com.example:core:1.0.0",
          "com.example:core:1.0.0:test",
        )
    )
  }

  test("single module produces main and test namespaces") {
    val workspace = Files.createTempDirectory("mbt-single")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    val app = project("app", workspace.resolve("app").toFile)

    MbtMojoImpl.run(new TestMojo(List(app), output.toFile, localRepo.toFile))

    val build =
      new Gson().fromJson(Files.readString(output), classOf[MbtBuildJson])
    val namespaces = build.namespaces.asScala.toMap

    assert(
      namespaces.keySet == Set(
        "com.example:app:1.0.0",
        "com.example:app:1.0.0:test",
      )
    )
    assert(namespaces("com.example:app:1.0.0").dependsOn.asScala.isEmpty)
    assert(
      namespaces("com.example:app:1.0.0:test").dependsOn.asScala.toList ==
        List("com.example:app:1.0.0")
    )
  }

  test("pom-packaging modules are excluded from export") {
    val workspace = Files.createTempDirectory("mbt-pom-filter")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    val parent = new MavenProject()
    parent.setGroupId("com.example")
    parent.setArtifactId("parent")
    parent.setVersion("1.0.0")
    parent.setPackaging("pom")

    val app = project("app", workspace.resolve("app").toFile)

    MbtMojoImpl.run(
      new TestMojo(List(parent, app), output.toFile, localRepo.toFile)
    )

    val build =
      new Gson().fromJson(Files.readString(output), classOf[MbtBuildJson])
    val namespaces = build.namespaces.asScala

    assert(!namespaces.contains("com.example:parent:1.0.0"))
    assert(namespaces.contains("com.example:app:1.0.0"))
  }

  test(
    "resolved SNAPSHOT dependency is found in local repo despite timestamp version"
  ) {
    val workspace = Files.createTempDirectory("mbt-snapshot-dep")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    val baseVersionDir = localRepo.resolve("org/example/core/1.0.0-SNAPSHOT")
    Files.createDirectories(baseVersionDir)
    val timestampedJar =
      baseVersionDir.resolve("core-1.0.0-20240101.120000-1.jar")
    Files.createFile(timestampedJar)

    val app = project("app", workspace.resolve("app").toFile)
    val snapshotArtifact = new DefaultArtifact(
      "org.example",
      "core",
      "1.0.0-20240101.120000-1",
      Artifact.SCOPE_COMPILE,
      "jar",
      null,
      new DefaultArtifactHandler("jar"),
    )
    app.setArtifacts(Set[Artifact](snapshotArtifact).asJava)

    MbtMojoImpl.run(new TestMojo(List(app), output.toFile, localRepo.toFile))

    val build =
      new Gson().fromJson(Files.readString(output), classOf[MbtBuildJson])
    val depIds = build.dependencyModules.asScala.map(_.id).toSet
    assert(
      depIds.contains("org.example:core:1.0.0-20240101.120000-1"),
      s"Expected snapshot dep in output, got: $depIds",
    )
  }

  test("external jar dependency appears in dependency modules") {
    val workspace = Files.createTempDirectory("mbt-ext-dep")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    val jarPath =
      localRepo.resolve("com/example/utils/2.0.0/utils-2.0.0.jar")
    Files.createDirectories(jarPath.getParent)
    Files.createFile(jarPath)

    val app = project("app", workspace.resolve("app").toFile)
    val extArtifact = new DefaultArtifact(
      "com.example",
      "utils",
      "2.0.0",
      Artifact.SCOPE_COMPILE,
      "jar",
      null,
      new DefaultArtifactHandler("jar"),
    )
    extArtifact.setFile(jarPath.toFile)
    app.setArtifacts(Set[Artifact](extArtifact).asJava)

    MbtMojoImpl.run(new TestMojo(List(app), output.toFile, localRepo.toFile))

    val build =
      new Gson().fromJson(Files.readString(output), classOf[MbtBuildJson])
    val depIds =
      build.dependencyModules.asScala.map(_.id).toSet
    val mainDeps =
      build.namespaces
        .asScala("com.example:app:1.0.0")
        .dependencyModules
        .asScala
        .toList

    assert(depIds.contains("com.example:utils:2.0.0"))
    assert(mainDeps.contains("com.example:utils:2.0.0"))
  }

  test(
    "declared dependencies are collected when Maven artifacts are unavailable"
  ) {
    val workspace = Files.createTempDirectory("mbt-declared-deps")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    createLocalJar(localRepo.toFile, "org.libs", "direct", "1.0.0")
    createLocalSourcesJar(localRepo.toFile, "org.libs", "direct", "1.0.0")
    createLocalJar(localRepo.toFile, "org.libs", "default-scope", "1.0.0")
    createLocalJar(localRepo.toFile, "org.libs", "provided", "1.0.0")
    createLocalJar(localRepo.toFile, "org.libs", "system", "1.0.0")
    createLocalJar(localRepo.toFile, "org.libs", "runtime", "1.0.0")
    createLocalJar(localRepo.toFile, "org.libs", "test-only", "1.0.0")
    createLocalJar(
      localRepo.toFile,
      "org.libs",
      "classified",
      "1.0.0",
      Some("shaded"),
    )
    createLocalJar(
      localRepo.toFile,
      "org.libs",
      "fixtures",
      "1.0.0",
      Some("tests"),
    )
    createLocalJar(localRepo.toFile, "org.libs", "managed", "3.0.0")

    val app = project("app", workspace.resolve("app").toFile)
    app.setDependencies(
      List(
        dependency("direct", "1.0.0", Artifact.SCOPE_COMPILE),
        dependency("default-scope", "1.0.0", null),
        dependency("provided", "1.0.0", Artifact.SCOPE_PROVIDED),
        dependency("system", "1.0.0", Artifact.SCOPE_SYSTEM),
        dependency("runtime", "1.0.0", Artifact.SCOPE_RUNTIME),
        dependency("test-only", "1.0.0", Artifact.SCOPE_TEST),
        dependency(
          "classified",
          "1.0.0",
          Artifact.SCOPE_COMPILE,
          classifier = Some("shaded"),
        ),
        dependency(
          "fixtures",
          "1.0.0",
          Artifact.SCOPE_TEST,
          tpe = "test-jar",
          classifier = Some("tests"),
        ),
        dependency("managed", null, Artifact.SCOPE_COMPILE),
        dependency("pom-only", "1.0.0", Artifact.SCOPE_COMPILE, tpe = "pom"),
      ).asJava
    )
    app.setManagedVersionMap(
      Map[String, Artifact](
        "org.libs:managed:jar" ->
          new DefaultArtifact(
            "org.libs",
            "managed",
            "3.0.0",
            Artifact.SCOPE_COMPILE,
            "jar",
            null,
            new DefaultArtifactHandler("jar"),
          )
      ).asJava
    )

    MbtMojoImpl.run(new TestMojo(List(app), output.toFile, localRepo.toFile))

    val build = readBuild(output.toFile)
    val modulesById = build.dependencyModules.asScala.map(m => m.id -> m).toMap
    val mainDeps =
      build.namespaces
        .asScala("com.example:app:1.0.0")
        .dependencyModules
        .asScala
        .toSet
    val testDeps =
      build.namespaces
        .asScala("com.example:app:1.0.0:test")
        .dependencyModules
        .asScala
        .toSet

    assert(
      mainDeps == Set(
        "org.libs:direct:1.0.0", "org.libs:default-scope:1.0.0",
        "org.libs:provided:1.0.0", "org.libs:system:1.0.0",
        "org.libs:classified:shaded:1.0.0", "org.libs:managed:3.0.0",
      )
    )
    assert(
      testDeps == mainDeps ++ Set(
        "org.libs:runtime:1.0.0",
        "org.libs:test-only:1.0.0",
        "org.libs:fixtures:tests:1.0.0",
      )
    )
    assert(!modulesById.contains("org.libs:pom-only:1.0.0"))
    assert(modulesById("org.libs:direct:1.0.0").sources != null)
  }

  test("downloadSources attaches sources resolved through repository system") {
    val workspace = Files.createTempDirectory("mbt-download-sources")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    createLocalJar(localRepo.toFile, "com.example", "utils", "2.0.0")
    val sources = Files
      .createDirectories(workspace.resolve("remote"))
      .resolve("utils-2.0.0-sources.jar")
    Files.createFile(sources)

    val app = project("app", workspace.resolve("app").toFile)
    val extArtifact = new DefaultArtifact(
      "com.example",
      "utils",
      "2.0.0",
      Artifact.SCOPE_COMPILE,
      "jar",
      null,
      new DefaultArtifactHandler("jar"),
    )
    app.setArtifacts(Set[Artifact](extArtifact).asJava)

    val session = new MavenSession(
      null,
      null.asInstanceOf[RepositorySystemSession],
      new DefaultMavenExecutionRequest(),
      null,
    )
    session.setCurrentProject(app)
    MbtMojoImpl.run(
      new TestMojo(
        List(app),
        output.toFile,
        localRepo.toFile,
        downloadSources = true,
        repoSystem = repoSystemResolving(
          Map("com.example:utils:2.0.0" -> sources.toFile)
        ),
        session = session,
      )
    )

    val build = readBuild(output.toFile)
    val module = build.dependencyModules.asScala
      .find(_.id == "com.example:utils:2.0.0")
      .get

    assert(module.sources == sources.toFile.toURI.toString)
  }

  test("output file is written even when parent directories do not exist") {
    val workspace = Files.createTempDirectory("mbt-nested-output")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("a/b/c/mbt.json")

    val app = project("app", workspace.resolve("app").toFile)
    MbtMojoImpl.run(new TestMojo(List(app), output.toFile, localRepo.toFile))

    assert(
      Files.exists(output),
      s"expected output file to be created at $output",
    )
  }

  test(
    "duplicate external dependency is emitted only once in dependency modules"
  ) {
    val workspace = Files.createTempDirectory("mbt-dedup-deps")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    createLocalJar(localRepo.toFile, "com.example", "shared", "1.0.0")

    val app = project("app", workspace.resolve("app").toFile)
    val sharedArtifact = new DefaultArtifact(
      "com.example",
      "shared",
      "1.0.0",
      Artifact.SCOPE_COMPILE,
      "jar",
      null,
      new DefaultArtifactHandler("jar"),
    )
    sharedArtifact.setFile(
      localRepo
        .resolve("com/example/shared/1.0.0/shared-1.0.0.jar")
        .toFile
    )
    app.setArtifacts(Set[Artifact](sharedArtifact, sharedArtifact).asJava)

    MbtMojoImpl.run(new TestMojo(List(app), output.toFile, localRepo.toFile))

    val build = readBuild(output.toFile)
    val depIds = build.dependencyModules.asScala.map(_.id).toList

    assert(
      depIds.count(_ == "com.example:shared:1.0.0") == 1,
      s"expected exactly one entry for shared, got: $depIds",
    )
  }

  test("artifact with missing file is silently skipped in dependency modules") {
    val workspace = Files.createTempDirectory("mbt-missing-jar")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    val app = project("app", workspace.resolve("app").toFile)
    val missingArtifact = new DefaultArtifact(
      "com.example",
      "ghost",
      "1.0.0",
      Artifact.SCOPE_COMPILE,
      "jar",
      null,
      new DefaultArtifactHandler("jar"),
    )
    missingArtifact.setFile(workspace.resolve("nonexistent.jar").toFile)
    app.setArtifacts(Set[Artifact](missingArtifact).asJava)

    val session = new MavenSession(
      null,
      null.asInstanceOf[RepositorySystemSession],
      new DefaultMavenExecutionRequest(),
      null,
    )
    session.setCurrentProject(app)
    MbtMojoImpl.run(
      new TestMojo(
        List(app),
        output.toFile,
        localRepo.toFile,
        session = session,
      )
    )

    val build = readBuild(output.toFile)
    assert(
      !build.dependencyModules.asScala
        .map(_.id)
        .toSet
        .contains("com.example:ghost:1.0.0"),
      "missing artifact must not appear in dependencyModules",
    )
    assert(
      build.namespaces
        .asScala("com.example:app:1.0.0")
        .dependencyModules
        .asScala
        .isEmpty,
      "namespace must not reference missing artifact",
    )
  }

  test(
    "artifact with existing non-jar file is not registered and not referenced in namespace"
  ) {
    val workspace = Files.createTempDirectory("mbt-non-jar")
    val localRepo = Files.createDirectories(workspace.resolve("m2"))
    val output = workspace.resolve("mbt.json")

    val pomFile = workspace.resolve("artifact.pom").toFile
    Files.writeString(pomFile.toPath, "<project/>")

    val app = project("app", workspace.resolve("app").toFile)
    val pomArtifact = new DefaultArtifact(
      "com.example",
      "pom-dep",
      "2.0.0",
      Artifact.SCOPE_COMPILE,
      "pom",
      null,
      new DefaultArtifactHandler("pom"),
    )
    pomArtifact.setFile(pomFile)
    app.setArtifacts(Set[Artifact](pomArtifact).asJava)

    val session = new MavenSession(
      null,
      null.asInstanceOf[RepositorySystemSession],
      new DefaultMavenExecutionRequest(),
      null,
    )
    session.setCurrentProject(app)
    MbtMojoImpl.run(
      new TestMojo(
        List(app),
        output.toFile,
        localRepo.toFile,
        session = session,
      )
    )

    val build = readBuild(output.toFile)
    val depModuleIds = build.dependencyModules.asScala.map(_.id).toSet
    val nsDepIds =
      build.namespaces
        .asScala("com.example:app:1.0.0")
        .dependencyModules
        .asScala
        .toSet

    assert(
      !depModuleIds.contains("com.example:pom-dep:2.0.0"),
      s"non-JAR artifact must not appear in dependencyModules: $depModuleIds",
    )
    assert(
      nsDepIds.isEmpty,
      s"namespace must not reference non-JAR artifact, got: $nsDepIds",
    )
    assert(
      nsDepIds.subsetOf(depModuleIds),
      s"dangling namespace references: ${nsDepIds -- depModuleIds}",
    )
  }

  private def project(artifactId: String, basedir: File): MavenProject = {
    Files.createDirectories(basedir.toPath.resolve("src/main/java"))
    Files.createDirectories(basedir.toPath.resolve("src/test/java"))
    val project = new MavenProject()
    project.setGroupId("com.example")
    project.setArtifactId(artifactId)
    project.setVersion("1.0.0")
    project.setPackaging("jar")
    project.setFile(basedir.toPath.resolve("pom.xml").toFile)
    val build = new Build()
    build.setDirectory(basedir.toPath.resolve("target").toString)
    project.setBuild(build)
    project.addCompileSourceRoot(
      basedir.toPath.resolve("src/main/java").toString
    )
    project.addTestCompileSourceRoot(
      basedir.toPath.resolve("src/test/java").toString
    )
    project.setArtifacts(Collections.emptySet[Artifact]())
    project
  }

  private def reactorArtifact(
      project: MavenProject,
      scope: String,
  ): Artifact =
    new DefaultArtifact(
      project.getGroupId,
      project.getArtifactId,
      project.getVersion,
      scope,
      "jar",
      null,
      new DefaultArtifactHandler("jar"),
    )

  private def testJarDependency(project: MavenProject): Dependency = {
    val dependency = new Dependency()
    dependency.setGroupId(project.getGroupId)
    dependency.setArtifactId(project.getArtifactId)
    dependency.setVersion(project.getVersion)
    dependency.setType("test-jar")
    dependency.setClassifier("tests")
    dependency.setScope(Artifact.SCOPE_TEST)
    dependency
  }

  private def dependency(
      artifactId: String,
      version: String,
      scope: String,
      tpe: String = "jar",
      classifier: Option[String] = None,
  ): Dependency = {
    val dependency = new Dependency()
    dependency.setGroupId("org.libs")
    dependency.setArtifactId(artifactId)
    dependency.setVersion(version)
    dependency.setType(tpe)
    classifier.foreach(dependency.setClassifier)
    dependency.setScope(scope)
    dependency
  }

  private def createLocalJar(
      localRepo: File,
      groupId: String,
      artifactId: String,
      version: String,
      classifier: Option[String] = None,
  ): File = {
    val classifierSuffix = classifier.map("-" + _).getOrElse("")
    val jar = new File(
      localRepo,
      s"${groupId.replace('.', '/')}/$artifactId/$version/$artifactId-$version$classifierSuffix.jar",
    )
    Files.createDirectories(jar.toPath.getParent)
    Files.createFile(jar.toPath)
    jar
  }

  private def createLocalSourcesJar(
      localRepo: File,
      groupId: String,
      artifactId: String,
      version: String,
  ): File = {
    val jar = new File(
      localRepo,
      s"${groupId.replace('.', '/')}/$artifactId/$version/$artifactId-$version-sources.jar",
    )
    Files.createDirectories(jar.toPath.getParent)
    Files.createFile(jar.toPath)
    jar
  }

  private def readBuild(output: File): MbtBuildJson =
    new Gson().fromJson(Files.readString(output.toPath), classOf[MbtBuildJson])

  private def repoSystemResolving(
      sourcesByCoords: Map[String, File]
  ): RepositorySystem =
    Proxy
      .newProxyInstance(
        classOf[RepositorySystem].getClassLoader,
        Array(classOf[RepositorySystem]),
        (_, method, args) =>
          if (method.getName == "resolveArtifacts") {
            val requests = args(1)
              .asInstanceOf[java.util.Collection[ArtifactRequest]]
              .asScala
            val results = requests.map { req =>
              val art = req.getArtifact
              val coords =
                s"${art.getGroupId}:${art.getArtifactId}:${art.getVersion}"
              val result = new ArtifactResult(req)
              sourcesByCoords.get(coords).foreach { file =>
                result.setArtifact(
                  art.setFile(file)
                )
              }
              result
            }
            new java.util.ArrayList(results.asJavaCollection)
          } else null,
      )
      .asInstanceOf[RepositorySystem]

  private class TestMojo(
      projects: List[MavenProject],
      output: File,
      localRepo: File,
      downloadSources: Boolean = false,
      repoSystem: RepositorySystem = null,
      session: MavenSession = null,
  ) extends MbtMojo {
    override def getLog: Log = new SystemStreamLog()
    override def getReactorProjects: java.util.List[MavenProject] =
      projects.asJava
    override def getOutputFile: File = output
    override def isDownloadSources: Boolean = downloadSources
    override def getRepoSystem: RepositorySystem = repoSystem
    override def getToolchainManager: ToolchainManager = null
    override def getRepositorySession: RepositorySystemSession = null
    override def getLocalRepositoryBasedir: File = localRepo
    override def getSession: MavenSession = session
  }
}
