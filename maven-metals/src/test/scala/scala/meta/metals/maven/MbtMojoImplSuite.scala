package scala.meta.metals.maven

import java.io.File
import java.nio.file.Files
import java.util.Collections

import scala.jdk.CollectionConverters._

import com.google.gson.Gson
import org.apache.maven.artifact.Artifact
import org.apache.maven.artifact.DefaultArtifact
import org.apache.maven.artifact.handler.DefaultArtifactHandler
import org.apache.maven.execution.MavenSession
import org.apache.maven.model.Build
import org.apache.maven.model.Dependency
import org.apache.maven.plugin.logging.Log
import org.apache.maven.plugin.logging.SystemStreamLog
import org.apache.maven.project.MavenProject
import org.apache.maven.toolchain.ToolchainManager
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.RepositorySystemSession
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

  private class TestMojo(
      projects: List[MavenProject],
      output: File,
      localRepo: File,
  ) extends MbtMojo {
    override def getLog: Log = new SystemStreamLog()
    override def getReactorProjects: java.util.List[MavenProject] =
      projects.asJava
    override def getOutputFile: File = output
    override def isDownloadSources: Boolean = false
    override def getRepoSystem: RepositorySystem = null
    override def getToolchainManager: ToolchainManager = null
    override def getRepositorySession: RepositorySystemSession = null
    override def getLocalRepositoryBasedir: File = localRepo
    override def getSession: MavenSession = null
  }
}
