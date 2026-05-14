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
