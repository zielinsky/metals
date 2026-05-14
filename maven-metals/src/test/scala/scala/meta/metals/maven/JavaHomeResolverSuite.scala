package scala.meta.metals.maven

import java.nio.file.Files
import java.nio.file.Path

import scala.jdk.CollectionConverters._

import org.apache.maven.model.Build
import org.apache.maven.model.Plugin
import org.apache.maven.model.PluginExecution
import org.apache.maven.project.MavenProject
import org.codehaus.plexus.util.xml.Xpp3Dom
import org.scalatest.funsuite.AnyFunSuite

class JavaHomeResolverSuite extends AnyFunSuite {

  test("selects a JDK satisfying version and enforcer vendor requirements") {
    val workspace = Files.createTempDirectory("jdk-selection")
    val azul21 = fakeJdk(workspace, "azul-21", "21.0.8", "Azul Systems, Inc.")
    val temurin25 =
      fakeJdk(workspace, "temurin-25", "25.0.2", "Eclipse Adoptium")
    val oracle25 =
      fakeJdk(workspace, "oracle-25", "25.0.1", "Oracle Corporation")

    val selected = JavaHomeResolver.selectJavaHome(
      project = projectWithJavaRequirement(
        version = "25.0.1",
        vendors = List("Eclipse Adoptium", "Oracle Corporation"),
      ),
      candidates = List(azul21, temurin25, oracle25).map(_.toString),
    )

    assert(selected == temurin25.toString)
  }

  test("returns null when no candidate JDK satisfies project requirements") {
    val workspace = Files.createTempDirectory("jdk-missing")
    val azul21 = fakeJdk(workspace, "azul-21", "21.0.8", "Azul Systems, Inc.")

    val selected = JavaHomeResolver.selectJavaHome(
      project = projectWithJavaRequirement(
        version = "25.0.1",
        vendors = List("Eclipse Adoptium"),
      ),
      candidates = List(azul21.toString),
    )

    assert(selected == null)
  }

  test("resolves javaHome from fork/executable in compiler plugin") {
    val workspace = Files.createTempDirectory("jdk-fork")
    val jdk21 = Files.createDirectories(workspace.resolve("jdk-21"))
    val bin = Files.createDirectories(jdk21.resolve("bin"))
    Files.createFile(bin.resolve("javac"))

    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(
      compilerPlugin(
        node(
          "configuration",
          node("fork", "true"),
          node("executable", bin.resolve("javac").toString),
        )
      )
    )

    val home = JavaHomeResolver.resolveFromForkExecutable(project)
    assert(home == Some(jdk21.toString))
  }

  test(
    "resolveFromForkExecutable interpolates property references in executable path"
  ) {
    val workspace = Files.createTempDirectory("jdk-fork-interpolate")
    val jdk21 = Files.createDirectories(workspace.resolve("jdk-21"))
    val bin = Files.createDirectories(jdk21.resolve("bin"))
    Files.createFile(bin.resolve("javac"))

    val project = new MavenProject()
    project.setBuild(new Build())
    project.getProperties.setProperty("my.jdk.home", jdk21.toString)
    project.getBuild.addPlugin(
      compilerPlugin(
        node(
          "configuration",
          node("fork", "true"),
          node("executable", "${my.jdk.home}/bin/javac"),
        )
      )
    )

    val home = JavaHomeResolver.resolveFromForkExecutable(project)
    assert(home == Some(jdk21.toString))
  }

  test("resolveFromForkExecutable ignores non-absolute executable") {
    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(
      compilerPlugin(
        node(
          "configuration",
          node("fork", "true"),
          node("executable", "javac"),
        )
      )
    )

    assert(JavaHomeResolver.resolveFromForkExecutable(project) == None)
  }

  test("resolveFromForkExecutable returns None when fork is false") {
    val workspace = Files.createTempDirectory("jdk-nofork")
    val bin = Files.createDirectories(workspace.resolve("jdk-21/bin"))
    Files.createFile(bin.resolve("javac"))

    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(
      compilerPlugin(
        node(
          "configuration",
          node("fork", "false"),
          node("executable", bin.resolve("javac").toString),
        )
      )
    )

    assert(JavaHomeResolver.resolveFromForkExecutable(project) == None)
  }

  test("resolves fork/executable separately for main and test executions") {
    val workspace = Files.createTempDirectory("jdk-fork-per-goal")
    val mainJdk = Files.createDirectories(workspace.resolve("jdk-main"))
    val testJdk = Files.createDirectories(workspace.resolve("jdk-test"))
    val mainBin = Files.createDirectories(mainJdk.resolve("bin"))
    val testBin = Files.createDirectories(testJdk.resolve("bin"))
    Files.createFile(mainBin.resolve("javac"))
    Files.createFile(testBin.resolve("javac"))

    val plugin = compilerPlugin(node("configuration"))
    plugin.addExecution(
      execution(
        id = "default-compile",
        goal = "compile",
        node(
          "configuration",
          node("fork", "true"),
          node("executable", mainBin.resolve("javac").toString),
        ),
      )
    )
    plugin.addExecution(
      execution(
        id = "default-testCompile",
        goal = "testCompile",
        node(
          "configuration",
          node("fork", "true"),
          node("executable", testBin.resolve("javac").toString),
        ),
      )
    )

    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(plugin)

    assert(
      JavaHomeResolver.resolveFromForkExecutable(project, isTest = false) ==
        Some(mainJdk.toString)
    )
    assert(
      JavaHomeResolver.resolveFromForkExecutable(project, isTest = true) ==
        Some(testJdk.toString)
    )
  }

  test("matches common JDK vendor aliases") {
    val workspace = Files.createTempDirectory("jdk-vendor-alias")
    val temurin = fakeJdk(workspace, "temurin-21", "21.0.8", "Eclipse Adoptium")

    val selected = JavaHomeResolver.selectJavaHome(
      project = projectWithJavaRequirement(
        version = "21",
        vendors = List("temurin"),
      ),
      candidates = List(temurin.toString),
    )

    assert(selected == temurin.toString)
  }

  test("javacOptionsRequirements extracts version from --release and -source") {
    assert(
      JavaHomeResolver.javacOptionsRequirements(List("--release", "21")) ==
        Some(Map("version" -> "21"))
    )
    assert(
      JavaHomeResolver.javacOptionsRequirements(
        List("-source", "17", "-target", "17")
      ) ==
        Some(Map("version" -> "17"))
    )
    assert(JavaHomeResolver.javacOptionsRequirements(Nil) == None)
    assert(
      JavaHomeResolver.javacOptionsRequirements(
        List("-encoding", "UTF-8")
      ) == None
    )
  }

  private def fakeJdk(
      workspace: Path,
      name: String,
      version: String,
      implementor: String,
  ): Path = {
    val home = workspace.resolve(name)
    Files.createDirectories(home)
    Files.writeString(
      home.resolve("release"),
      s"""|JAVA_VERSION="$version"
          |IMPLEMENTOR="$implementor"
          |""".stripMargin,
    )
    home
  }

  private def projectWithJavaRequirement(
      version: String,
      vendors: List[String],
  ): MavenProject = {
    val project = new MavenProject()
    project.getProperties.setProperty("air.java.version", version)
    val build = new Build()
    build.addPlugin(enforcerPlugin(vendors))
    project.setBuild(build)
    project
  }

  private def enforcerPlugin(vendors: List[String]): Plugin = {
    val plugin = new Plugin()
    plugin.setGroupId("org.apache.maven.plugins")
    plugin.setArtifactId("maven-enforcer-plugin")
    plugin.setConfiguration(
      node(
        "configuration",
        node(
          "rules",
          node(
            "requireJavaVendor",
            node("includes", vendors.map(vendor => node("include", vendor)): _*),
          ),
        ),
      )
    )
    plugin
  }

  private def compilerPlugin(configuration: Xpp3Dom): Plugin = {
    val plugin = new Plugin()
    plugin.setGroupId("org.apache.maven.plugins")
    plugin.setArtifactId("maven-compiler-plugin")
    plugin.setConfiguration(configuration)
    plugin
  }

  private def execution(
      id: String,
      goal: String,
      configuration: Xpp3Dom,
  ): PluginExecution = {
    val exec = new PluginExecution()
    exec.setId(id)
    exec.addGoal(goal)
    exec.setConfiguration(configuration)
    exec
  }

  private def node(name: String, value: String): Xpp3Dom = {
    val dom = new Xpp3Dom(name)
    dom.setValue(value)
    dom
  }

  private def node(name: String, children: Xpp3Dom*): Xpp3Dom = {
    val dom = new Xpp3Dom(name)
    children.foreach(dom.addChild)
    dom
  }
}
