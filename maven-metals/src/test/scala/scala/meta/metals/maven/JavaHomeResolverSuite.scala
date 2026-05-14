package scala.meta.metals.maven

import java.nio.file.Files
import java.nio.file.Path
import java.{util => ju}

import scala.jdk.CollectionConverters._

import org.apache.maven.execution.MavenSession
import org.apache.maven.model.Build
import org.apache.maven.model.Plugin
import org.apache.maven.model.PluginExecution
import org.apache.maven.plugin.logging.SystemStreamLog
import org.apache.maven.project.MavenProject
import org.apache.maven.toolchain.Toolchain
import org.apache.maven.toolchain.ToolchainManager
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

    assert(selected == Some(temurin25.toString))
  }

  test("returns None when no candidate JDK satisfies project requirements") {
    val workspace = Files.createTempDirectory("jdk-missing")
    val azul21 = fakeJdk(workspace, "azul-21", "21.0.8", "Azul Systems, Inc.")

    val selected = JavaHomeResolver.selectJavaHome(
      project = projectWithJavaRequirement(
        version = "25.0.1",
        vendors = List("Eclipse Adoptium"),
      ),
      candidates = List(azul21.toString),
    )

    assert(selected == None)
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

    val home =
      JavaHomeResolver.fromForkExecutable(project, isTest = false)
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

    val home =
      JavaHomeResolver.fromForkExecutable(project, isTest = false)
    assert(home == Some(jdk21.toString))
  }

  test("resolveFromForkExecutable resolves bare command name from PATH") {
    val workspace = Files.createTempDirectory("jdk-fork-bare")
    val jdk21 = Files.createDirectories(workspace.resolve("jdk-21"))
    val bin = Files.createDirectories(jdk21.resolve("bin"))
    val javac21 = bin.resolve("javac21")
    Files.createFile(javac21)
    javac21.toFile.setExecutable(true)

    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(
      compilerPlugin(
        node(
          "configuration",
          node("fork", "true"),
          node("executable", "javac21"),
        )
      )
    )

    val home = JavaHomeResolver.fromForkExecutable(
      project,
      isTest = false,
      pathDirs = Seq(bin.toString),
    )
    assert(home == Some(jdk21.toString))
  }

  test("resolveFromForkExecutable returns None when bare command not on PATH") {
    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(
      compilerPlugin(
        node(
          "configuration",
          node("fork", "true"),
          node("executable", "javac-nonexistent"),
        )
      )
    )

    assert(
      JavaHomeResolver.fromForkExecutable(
        project,
        isTest = false,
        pathDirs = Seq.empty,
      ) == None
    )
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

    assert(
      JavaHomeResolver.fromForkExecutable(
        project,
        isTest = false,
      ) == None
    )
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
      JavaHomeResolver.fromForkExecutable(project, isTest = false) ==
        Some(mainJdk.toString)
    )
    assert(
      JavaHomeResolver.fromForkExecutable(project, isTest = true) ==
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

    assert(selected == Some(temurin.toString))
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

  test("javacOptionsRequirements handles --release= and -source= forms") {
    assert(
      JavaHomeResolver.javacOptionsRequirements(List("--release=21")) ==
        Some(Map("version" -> "21"))
    )
    assert(
      JavaHomeResolver.javacOptionsRequirements(List("-source=17")) ==
        Some(Map("version" -> "17"))
    )
    // space form takes priority over = form when both present
    assert(
      JavaHomeResolver.javacOptionsRequirements(
        List("--release", "21", "--release=17")
      ) ==
        Some(Map("version" -> "21"))
    )
  }

  test("resolve falls back to java.home when no toolchain available") {
    val project = new MavenProject()
    project.setBuild(new Build())
    val result = JavaHomeResolver.resolve(Nil, project, false, nullMojo)
    assert(result == sys.props.get("java.home").filter(_.nonEmpty))
  }

  test("resolve returns java home from active maven session toolchain") {
    val jdkHome = "/fake/temurin-21"
    val result = JavaHomeResolver.resolve(
      Nil,
      new MavenProject(),
      false,
      mojoWithActiveToolchain(jdkHome),
    )
    assert(result == Some(jdkHome))
  }

  test("resolve uses jdkToolchain config from compiler plugin") {
    val jdkHome = "/fake/jdk17"
    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(
      compilerPlugin(
        node(
          "configuration",
          node("jdkToolchain", node("version", "17")),
        )
      )
    )
    val result = JavaHomeResolver.resolve(
      Nil,
      project,
      false,
      mojoWithToolchains(Map(Map("version" -> "17") -> jdkHome)),
    )
    assert(result == Some(jdkHome))
  }

  test(
    "resolve maps full property version to major for toolchain lookup"
  ) {
    // regression: air.java.version=25.0.1 must match a toolchain with version=25
    val workspace = Files.createTempDirectory("jdk-property-version")
    val jdk25 = fakeJdk(workspace, "temurin-25", "25.0.1", "Eclipse Adoptium")
    val project = new MavenProject()
    project.getProperties.setProperty("air.java.version", "25.0.1")
    project.setBuild(new Build())
    val result = JavaHomeResolver.resolve(
      Nil,
      project,
      false,
      mojoWithToolchains(Map(Map("version" -> "25") -> jdk25.toString)),
    )
    assert(result == Some(jdk25.toString))
  }

  test("resolve uses javacOptions release flag as toolchain hint") {
    val jdkHome = "/fake/jdk11"
    val project = new MavenProject()
    project.setBuild(new Build())
    val result = JavaHomeResolver.resolve(
      List("--release", "11"),
      project,
      false,
      mojoWithToolchains(Map(Map("version" -> "11") -> jdkHome)),
    )
    assert(result == Some(jdkHome))
  }

  test("resolve prioritizes fork/executable over active session toolchain") {
    val workspace = Files.createTempDirectory("jdk-priority")
    val forkJdk = Files.createDirectories(workspace.resolve("fork-jdk"))
    val bin = Files.createDirectories(forkJdk.resolve("bin"))
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

    val result = JavaHomeResolver.resolve(
      Nil,
      project,
      false,
      mojoWithActiveToolchain("/fake/session-jdk"),
    )
    assert(result == Some(forkJdk.toString))
  }

  test(
    "resolve matches toolchain by version even when enforcer vendor differs from toolchain vendor"
  ) {
    // Eclipse Adoptium (enforcer) and temurin (toolchains.xml) are the same vendor —
    // vendor is stripped before querying ToolchainManager; selectJavaHome handles aliases.
    val workspace = Files.createTempDirectory("jdk-vendor-mismatch")
    val jdk25 = fakeJdk(workspace, "temurin-25", "25.0.1", "Eclipse Adoptium")

    val project = new MavenProject()
    project.getProperties.setProperty("air.java.version", "25.0.1")
    project.setBuild(new Build())
    project.getBuild.addPlugin(enforcerPlugin(List("Eclipse Adoptium")))

    val result = JavaHomeResolver.resolve(
      Nil,
      project,
      false,
      // toolchain keyed with "temurin" — would fail with exact vendor match
      mojoWithToolchains(Map(Map("version" -> "25") -> jdk25.toString)),
    )
    assert(result == Some(jdk25.toString))
  }

  test("resolve uses toolchains plugin config over property requirements") {
    val tcJdkHome = "/fake/jdk17-from-toolchains-plugin"
    val propJdkHome = "/fake/jdk25-from-property"

    val tcPlugin = new Plugin()
    tcPlugin.setGroupId("org.apache.maven.plugins")
    tcPlugin.setArtifactId("maven-toolchains-plugin")
    tcPlugin.setConfiguration(
      node(
        "configuration",
        node("toolchains", node("jdk", node("version", "17"))),
      )
    )
    val project = new MavenProject()
    project.getProperties.setProperty("air.java.version", "25.0.1")
    project.setBuild(new Build())
    project.getBuild.addPlugin(tcPlugin)

    val result = JavaHomeResolver.resolve(
      Nil,
      project,
      false,
      mojoWithToolchains(
        Map(
          Map("version" -> "17") -> tcJdkHome,
          Map("version" -> "25") -> propJdkHome,
        )
      ),
    )
    assert(result == Some(tcJdkHome))
  }

  private def nullMojo: MbtMojo = mojoWithToolchains(Map.empty)

  private def mojoWithActiveToolchain(jdkHome: String): MbtMojo =
    new MbtMojo {
      override def getLog = new SystemStreamLog()
      override def getReactorProjects = ju.Collections.emptyList()
      override def getOutputFile = null
      override def isDownloadSources = false
      override def getRepoSystem = null
      override def getRepositorySession = null
      override def getLocalRepositoryBasedir = null
      override def getSession: MavenSession = null
      override def getToolchainManager: ToolchainManager =
        new ToolchainManager {
          override def getToolchainFromBuildContext(
              t: String,
              s: MavenSession,
          ): Toolchain = mkToolchain(jdkHome)
          override def getToolchains(
              s: MavenSession,
              t: String,
              reqs: ju.Map[String, String],
          ): ju.List[Toolchain] = ju.Collections.emptyList()
        }
    }

  private def mojoWithToolchains(
      byReqs: Map[Map[String, String], String]
  ): MbtMojo =
    new MbtMojo {
      override def getLog = new SystemStreamLog()
      override def getReactorProjects = ju.Collections.emptyList()
      override def getOutputFile = null
      override def isDownloadSources = false
      override def getRepoSystem = null
      override def getRepositorySession = null
      override def getLocalRepositoryBasedir = null
      override def getSession: MavenSession = null
      override def getToolchainManager: ToolchainManager =
        new ToolchainManager {
          override def getToolchainFromBuildContext(
              t: String,
              s: MavenSession,
          ): Toolchain = null
          override def getToolchains(
              s: MavenSession,
              t: String,
              reqs: ju.Map[String, String],
          ): ju.List[Toolchain] =
            byReqs.get(reqs.asScala.toMap) match {
              case Some(home) => ju.List.of(mkToolchain(home))
              case None => ju.Collections.emptyList()
            }
        }
    }

  private def mkToolchain(jdkHome: String): Toolchain = new Toolchain {
    override def getType = "jdk"
    override def findTool(name: String): String =
      if (name == "java") s"$jdkHome/bin/java" else null
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
