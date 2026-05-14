package scala.meta.metals.maven

import org.apache.maven.model.Build
import org.apache.maven.model.Plugin
import org.apache.maven.model.PluginExecution
import org.apache.maven.project.MavenProject
import org.codehaus.plexus.util.xml.Xpp3Dom
import org.scalatest.funsuite.AnyFunSuite

class MavenCompilerConfigSuite extends AnyFunSuite {

  test("extracts javac and scalac options from Maven plugin configuration") {
    val project = new MavenProject()
    project.setBuild(new Build())
    project.getProperties.setProperty("project.build.sourceEncoding", "UTF-8")

    project.getBuild.addPlugin(
      plugin(
        "org.apache.maven.plugins",
        "maven-compiler-plugin",
        configuration(
          "release" -> "17",
          "enablePreview" -> "true",
          "parameters" -> "true",
          "compilerArg" -> "-Xlint:all",
        ),
      )
    )
    project.getBuild.addPlugin(
      plugin(
        "net.alchim31.maven",
        "scala-maven-plugin",
        node(
          "configuration",
          node("scalaVersion", "2.13.14"),
          node("args", node("arg", "-deprecation"), node("arg", "-feature")),
          node("addScalacArgs", "-Wunused| -Xlint"),
        ),
      )
    )

    val config = MavenCompilerConfig.extract(project, isTest = false)

    assert(
      config.javacOptions == List(
        "--release", "17", "-encoding", "UTF-8", "--enable-preview",
        "-parameters", "-Xlint:all",
      )
    )
    assert(
      config.scalacOptions == List(
        "-deprecation",
        "-feature",
        "-Wunused",
        "-Xlint",
      )
    )
    assert(config.scalaVersion == "2.13.14")
  }

  test("extracts test-specific javac options using testRelease property") {
    val project = new MavenProject()
    project.setBuild(new Build())
    project.getProperties.setProperty("maven.compiler.release", "17")
    project.getProperties.setProperty("maven.compiler.testRelease", "21")

    val mainConfig = MavenCompilerConfig.extract(project, isTest = false)
    val testConfig = MavenCompilerConfig.extract(project, isTest = true)

    assert(mainConfig.javacOptions.contains("17"))
    assert(testConfig.javacOptions.contains("21"))
  }

  test("extracts legacy compilerArgument and compilerArguments fields") {
    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(
      plugin(
        "org.apache.maven.plugins",
        "maven-compiler-plugin",
        node(
          "configuration",
          node("release", "11"),
          node("compilerArgument", "-Xlint:deprecation"),
          node(
            "compilerArguments",
            node("Xlint", ""),
            node("Afoo", "bar"),
          ),
        ),
      )
    )
    val config = MavenCompilerConfig.extract(project, isTest = false)
    assert(config.javacOptions.contains("-Xlint:deprecation"))
    assert(config.javacOptions.contains("-Xlint"))
    assert(config.javacOptions.contains("-Afoo:bar"))
  }

  test("uses goal-specific compiler executions for main and test") {
    val project = new MavenProject()
    project.setBuild(new Build())

    val compiler = plugin(
      "org.apache.maven.plugins",
      "maven-compiler-plugin",
      configuration("release" -> "8"),
    )
    compiler.addExecution(
      execution(
        id = "default-compile",
        goal = "compile",
        configuration("release" -> "11", "compilerArg" -> "-Xmain"),
      )
    )
    compiler.addExecution(
      execution(
        id = "default-testCompile",
        goal = "testCompile",
        configuration("release" -> "17", "compilerArg" -> "-Xtest"),
      )
    )
    project.getBuild.addPlugin(compiler)

    val mainConfig = MavenCompilerConfig.extract(project, isTest = false)
    val testConfig = MavenCompilerConfig.extract(project, isTest = true)

    assert(mainConfig.javacOptions.contains("11"))
    assert(mainConfig.javacOptions.contains("-Xmain"))
    assert(!mainConfig.javacOptions.contains("-Xtest"))
    assert(testConfig.javacOptions.contains("17"))
    assert(testConfig.javacOptions.contains("-Xtest"))
    assert(!testConfig.javacOptions.contains("-Xmain"))
  }

  test("extracts scalac options per main/test execution") {
    val project = new MavenProject()
    project.setBuild(new Build())

    val scalac = plugin(
      "net.alchim31.maven",
      "scala-maven-plugin",
      node("configuration", node("scalaVersion", "2.13.14")),
    )
    scalac.addExecution(
      execution(
        id = "default-compile",
        goal = "compile",
        node("configuration", node("args", node("arg", "-Xmain"))),
      )
    )
    scalac.addExecution(
      execution(
        id = "default-testCompile",
        goal = "testCompile",
        node("configuration", node("args", node("arg", "-Xtest"))),
      )
    )
    project.getBuild.addPlugin(scalac)

    val mainConfig = MavenCompilerConfig.extract(project, isTest = false)
    val testConfig = MavenCompilerConfig.extract(project, isTest = true)

    assert(mainConfig.scalacOptions.contains("-Xmain"))
    assert(!mainConfig.scalacOptions.contains("-Xtest"))
    assert(testConfig.scalacOptions.contains("-Xtest"))
    assert(!testConfig.scalacOptions.contains("-Xmain"))
    assert(mainConfig.scalaVersion == "2.13.14")
    assert(testConfig.scalaVersion == "2.13.14")
  }

  test("emits -parameters from maven.compiler.parameters property") {
    val project = new MavenProject()
    project.setBuild(new Build())
    project.getProperties.setProperty("maven.compiler.parameters", "true")

    val config = MavenCompilerConfig.extract(project, isTest = false)
    assert(config.javacOptions.contains("-parameters"))
  }

  test("extracts annotation processor flags") {
    val project = new MavenProject()
    project.setBuild(new Build())
    project.getBuild.addPlugin(
      plugin(
        "org.apache.maven.plugins",
        "maven-compiler-plugin",
        node(
          "configuration",
          node("proc", "only"),
          node(
            "annotationProcessors",
            node("annotationProcessor", "com.example.One"),
            node("annotationProcessor", "com.example.Two"),
          ),
        ),
      )
    )

    val config = MavenCompilerConfig.extract(project, isTest = false)
    assert(config.javacOptions.contains("-proc:only"))
    assert(config.javacOptions.contains("-processor"))
    assert(config.javacOptions.contains("com.example.One,com.example.Two"))
  }

  private def plugin(
      groupId: String,
      artifactId: String,
      configuration: Xpp3Dom,
  ): Plugin = {
    val plugin = new Plugin()
    plugin.setGroupId(groupId)
    plugin.setArtifactId(artifactId)
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

  private def configuration(values: (String, String)*): Xpp3Dom =
    node(
      "configuration",
      values.map { case (name, value) => node(name, value) }: _*
    )

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
