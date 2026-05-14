package scala.meta.metals.maven

import java.io.File

import org.apache.maven.model.Build
import org.apache.maven.model.Plugin
import org.apache.maven.model.PluginExecution
import org.apache.maven.model.PluginManagement
import org.apache.maven.project.MavenProject
import org.codehaus.plexus.util.xml.Xpp3Dom
import org.scalatest.funsuite.AnyFunSuite

class MavenSourceRootsSuite extends AnyFunSuite {

  import MavenPluginSupport._

  // ── helpers ──────────────────────────────────────────────────────────────

  private def project(buildDir: String = "/project/target"): MavenProject = {
    val p = new MavenProject()
    val build = new Build()
    build.setDirectory(buildDir)
    build.setSourceDirectory(s"$buildDir/../src/main/java")
    build.setTestSourceDirectory(s"$buildDir/../src/test/java")
    p.setBuild(build)
    p
  }

  private def pluginWith(key: String, cfg: Xpp3Dom): Plugin = {
    val Array(g, a) = key.split(":")
    val plugin = new Plugin()
    plugin.setGroupId(g)
    plugin.setArtifactId(a)
    plugin.setConfiguration(cfg)
    plugin
  }

  private def executionWith(goal: String, cfg: Xpp3Dom): PluginExecution = {
    val e = new PluginExecution()
    e.addGoal(goal)
    e.setConfiguration(cfg)
    e
  }

  private def projectWithManagedPlugin(
      key: String,
      cfg: Xpp3Dom,
      buildDir: String = "/project/target",
  ): MavenProject = {
    val p = project(buildDir)
    val pm = new PluginManagement()
    pm.addPlugin(pluginWith(key, cfg))
    p.getBuild.setPluginManagement(pm)
    p
  }

  private def node(name: String, value: String): Xpp3Dom = {
    val d = new Xpp3Dom(name)
    d.setValue(value)
    d
  }

  private def node(name: String, children: Xpp3Dom*): Xpp3Dom = {
    val d = new Xpp3Dom(name)
    children.foreach(d.addChild)
    d
  }

  private def sourceRoots(p: MavenProject, isTest: Boolean): List[String] = {
    val rawRoots =
      if (isTest) p.getTestCompileSourceRoots
      else p.getCompileSourceRoots
    MavenSourceRoots.existingSources(rawRoots, p, isTest)
  }

  // ── annotation processing ────────────────────────────────────────────────

  test("annotation processing: bare compiler plugin does NOT add AP root") {
    val p = project()
    p.getBuild.addPlugin(pluginWith(JavaCompilerPlugin, node("configuration")))
    val roots = sourceRoots(p, isTest = false)
    assert(!roots.contains("/project/target/generated-sources/annotations"))
  }

  test(
    "annotation processing: annotationProcessors element triggers default main root"
  ) {
    val p = project()
    p.getBuild.addPlugin(
      pluginWith(
        JavaCompilerPlugin,
        node("configuration", node("annotationProcessors")),
      )
    )
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/project/target/generated-sources/annotations"))
  }

  test(
    "annotation processing: annotationProcessorPaths element triggers default main root"
  ) {
    val p = project()
    p.getBuild.addPlugin(
      pluginWith(
        JavaCompilerPlugin,
        node("configuration", node("annotationProcessorPaths")),
      )
    )
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/project/target/generated-sources/annotations"))
  }

  test(
    "annotation processing: default test root is generated-test-sources/test-annotations"
  ) {
    val p = project()
    p.getBuild.addPlugin(
      pluginWith(
        JavaCompilerPlugin,
        node("configuration", node("annotationProcessors")),
      )
    )
    val roots = sourceRoots(p, isTest = true)
    assert(
      roots.contains("/project/target/generated-test-sources/test-annotations")
    )
    assert(
      !roots.contains("/project/target/generated-test-sources/annotations")
    )
  }

  test("annotation processing: explicit generatedSourcesDirectory is used") {
    val p = project()
    p.getBuild.addPlugin(
      pluginWith(
        JavaCompilerPlugin,
        node("configuration", node("generatedSourcesDirectory", "/custom/gen")),
      )
    )
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/custom/gen"))
    assert(!roots.contains("/project/target/generated-sources/annotations"))
  }

  test(
    "annotation processing: proc=none suppresses root even with annotationProcessors"
  ) {
    val p = project()
    p.getBuild.addPlugin(
      pluginWith(
        JavaCompilerPlugin,
        node(
          "configuration",
          node("proc", "none"),
          node("annotationProcessors"),
        ),
      )
    )
    val roots = sourceRoots(p, isTest = false)
    assert(!roots.contains("/project/target/generated-sources/annotations"))
  }

  test(
    "annotation processing: AP config in testCompile execution detected for tests"
  ) {
    val p = project()
    val plugin = pluginWith(JavaCompilerPlugin, node("configuration"))
    plugin.addExecution(
      executionWith(
        "testCompile",
        node("configuration", node("annotationProcessorPaths")),
      )
    )
    p.getBuild.addPlugin(plugin)
    val roots = sourceRoots(p, isTest = true)
    assert(
      roots.contains("/project/target/generated-test-sources/test-annotations")
    )
  }

  // ── protobuf ─────────────────────────────────────────────────────────────

  test("protobuf: no executions → both java and grpc-java roots for main") {
    val p = project()
    p.getBuild.addPlugin(pluginWith(ProtobufMavenPlugin, node("configuration")))
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/project/target/generated-sources/protobuf/java"))
    assert(
      roots.contains("/project/target/generated-sources/protobuf/grpc-java")
    )
  }

  test(
    "protobuf: no executions → both java and grpc-java roots for tests (default lifecycle binding)"
  ) {
    val p = project()
    p.getBuild.addPlugin(pluginWith(ProtobufMavenPlugin, node("configuration")))
    val roots = sourceRoots(p, isTest = true)
    assert(
      roots.contains("/project/target/generated-test-sources/protobuf/java")
    )
    assert(
      roots.contains(
        "/project/target/generated-test-sources/protobuf/grpc-java"
      )
    )
  }

  test(
    "protobuf: plugin-level outputDirectory visible even when execution has no output"
  ) {
    val p = project()
    val plugin =
      pluginWith(
        ProtobufMavenPlugin,
        node("configuration", node("outputDirectory", "/plugin/proto")),
      )
    plugin.addExecution(executionWith("compile", node("configuration")))
    p.getBuild.addPlugin(plugin)
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/plugin/proto"))
  }

  test("protobuf: compile execution output is used for main") {
    val p = project()
    val plugin = pluginWith(ProtobufMavenPlugin, node("configuration"))
    plugin.addExecution(
      executionWith(
        "compile",
        node("configuration", node("outputDirectory", "/exec/proto")),
      )
    )
    p.getBuild.addPlugin(plugin)
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/exec/proto"))
  }

  test("protobuf: explicit compile-only executions suppress test roots") {
    val p = project()
    val plugin = pluginWith(ProtobufMavenPlugin, node("configuration"))
    plugin.addExecution(
      executionWith(
        "compile",
        node("configuration", node("outputDirectory", "/exec/proto")),
      )
    )
    p.getBuild.addPlugin(plugin)
    val roots = sourceRoots(p, isTest = true)
    assert(
      !roots.contains("/project/target/generated-test-sources/protobuf/java")
    )
    assert(
      !roots.contains(
        "/project/target/generated-test-sources/protobuf/grpc-java"
      )
    )
    assert(!roots.contains("/exec/proto"))
  }

  test("protobuf: test-compile execution contributes test roots") {
    val p = project()
    val plugin = pluginWith(ProtobufMavenPlugin, node("configuration"))
    plugin.addExecution(
      executionWith(
        "test-compile",
        node("configuration", node("outputDirectory", "/exec/test-proto")),
      )
    )
    p.getBuild.addPlugin(plugin)
    val roots = sourceRoots(p, isTest = true)
    assert(roots.contains("/exec/test-proto"))
    assert(
      roots.contains(
        "/project/target/generated-test-sources/protobuf/grpc-java"
      )
    )
  }

  test(
    "protobuf: two executions with distinct outputDirectories yield two roots"
  ) {
    val p = project()
    val plugin = pluginWith(ProtobufMavenPlugin, node("configuration"))
    plugin.addExecution(
      executionWith(
        "compile",
        node("configuration", node("outputDirectory", "/out/a")),
      )
    )
    plugin.addExecution(
      executionWith(
        "compile-custom",
        node("configuration", node("outputDirectory", "/out/b")),
      )
    )
    p.getBuild.addPlugin(plugin)
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/out/a"))
    assert(roots.contains("/out/b"))
  }

  // ── pluginManagement inheritance (multi-module pattern) ──────────────────

  test(
    "generator declared only in pluginManagement is still detected (protobuf)"
  ) {
    val p = projectWithManagedPlugin(ProtobufMavenPlugin, node("configuration"))
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/project/target/generated-sources/protobuf/java"))
    assert(
      roots.contains("/project/target/generated-sources/protobuf/grpc-java")
    )
  }

  test(
    "generator in pluginManagement with custom outputDirectory is respected"
  ) {
    val p = projectWithManagedPlugin(
      ProtobufMavenPlugin,
      node("configuration", node("outputDirectory", "/custom/proto")),
    )
    val roots = sourceRoots(p, isTest = false)
    assert(roots.contains("/custom/proto"))
  }

  test("each module uses its own build directory for generator roots") {
    val moduleA = projectWithManagedPlugin(
      ProtobufMavenPlugin,
      node("configuration"),
      buildDir = "/parent/module-a/target",
    )
    val moduleB = projectWithManagedPlugin(
      ProtobufMavenPlugin,
      node("configuration"),
      buildDir = "/parent/module-b/target",
    )
    val rootsA = sourceRoots(moduleA, isTest = false)
    val rootsB = sourceRoots(moduleB, isTest = false)
    assert(
      rootsA.contains("/parent/module-a/target/generated-sources/protobuf/java")
    )
    assert(
      rootsB.contains("/parent/module-b/target/generated-sources/protobuf/java")
    )
    assert(
      !rootsA.contains(
        "/parent/module-b/target/generated-sources/protobuf/java"
      )
    )
  }

  // ── no-exists filter ─────────────────────────────────────────────────────

  test(
    "configured generator roots are included even when the directory does not exist"
  ) {
    val p = project(buildDir = "/nonexistent/target")
    p.getBuild.addPlugin(pluginWith(ProtobufMavenPlugin, node("configuration")))
    val roots = sourceRoots(p, isTest = false)
    assert(
      roots.contains("/nonexistent/target/generated-sources/protobuf/java")
    )
  }
}
