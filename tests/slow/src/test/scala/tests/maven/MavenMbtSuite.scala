package tests.maven

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import scala.meta.internal.metals.{BuildInfo => V}

import com.google.gson.JsonObject
import com.google.gson.JsonParser

class MavenMbtSuite extends munit.FunSuite {

  private val pluginGoal =
    s"org.scalameta:metals-maven-plugin:${V.metalsMavenPluginVersion}:export"

  test("single-module-export") {
    val workspace = newWorkspace()
    writePom(workspace, "single-module.xml")
    mkdirs(workspace, "src/main/scala", "src/test/scala")
    val build = runExport(workspace)
    assertNamespaceNames(
      build,
      Set(
        "com.example:maven-mbt-test:1.0-SNAPSHOT",
        "com.example:maven-mbt-test:1.0-SNAPSHOT:test",
      ),
    )
    assert(
      relativeSources(
        workspace,
        namespace(build, "com.example:maven-mbt-test:1.0-SNAPSHOT"),
      ) == List("src/main/scala")
    )
  }

  test("compiler-options-from-pom") {
    val workspace = newWorkspace()
    writePom(workspace, "compiler-options.xml")
    mkdirs(workspace, "src/main/scala", "src/test/scala")
    val build = runExport(workspace)
    val main = namespace(build, "com.example:compiler-options:1.0.0")
    val test = namespace(build, "com.example:compiler-options:1.0.0:test")
    assert(
      strings(main, "javacOptions") == List(
        "--release", "11", "-encoding", "UTF-8", "--enable-preview",
        "-parameters", "-proc:full", "-processor",
        "example.MainProcessor,example.SecondProcessor", "-Xlint:deprecation",
        "-ApluginConfig=top", "-Werror", "-Xdoclint:none", "-Ametals:enabled",
        "-verbose",
      )
    )
    assert(
      strings(test, "javacOptions") == List(
        "-source", "17", "-target", "17", "-encoding", "UTF-8",
        "--enable-preview", "-parameters", "-proc:full", "-processor",
        "example.MainProcessor,example.SecondProcessor", "-Xlint:deprecation",
        "-ApluginConfig=top", "-AtestExecution=true", "-Werror",
        "-Xdoclint:none", "-Ametals:enabled", "-verbose",
      )
    )
    assert(
      strings(main, "scalacOptions") == List(
        "-Xlint",
        "-deprecation",
        "-feature",
      )
    )
    assert(
      strings(test, "scalacOptions") == List(
        "-unchecked",
        "-Wconf:any:warning-verbose",
        "-Xsource:3",
      )
    )
    assert(string(main, "scalaVersion") == Some(V.scala213))
    assert(string(test, "scalaVersion") == Some("2.13"))
  }

  test("source-roots-from-pom") {
    val workspace = newWorkspace()
    writePom(workspace, "source-roots.xml")
    mkdirs(
      workspace,
      "src/main/java",
      "src/main/scala",
      "src/test/java",
      "src/test/scala",
      "src/generated/scala",
      "src/generated/extra-main",
      "src/generated-test/scala",
      "target/generated-sources/openapi/src/main/java/example",
      "target/generated-sources/plain/example",
      "target/generated-test-sources/spec/src/test/scala/example",
    )
    writeFile(workspace, "target/generated-sources/Top.java", "class Top {}")
    writeFile(
      workspace,
      "target/generated-sources/openapi/src/main/java/example/OpenApi.java",
      "package example; class OpenApi {}",
    )
    writeFile(
      workspace,
      "target/generated-sources/plain/example/Plain.scala",
      "package example\nobject Plain",
    )
    writeFile(
      workspace,
      "target/generated-test-sources/TopTest.java",
      "class TopTest {}",
    )
    writeFile(
      workspace,
      "target/generated-test-sources/spec/src/test/scala/example/SpecGenerated.scala",
      "package example\nclass SpecGenerated",
    )
    val build = runExport(workspace)
    val mainSources = relativeSources(
      workspace,
      namespace(build, "com.example:source-roots:1.0.0"),
    )
    val testSources =
      relativeSources(
        workspace,
        namespace(build, "com.example:source-roots:1.0.0:test"),
      )
    assertContainsAll(
      mainSources,
      List(
        "src/main/java", "src/main/scala",
        "target/generated-sources/antlr-custom", "src/generated/scala",
        "src/generated/extra-main", "target/generated-sources/apt-main",
        "target/generated-sources/protobuf-custom",
        "target/generated-sources/protobuf/grpc-java",
        "target/generated-sources",
        "target/generated-sources/openapi/src/main/java",
        "target/generated-sources/plain",
      ),
    )
    assertContainsAll(
      testSources,
      List(
        "src/test/java", "src/test/scala", "src/generated-test/scala",
        "target/generated-test-sources/apt-test",
        "target/generated-test-sources/protobuf-test-custom",
        "target/generated-test-sources/protobuf/grpc-java",
        "target/generated-test-sources",
        "target/generated-test-sources/spec/src/test/scala",
      ),
    )
    assert(mainSources.distinct == mainSources)
    assert(testSources.distinct == testSources)
  }

  test("java-only-pom") {
    val workspace = newWorkspace()
    writePom(workspace, "java-only.xml")
    mkdirs(workspace, "src/main/java", "src/test/java")
    val build = runExport(workspace)
    assertNamespaceNames(
      build,
      Set("com.example:java-only:1.0.0", "com.example:java-only:1.0.0:test"),
    )
    val main = namespace(build, "com.example:java-only:1.0.0")
    val test = namespace(build, "com.example:java-only:1.0.0:test")
    assert(strings(main, "javacOptions") == List("--release", "17"))
    assert(strings(test, "dependsOn") == List("com.example:java-only:1.0.0"))
    assert(main.get("scalaVersion").isJsonNull)
    assert(test.get("scalaVersion").isJsonNull)
  }

  test("jdk-from-forked-compiler-pom") {
    val workspace = newWorkspace()
    writePom(workspace, "jdk-fork.xml")
    mkdirs(
      workspace,
      "src/main/java",
      "src/test/java",
      "jdk-main/bin",
      "jdk-test/bin",
    )
    mkExecutable(workspace, "jdk-main/bin/javac")
    mkExecutable(workspace, "jdk-test/bin/javac")
    val build = runExport(workspace)
    val main = namespace(build, "com.example:jdk-fork:1.0.0")
    val test = namespace(build, "com.example:jdk-fork:1.0.0:test")
    assert(
      string(main, "javaHome").map(toRelative(workspace, _)) == Some("jdk-main")
    )
    assert(
      string(test, "javaHome").map(toRelative(workspace, _)) == Some("jdk-test")
    )
    assert(strings(main, "javacOptions") == List("--release", "11"))
    assert(strings(test, "javacOptions") == List("--release", "17"))
  }

  test("jdktoolchain-in-compiler-plugin-selects-matching-toolchain") {
    val workspace = newWorkspace()
    writePom(workspace, "jdktoolchain-compiler-plugin.xml")
    mkdirs(workspace, "src/main/java", "src/test/java")
    fakeJdk(workspace, "jdk-17", "17.0.12")
    val jdk21 = fakeJdk(workspace, "jdk-21", "21.0.8")
    val tc = writeToolchains(
      workspace.resolve("toolchains.xml"),
      ("17", None, workspace.resolve("jdk-17")),
      ("21", None, jdk21),
    )
    val build = runExport(workspace, Some(tc))
    val main =
      namespace(build, "com.example:jdktoolchain-compiler-plugin:1.0.0")
    assert(
      string(main, "javaHome").map(toRelative(workspace, _)) == Some("jdk-21"),
      s"expected jdk-21, got ${string(main, "javaHome")}",
    )
    assert(strings(main, "javacOptions").contains("21"))
  }

  test("toolchains-plugin-config-in-pom-wins-over-release-in-javac-options") {
    val workspace = newWorkspace()
    writePom(workspace, "toolchains-plugin-jdk17.xml")
    mkdirs(workspace, "src/main/java", "src/test/java")
    val jdk17 = fakeJdk(workspace, "jdk-17", "17.0.12")
    fakeJdk(workspace, "jdk-21", "21.0.8")
    val tc = writeToolchains(
      workspace.resolve("toolchains.xml"),
      ("17", None, jdk17),
      ("21", None, workspace.resolve("jdk-21")),
    )
    val build = runExport(workspace, Some(tc))
    val main = namespace(build, "com.example:toolchains-plugin-jdk17:1.0.0")
    assert(
      string(main, "javaHome").map(toRelative(workspace, _)) == Some("jdk-17"),
      s"expected jdk-17, got ${string(main, "javaHome")}",
    )
  }

  test("fork-executable-wins-over-jdktoolchain-in-compiler-plugin") {
    val workspace = newWorkspace()
    writePom(workspace, "fork-beats-jdktoolchain.xml")
    mkdirs(workspace, "src/main/java", "src/test/java", "jdk-17/bin")
    mkExecutable(workspace, "jdk-17/bin/javac")
    fakeJdk(workspace, "jdk-21", "21.0.8")
    val tc = writeToolchains(
      workspace.resolve("toolchains.xml"),
      ("17", None, workspace.resolve("jdk-17")),
      ("21", None, workspace.resolve("jdk-21")),
    )
    val build = runExport(workspace, Some(tc))
    val main = namespace(build, "com.example:fork-beats-jdktoolchain:1.0.0")
    assert(
      string(main, "javaHome").map(toRelative(workspace, _)) == Some("jdk-17"),
      s"fork/executable must win over jdkToolchain: got ${string(main, "javaHome")}",
    )
  }

  test("maven-compiler-release-property-selects-matching-toolchain") {
    val workspace = newWorkspace()
    writePom(workspace, "release-21-toolchain.xml")
    mkdirs(workspace, "src/main/java", "src/test/java")
    fakeJdk(workspace, "jdk-17", "17.0.12")
    val jdk21 = fakeJdk(workspace, "jdk-21", "21.0.8")
    val tc = writeToolchains(
      workspace.resolve("toolchains.xml"),
      ("17", None, workspace.resolve("jdk-17")),
      ("21", None, jdk21),
    )
    val build = runExport(workspace, Some(tc))
    val main = namespace(build, "com.example:release-21-toolchain:1.0.0")
    assert(
      string(main, "javaHome").map(toRelative(workspace, _)) == Some("jdk-21"),
      s"expected jdk-21 selected by release=21, got ${string(main, "javaHome")}",
    )
    assert(strings(main, "javacOptions").contains("21"))
  }

  test("main-and-test-namespaces-resolve-to-different-jdk-homes") {
    val workspace = newWorkspace()
    writePom(workspace, "main-17-test-21-executions.xml")
    mkdirs(workspace, "src/main/java", "src/test/java")
    val jdk17 = fakeJdk(workspace, "jdk-17", "17.0.12")
    val jdk21 = fakeJdk(workspace, "jdk-21", "21.0.8")
    val tc = writeToolchains(
      workspace.resolve("toolchains.xml"),
      ("17", None, jdk17),
      ("21", None, jdk21),
    )
    val build = runExport(workspace, Some(tc))
    val main = namespace(build, "com.example:main-17-test-21:1.0.0")
    val test = namespace(build, "com.example:main-17-test-21:1.0.0:test")
    assert(
      string(main, "javaHome").map(toRelative(workspace, _)) == Some("jdk-17"),
      s"main namespace must use jdk-17, got ${string(main, "javaHome")}",
    )
    assert(
      string(test, "javaHome").map(toRelative(workspace, _)) == Some("jdk-21"),
      s"test namespace must use jdk-21, got ${string(test, "javaHome")}",
    )
    assert(strings(main, "javacOptions").contains("17"))
    assert(strings(test, "javacOptions").contains("21"))
  }

  test("jdktoolchain-vendor-narrows-toolchain-selection") {
    val workspace = newWorkspace()
    writePom(workspace, "jdktoolchain-vendor-temurin.xml")
    mkdirs(workspace, "src/main/java", "src/test/java")
    val jdkTemurin = fakeJdk(workspace, "jdk-temurin-21", "21.0.8")
    fakeJdk(workspace, "jdk-oracle-21", "21.0.8")
    val tc = writeToolchains(
      workspace.resolve("toolchains.xml"),
      ("21", Some("temurin"), jdkTemurin),
      ("21", Some("oracle"), workspace.resolve("jdk-oracle-21")),
    )
    val build = runExport(workspace, Some(tc))
    val main = namespace(build, "com.example:jdktoolchain-vendor-temurin:1.0.0")
    assert(
      string(main, "javaHome").map(toRelative(workspace, _)) == Some(
        "jdk-temurin-21"
      ),
      s"vendor=temurin must select jdk-temurin-21, got ${string(main, "javaHome")}",
    )
  }

  test("two-independent-build-roots-are-exported-without-cross-contamination") {
    val workspaceA = newWorkspace()
    writePom(workspaceA, "single-module.xml")
    mkdirs(workspaceA, "src/main/scala", "src/test/scala")

    val workspaceB = newWorkspace()
    writePom(workspaceB, "java-only.xml")
    mkdirs(workspaceB, "src/main/java", "src/test/java")

    val buildA = runExport(workspaceA)
    val buildB = runExport(workspaceB)

    assertNamespaceNames(
      buildA,
      Set(
        "com.example:maven-mbt-test:1.0-SNAPSHOT",
        "com.example:maven-mbt-test:1.0-SNAPSHOT:test",
      ),
    )
    assertNamespaceNames(
      buildB,
      Set("com.example:java-only:1.0.0", "com.example:java-only:1.0.0:test"),
    )

    val depsA = dependencyIdsFrom(buildA)
    val depsB = dependencyIdsFrom(buildB)

    assert(
      depsA.exists(_.startsWith("org.scala-lang:scala-library:")),
      "root A (Scala project) must declare scala-library",
    )
    assert(
      !depsB.exists(_.startsWith("org.scala-lang:scala-library:")),
      "root B (Java-only project) must not contain scala-library from root A",
    )

    val sourcesA =
      relativeSources(
        workspaceA,
        namespace(buildA, "com.example:maven-mbt-test:1.0-SNAPSHOT"),
      )
    assert(
      sourcesA.forall(!_.startsWith(workspaceB.toString)),
      "root A source paths must not reference root B workspace",
    )
  }

  test("reactor-modules-from-pom") {
    val workspace = newWorkspace()
    writePom(workspace, "reactor-parent.xml")
    writeModulePom(workspace, "core", "reactor-core.xml")
    writeModulePom(workspace, "util", "reactor-util.xml")
    writeModulePom(workspace, "app", "reactor-app.xml")
    for (module <- List("core", "util", "app"))
      mkdirs(workspace, s"$module/src/main/scala", s"$module/src/test/scala")
    val build = runExport(workspace)
    assertNamespaceNames(
      build,
      Set(
        "com.example:core:1.0.0", "com.example:core:1.0.0:test",
        "com.example:util:1.0.0", "com.example:util:1.0.0:test",
        "com.example:app:1.0.0", "com.example:app:1.0.0:test",
      ),
    )
    assert(
      strings(namespace(build, "com.example:core:1.0.0:test"), "dependsOn") ==
        List("com.example:core:1.0.0")
    )
    assert(
      strings(namespace(build, "com.example:app:1.0.0"), "dependsOn") ==
        List("com.example:core:1.0.0")
    )
    assert(
      strings(
        namespace(build, "com.example:app:1.0.0:test"),
        "dependsOn",
      ).toSet ==
        Set(
          "com.example:app:1.0.0",
          "com.example:core:1.0.0",
          "com.example:core:1.0.0:test",
          "com.example:util:1.0.0",
        )
    )
    val dependencyIds = dependencyIdsFrom(build)
    assert(dependencyIds.distinct == dependencyIds)
    assert(
      dependencyIds.contains(s"org.scala-lang:scala-library:${V.scala213}")
    )
  }

  private def newWorkspace(): Path =
    Files.createTempDirectory("maven-mbt-suite").toRealPath()

  private def writePom(workspace: Path, pomName: String): Unit =
    Files.writeString(workspace.resolve("pom.xml"), pom(pomName))

  private def writeModulePom(
      workspace: Path,
      module: String,
      pomName: String,
  ): Unit = {
    Files.createDirectories(workspace.resolve(module))
    Files.writeString(workspace.resolve(s"$module/pom.xml"), pom(pomName))
  }

  private def mkdirs(workspace: Path, paths: String*): Unit =
    paths.foreach(p => Files.createDirectories(workspace.resolve(p)))

  private def writeFile(
      workspace: Path,
      relPath: String,
      content: String,
  ): Unit = {
    val file = workspace.resolve(relPath)
    Files.createDirectories(file.getParent)
    Files.writeString(file, content)
  }

  private def mkExecutable(workspace: Path, relPath: String): Unit = {
    val file = workspace.resolve(relPath)
    Files.writeString(file, "#!/bin/sh\n")
    file.toFile.setExecutable(true)
  }

  private lazy val mavenWrapperCommand: List[String] = {
    val tempDir = Files.createTempDirectory("metals-maven-wrapper")
    for (resource <- List("maven-wrapper.jar", "maven-wrapper.properties")) {
      val in = getClass.getResourceAsStream(s"/$resource")
      Files.copy(
        in,
        tempDir.resolve(resource),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING,
      )
    }
    List(
      scala.meta.internal.metals.JavaBinary(None),
      "-Dfile.encoding=UTF-8",
      s"-Dmaven.home=$tempDir",
      "-cp",
      tempDir.resolve("maven-wrapper.jar").toString,
      "org.apache.maven.wrapper.MavenWrapperMain",
    )
  }

  private def runExport(
      workspace: Path,
      toolchains: Option[Path] = None,
  ): JsonObject = {
    val outputFile = workspace.resolve("mbt.json")
    val toolchainsArgs =
      toolchains.toList.flatMap(tc => List("--toolchains", tc.toString))
    val command = mavenWrapperCommand :::
      List(
        s"-Dmaven.multiModuleProjectDirectory=$workspace",
        "--quiet",
        pluginGoal,
        s"-DmbtOutputFile=$outputFile",
      ) ::: toolchainsArgs
    val pb = new ProcessBuilder(command: _*)
      .directory(workspace.toFile)
      .inheritIO()
    val exitCode = pb.start().waitFor()
    assert(
      exitCode == 0,
      s"mvn export failed with exit code $exitCode in $workspace",
    )
    JsonParser.parseString(Files.readString(outputFile)).getAsJsonObject
  }

  private def fakeJdk(workspace: Path, name: String, version: String): Path = {
    val jdkDir = workspace.resolve(name)
    mkdirs(workspace, s"$name/bin")
    mkExecutable(workspace, s"$name/bin/java")
    mkExecutable(workspace, s"$name/bin/javac")
    Files.writeString(
      jdkDir.resolve("release"),
      s"""JAVA_VERSION="$version"\nIMPLEMENTOR="Test JDK"\n""",
    )
    jdkDir
  }

  private def writeToolchains(
      dest: Path,
      entries: (String, Option[String], Path)*
  ): Path = {
    val blocks = entries.map { case (version, vendor, path) =>
      val vendorXml =
        vendor.map(v => s"\n      <vendor>$v</vendor>").getOrElse("")
      s"""|  <toolchain>
          |    <type>jdk</type>
          |    <provides>
          |      <version>$version</version>$vendorXml
          |    </provides>
          |    <configuration><jdkHome>$path</jdkHome></configuration>
          |  </toolchain>""".stripMargin
    }
    val xml =
      s"""|<?xml version="1.0" encoding="UTF-8"?>
          |<toolchains xmlns="http://maven.apache.org/TOOLCHAINS/1.1.0">
          |${blocks.mkString("\n")}
          |</toolchains>""".stripMargin
    Files.writeString(dest, xml)
    dest
  }

  private def namespace(build: JsonObject, name: String): JsonObject = {
    val ns = build.getAsJsonObject("namespaces").get(name)
    assert(ns != null && ns.isJsonObject, s"missing namespace: $name")
    ns.getAsJsonObject
  }

  private def assertNamespaceNames(
      build: JsonObject,
      expected: Set[String],
  ): Unit = {
    val names = Set.newBuilder[String]
    build.getAsJsonObject("namespaces").keySet.forEach(names += _)
    assert(names.result() == expected)
  }

  private def strings(obj: JsonObject, field: String): List[String] = {
    val result = List.newBuilder[String]
    obj.getAsJsonArray(field).forEach(e => result += e.getAsString)
    result.result()
  }

  private def string(obj: JsonObject, field: String): Option[String] =
    Option(obj.get(field)).filterNot(_.isJsonNull).map(_.getAsString)

  private def relativeSources(workspace: Path, ns: JsonObject): List[String] =
    strings(ns, "sources").map(toRelative(workspace, _))

  private def toRelative(workspace: Path, path: String): String =
    workspace.relativize(Path.of(path)).toString.replace('\\', '/')

  private def assertContainsAll(
      actual: List[String],
      expected: List[String],
  ): Unit = {
    val missing = expected.filterNot(actual.contains)
    assert(
      missing.isEmpty,
      s"missing: ${missing.mkString(", ")}; actual: ${actual.sorted.mkString(", ")}",
    )
  }

  private def dependencyIdsFrom(build: JsonObject): List[String] = {
    val result = List.newBuilder[String]
    build.getAsJsonArray("dependencyModules").forEach { e =>
      result += e.getAsJsonObject.get("id").getAsString
    }
    result.result()
  }

  private def pom(name: String): String = {
    val stream = getClass.getResourceAsStream(s"/maven/poms/$name")
    assert(
      stream != null,
      s"POM fixture not found on classpath: /maven/poms/$name",
    )
    new String(stream.readAllBytes(), StandardCharsets.UTF_8)
      .replace("@@SCALA_VERSION@@", V.scala213)
  }
}
