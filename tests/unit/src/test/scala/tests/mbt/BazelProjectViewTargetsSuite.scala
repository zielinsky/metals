package tests.mbt

import scala.meta.internal.builds.BazelProjectViewTargets

import munit.FunSuite

class BazelProjectViewTargetsSuite extends FunSuite {

  test("parse-indented-targets") {
    val text = """targets:
                 |    //apps/...
                 |    //libs/...
                 |
                 |build_manual_targets: false
                 |""".stripMargin
    assertEquals(
      BazelProjectViewTargets.parseFromContent(text),
      List("//apps/...", "//libs/..."),
    )
  }

  test("parse-inline-targets") {
    val text = """targets: //foo/...
                 |
                 |directories:
                 |    .
                 |""".stripMargin
    assertEquals(
      BazelProjectViewTargets.parseFromContent(text),
      List("//foo/..."),
    )
  }

  test("parse-no-targets-key") {
    assertEquals(BazelProjectViewTargets.parseFromContent("foo: bar"), Nil)
  }

}
