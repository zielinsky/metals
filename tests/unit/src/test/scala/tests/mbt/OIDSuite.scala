package tests.mbt

import scala.util.Properties

import scala.meta.internal.metals.mbt.OID

class OIDSuite extends munit.FunSuite {
  test("fromText") {
    // Taken from bin/bloop.json in the Metals repo.
    val text = """{
  "javaOptions": ["-Xmx1G", "-Xss16m"]
}
"""
    val oid = OID.fromText(text)
    if (Properties.isWin)
      assertEquals(oid, "2f0dc7a4ec2b7740cd4d5d39ec3ef0558f9d2f35")
    else
      assertEquals(oid, "1f30594bc0798678457920a78b1ef25f387d05ba")
  }
}
