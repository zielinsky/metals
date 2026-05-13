package scala.meta.internal.builds

import java.security.MessageDigest

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

object MavenDigest extends Digestable {
  override protected def digestWorkspace(
      workspace: AbsolutePath,
      digest: MessageDigest,
  ): Boolean = {
    val digestedPoms = workspace.listRecursive.forall {
      case file if file.filename == "pom.xml" =>
        Digest.digestFile(file, digest)
      case _ =>
        true
    }

    val mvnDir = workspace.resolve(".mvn")
    val digestedMvn =
      List("toolchains.xml", "maven.config", "jvm.config", "extensions.xml")
        .forall { name =>
          Digest.digestFile(mvnDir.resolve(name), digest)
        }

    val m2 = AbsolutePath(
      java.nio.file.Paths.get(System.getProperty("user.home"), ".m2")
    )
    val digestedGlobalM2 =
      List("settings.xml", "toolchains.xml").forall { name =>
        Digest.digestFile(m2.resolve(name), digest)
      }

    digestedPoms && digestedMvn && digestedGlobalM2
  }
}
