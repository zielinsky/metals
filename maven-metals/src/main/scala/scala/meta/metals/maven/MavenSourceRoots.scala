package scala.meta.metals.maven

import java.io.File
import java.{util => ju}

import scala.jdk.CollectionConverters._

import org.apache.maven.project.MavenProject
import org.codehaus.plexus.util.xml.Xpp3Dom

private[maven] object MavenSourceRoots {

  import MavenPluginSupport._

  def existingSources(
      roots: ju.List[String],
      project: MavenProject,
      isTest: Boolean,
  ): List[String] = {
    val declared = roots.asScala
      .map(new File(_))
      .filter(_.exists())
      .map(_.getAbsolutePath)
      .toList

    val scalaDir = new File(
      project.getBasedir,
      if (isTest) "src/test/scala" else "src/main/scala",
    )
    val scalaFallback =
      if (scalaDir.isDirectory && !declared.contains(scalaDir.getAbsolutePath))
        List(scalaDir.getAbsolutePath)
      else Nil

    val configuredGenerated =
      configuredGeneratedSourceRoots(project, isTest)

    val generatedParent = new File(
      project.getBasedir,
      if (isTest) "target/generated-test-sources"
      else "target/generated-sources",
    )
    val generated =
      if (generatedParent.isDirectory)
        generatedSourceRootsFromDirectory(
          generatedParent,
          isTest,
          declared.toSet,
        )
      else Nil

    (declared ++ scalaFallback ++ configuredGenerated ++ generated).distinct
  }

  private def generatedSourceRootsFromDirectory(
      generatedParent: File,
      isTest: Boolean,
      declared: Set[String],
  ): List[String] = {
    val children = Option(generatedParent.listFiles()).toList.flatten
      .filter(_.isDirectory)
      .map(_.getAbsolutePath)
      .filterNot(declared)

    if (isTest) generatedParent.getAbsolutePath :: children
    else children
  }

  private def configuredGeneratedSourceRoots(
      project: MavenProject,
      isTest: Boolean,
  ): List[String] = {
    val plugins = effectivePlugins(project)
    val antlrRoots =
      if (!isTest && plugins.containsKey(Antlr4MavenPlugin)) {
        val plugin = plugins.get(Antlr4MavenPlugin)
        val outputDirectory =
          mergedPluginConfiguration(plugin)
            .flatMap(childText(_, "outputDirectory"))
            .map(interpolatePath(_, project))
            .getOrElse(
              new File(
                project.getBuild.getDirectory,
                "generated-sources/antlr4",
              ).getPath
            )
        List(absolutePath(outputDirectory, project))
      } else Nil

    val buildHelperRoots =
      Option(plugins.get(BuildHelperMavenPlugin)).toList.flatMap { plugin =>
        val goal = if (isTest) "add-test-source" else "add-source"
        plugin.getExecutions.asScala.toList
          .filter(_.getGoals.asScala.contains(goal))
          .flatMap(e => mergedPluginConfiguration(plugin, e))
          .flatMap(buildHelperSources(_, project))
      }

    (antlrRoots ++ buildHelperRoots).distinct
  }

  private def buildHelperSources(
      cfg: Xpp3Dom,
      project: MavenProject,
  ): List[String] =
    Option(cfg.getChild("sources")).toList.flatMap { sources =>
      sources.getChildren("source").toList.map { source =>
        absolutePath(interpolatePath(source.getValue, project), project)
      }
    }
}
