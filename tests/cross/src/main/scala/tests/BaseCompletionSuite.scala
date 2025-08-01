package tests

import java.nio.file.Paths
import java.util.Collections

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.TextEdits
import scala.meta.internal.mtags.CommonMtagsEnrichments.XtensionCompletionItemData
import scala.meta.pc.CancelToken

import munit.Location
import munit.TestOptions
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionList

abstract class BaseCompletionSuite extends BasePCSuite {

  private def cancelToken: CancelToken = EmptyCancelToken

  private def resolvedCompletions(
      params: CompilerOffsetParams,
      restart: Boolean
  ): CompletionList = {
    if (restart) presentationCompiler.restart()
    val result = presentationCompiler.complete(params).get()
    val newItems = result.getItems.asScala.map { item =>
      item.data
        .map { data =>
          presentationCompiler.completionItemResolve(item, data.symbol).get()
        }
        .getOrElse(item)
    }
    result.setItems(newItems.asJava)
    result
  }

  // NOTE: this filters out things from `java.lang.classfile` which was added in JDK 22
  protected def getItems(
      original: String,
      filename: String = "A.scala",
      restart: Boolean = true
  ): Seq[CompletionItem] = {
    val (code, offset) = params(original, filename)
    val result = resolvedCompletions(
      CompilerOffsetParams(
        Paths.get(filename).toUri(),
        code,
        offset,
        cancelToken
      ),
      restart
    )
    result.getItems.asScala
      .filterNot(item => item.getLabel().contains("- java.lang.classfile"))
      .sortBy(item => Option(item.getSortText).getOrElse(item.getLabel()))
      .toSeq
  }

  /**
   * Check completions using `fn` returned at @@ cursor position indicated in the `original` string.
   *
   * @param name name of the test
   * @param original snippet to test with `@@` indicating the cursor position
   * @param fn function used to run assertions on completion items
   */
  def checkItems(
      name: TestOptions,
      original: String,
      fn: Seq[CompletionItem] => Boolean
  )(implicit loc: Location): Unit = {
    test(name) { assert(fn(getItems(original))) }
  }

  /**
   * Check completion line `original`, which if included in the `template` should be
   * changed to `expected` if first completion on the list is applied.
   *
   * @param name name of the tests
   * @param template whole source file with `---` indicating where line `original`
   * should be inserted.
   * @param original line to be inserted and checked
   * @param expected expected line `original` after being modified with the first completion
   * @param filterText filter returned completions according to text
   * @param assertSingleItem make sure only one item is suggested, true by default
   * @param filter similar to filterText, but uses a function
   * @param command additional command that should be applied after this completion is inserted
   * @param compat additional compatibility map for different Scala versions
   */
  def checkEditLine(
      name: TestOptions,
      template: String,
      original: String,
      expected: String,
      filterText: String = "",
      assertSingleItem: Boolean = true,
      filter: String => Boolean = _ => true,
      command: Option[String] = None,
      compat: Map[String, String] = Map.empty
  )(implicit loc: Location): Unit = {
    val compatTemplate = compat.map { case (key, value) =>
      key -> template.replace("___", value)
    }
    checkEdit(
      name = name,
      original = template.replace("___", original),
      expected = template.replace("___", expected),
      filterText = filterText,
      assertSingleItem = assertSingleItem,
      filter = filter,
      command = command,
      compat = compatTemplate
    )
  }

  /**
   * Check the results of applying the first completion suggested at a cursor position
   *     indicated by `@@`.
   *
   * @param name name of the test
   * @param original snippet to test with `@@` indicating the cursor position
   * @param expected snippet after applying the first completion
   * @param filterText filter returned completions according to text
   * @param assertSingleItem make sure only one item is suggested, true by default
   * @param filter similar to filterText, but uses a function
   * @param command additional command that should be applied after this completion is inserted
   * @param compat additional compatibility map for different Scala versions
   */
  def checkEdit(
      name: TestOptions,
      original: String,
      expected: String,
      filterText: String = "",
      assertSingleItem: Boolean = true,
      filter: String => Boolean = _ => true,
      command: Option[String] = None,
      compat: Map[String, String] = Map.empty,
      itemIndex: Int = 0,
      filename: String = "A.scala"
  )(implicit loc: Location): Unit = {
    test(name) {
      val items =
        getItems(original, filename).filter(item => filter(item.getLabel))
      if (items.isEmpty) fail("obtained empty completions!")
      if (assertSingleItem && items.length != 1) {
        fail(
          s"expected single completion item, obtained ${items.length} items.\n${items}"
        )
      }
      if (items.size <= itemIndex) fail("Not enough completion items")
      val item = items(itemIndex)
      val (code, _) = params(original, filename)
      val obtained = TextEdits.applyEdits(code, item)
      assertNoDiff(
        obtained,
        getExpected(expected, compat, scalaVersion)
      )
      if (filterText.nonEmpty) {
        assertNoDiff(item.getFilterText, filterText, "Invalid filter text")
      }
      assertNoDiff(
        Option(item.getCommand).fold("")(_.getCommand),
        command.getOrElse(""),
        "Invalid command"
      )
    }
  }

  /**
   * Check snippet syntax returned in the completions. Snippets show the editor where the cursor should end up ($0).
   *
   * @param name name of the test
   * @param original snippet to test with `@@` indicating the cursor position
   * @param expected string with a list of obtained completions
   * @param compat additional compatibility map for different Scala versions
   */
  def checkSnippet(
      name: TestOptions,
      original: String,
      expected: String,
      compat: Map[String, String] = Map.empty,
      topLines: Option[Int] = None,
      includeDetail: Boolean = false
  )(implicit loc: Location): Unit = {
    test(name) {
      val baseItems = getItems(original)
      val items = topLines match {
        case Some(top) => baseItems.take(top)
        case None => baseItems
      }
      val obtained = items
        .map { item =>
          val results = item
            .getLeftTextEdit()
            .map(_.getNewText)
            .orElse(Option(item.getInsertText()))
            .getOrElse(item.getLabel)
          if (includeDetail) results + " - " + item.getDetail()
          else results
        }
        .mkString("\n")
      assertNoDiff(
        obtained,
        getExpected(expected, compat, scalaVersion)
      )
    }
  }

  /**
   * Check completions that will be shown in original param after `@@` marker
   * correspoding to the cursor positions
   * @param name test name
   * @param original snippet to test with `@@` indicating the cursor position, by default wrapped in package
   * @param expected expected list of completions
   * @param includeDocs whether to include documentation in the completion description
   * @param includeCommitCharacter  show commit characters, which when typed will
   *    indicate that completion is accepted.
   * @param compat additional compatibility map for different Scala versions
   * @param postProcessObtained function used to modify resulting completion list
   * @param stableOrder we should not sort completions if set to true
   * @param postAssert additional assertions to make on the results
   * @param topLines a number of completions to include, by default all
   * @param filterText filter returned completions according to text
   * @param includeDetail include the completion detail in the results, true by default
   * @param filename name of the file to run the test on, `A.scala` by default
   * @param filter similar to filterText, but uses a function
   * @param enablePackageWrap whether to wrap the code in a package, true by default
   */
  def check(
      name: TestOptions,
      original: String,
      expected: String,
      includeDocs: Boolean = false,
      includeCommitCharacter: Boolean = false,
      compat: Map[String, String] = Map.empty,
      postProcessObtained: String => String = identity,
      stableOrder: Boolean = true,
      topLines: Option[Int] = None,
      filterText: String = "",
      includeDetail: Boolean = true,
      filename: String = "A.scala",
      filter: String => Boolean = _ => true,
      enablePackageWrap: Boolean = true
  )(implicit loc: Location): Unit = {
    test(name) {
      val out = new StringBuilder()
      val withPkg =
        if (original.contains("package") || !enablePackageWrap) original
        else s"package ${scala.meta.Term.Name(name.name)}\n$original"
      val baseItems = getItems(withPkg, filename)
      val items = topLines match {
        case Some(top) => baseItems.take(top)
        case None => baseItems
      }
      val filteredItems = items.filter(item => filter(item.getLabel))
      filteredItems.foreach { item =>
        val label = TestCompletions.getFullyQualifiedLabel(item)
        val commitCharacter =
          if (includeCommitCharacter)
            Option(item.getCommitCharacters)
              .getOrElse(Collections.emptyList())
              .asScala
              .mkString(" (commit: '", " ", "')")
          else ""
        val documentation = doc(item.getDocumentation)
        if (includeDocs && documentation.nonEmpty) {
          out.append("> ").append(documentation).append("\n")
        }
        out
          .append(label)
          .append({
            val detailIsDefined = Option(item.getDetail).isDefined
            if (
              includeDetail && detailIsDefined && !item.getLabel
                .contains(item.getDetail)
            ) {
              item.getDetail
            } else {
              ""
            }
          })
          .append(commitCharacter)
          .append("\n")
      }
      assertNoDiff(
        sortLines(
          stableOrder,
          postProcessObtained(trimTrailingSpace(out.toString()))
        ),
        sortLines(
          stableOrder,
          getExpected(expected, compat, scalaVersion)
        )
      )
      if (filterText.nonEmpty) {
        filteredItems.foreach { item =>
          assertNoDiff(
            item.getFilterText,
            filterText,
            s"Invalid filter text for item:\n$item"
          )
        }
      }
    }
  }

  private def trimTrailingSpace(string: String): String = {
    string.linesIterator
      .map(_.replaceFirst("\\s++$", ""))
      .mkString("\n")
  }

  override val compatProcess: Map[String, String => String] = Map(
    "2.13" -> { s =>
      s.replace("equals(obj: Any)", "equals(obj: Object)")
        .replace(
          "singletonList[T](o: T)",
          "singletonList[T <: Object](o: T)"
        )
    }
  )

}
