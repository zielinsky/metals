package scala.meta.internal.pc

import javax.lang.model.element.Element

import scala.jdk.CollectionConverters.SeqHasAsJava

import scala.meta.internal.jpc.JavaMetalsCompiler
import scala.meta.internal.jpc.SemanticdbSymbol
import scala.meta.pc.OffsetParams
import scala.meta.pc.ReferencesRequest
import scala.meta.pc.ReferencesResult

import com.sun.source.tree.CompilationUnitTree
import com.sun.source.util.Trees
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Range

class JavaReferencesProvider(
    params: ReferencesRequest,
    compiler: JavaMetalsCompiler
) {

  def references(): List[ReferencesResult] = {
    params.offsetOrSymbol() match {
      case either if either.isLeft() =>
        val offsetParams =
          new OffsetParams {
            def uri() = params.file().uri()
            def text() = params.file().text()
            def offset() = either.getLeft()
            def token() = params.file().token()
          }
        if (!isWhitespace(offsetParams)) {
          val refs = findReferences(offsetParams)
          if (refs.locations.isEmpty()) Nil
          else List(refs)
        } else {
          Nil
        }
      case either if either.isRight() =>
        val symbol = either.getRight()
        val refs = findReferences(symbol, params)
        if (refs.locations.isEmpty()) Nil
        else List(refs)
      case _ =>
        Nil
    }
  }

  private def isWhitespace(params: OffsetParams): Boolean = {
    params.offset() < 0 ||
    params.offset() >= params.text().length ||
    params.text().charAt(params.offset()).isWhitespace
  }

  private def findReferences(
      symbol: String,
      params: ReferencesRequest
  ): PcReferencesResult = {
    val compile = compiler
      .compilationTask(params.file())
      .withAnalyzePhase()
    val task = compile.task
    val trees = Trees.instance(task)
    val symbolScanner = new SymbolReferencesScanner(
      symbol,
      trees,
      compile.cu,
      params.file().text(),
      params.file().uri().toString(),
      params.includeDefinition()
    )
    symbolScanner.scan(compile.cu, null)
    PcReferencesResult(
      symbol,
      symbolScanner
        .result()
        .reverse
        .distinctBy(_.getRange().toString())
        .asJava
    )
  }

  private def findReferences(
      offsetParams: OffsetParams
  ): PcReferencesResult = {
    val node = compiler.nodeAtPosition(offsetParams, forReferences = true)

    node match {
      case Some((compile, treePath)) =>
        val trees = Trees.instance(compile.task)
        val element = trees.getElement(treePath)

        def atIdentifier = compiler.isAtIdentifier(
          treePath,
          element,
          offsetParams.text(),
          offsetParams.offset(),
          trees,
          compile.cu
        )
        if (element != null && atIdentifier) {
          findAllReferences(
            compile.cu,
            element,
            trees,
            offsetParams.text(),
            offsetParams.uri().toString()
          )
        } else {
          PcReferencesResult.empty.asInstanceOf[PcReferencesResult]
        }
      case None => PcReferencesResult.empty.asInstanceOf[PcReferencesResult]
    }
  }

  private def findAllReferences(
      root: CompilationUnitTree,
      targetElement: Element,
      trees: Trees,
      text: String,
      uri: String
  ): PcReferencesResult = {
    val scanner =
      new ReferencesScanner(
        targetElement,
        trees,
        root,
        text,
        uri,
        params.includeDefinition()
      )
    scanner.scan(root, null)

    val symbol = SemanticdbSymbol.fromElement(targetElement)
    PcReferencesResult(
      symbol,
      scanner.result().reverse.distinctBy(_.getRange().toString()).asJava
    )
  }

  private class ReferencesScanner(
      targetElement: Element,
      trees: Trees,
      root: CompilationUnitTree,
      text: String,
      uri: String,
      includeDefinition: Boolean
  ) extends ReferenceScanner[Location](
        element => element.equals(targetElement),
        trees,
        root,
        text,
        compiler,
        includeDefinition
      ) {
    override protected def createElement(
        range: Range,
        isDefinition: Boolean
    ): Location = {
      new Location(
        uri,
        range
      )
    }
  }
  private class SymbolReferencesScanner(
      targetElement: String,
      trees: Trees,
      root: CompilationUnitTree,
      text: String,
      uri: String,
      includeDefinition: Boolean
  ) extends ReferenceScanner[Location](
        element => SemanticdbSymbol.fromElement(element).equals(targetElement),
        trees,
        root,
        text,
        compiler,
        includeDefinition
      ) {
    override protected def createElement(
        range: Range,
        isDefinition: Boolean
    ): Location = {
      new Location(
        uri,
        range
      )
    }
  }
}
