package scala.meta.languageserver

import java.nio.file.Files
import scala.meta.AbsolutePath
import org.langmeta.internal.semanticdb.vfs.SemanticdbPaths
import scalafix.internal.util.EagerInMemorySemanticdbIndex
import scala.{meta => m}
import org.langmeta.internal.semanticdb.{schema => s}
import langserver.types._
import langserver.core.Connection
import com.typesafe.scalalogging.LazyLogging

class SymbolProvider(cwd: AbsolutePath, connection: Connection)
    extends LazyLogging {

  def symbolsForDocument(path: AbsolutePath): Seq[SymbolInformation] = {
    logger.info("Retrieving symbols")
    logger.info(path.toString)
    val target = AbsolutePath("target/scala-2.12/classes")(cwd)
    val dbPath =
      SemanticdbPaths.fromScala(path.toRelative(cwd)).toAbsolute(target)
    logger.info(s"dbPath: $dbPath")
    val bytes = Files.readAllBytes(dbPath.toNIO)
    val sdb = s.Database.parseFrom(bytes)
    val mdb = sdb.toDb(None)
    val index =
      EagerInMemorySemanticdbIndex(mdb, m.Sourcepath(Nil), m.Classpath(Nil))
    try {
      for {
        name <- index.names
        if name.isDefinition
        symbol = m.Symbol(name.symbol.syntax)
        if symbol.isInstanceOf[m.Symbol.Global]
        resolvedSymbol <- index.symbols.find(
          _.symbol.syntax == name.symbol.syntax)
        denotation = resolvedSymbol.denotation
        kind = symbolKind(denotation)
      } yield {
        SymbolInformation(
          name = denotation.name,
          kind = kind,
          location = Location(
            uri = path.toURI.toString,
            range = Range(Position(0, 0), Position(0, 0))
          ),
          containerName = Some(denotation.signature)
        )
      }
    } catch {
      case e: Exception =>
        logger.debug(e.getMessage)
        val sw = new java.io.StringWriter
        e.printStackTrace(new java.io.PrintWriter(sw));
        logger.debug(sw.toString)
        Nil
    }

  }

  def symbolKind(denotation: m.Denotation): Int = {
    import denotation._

    if (isParam || isTypeParam)
      0
    else if (isVal || isVar)
      SymbolKind.Variable
    else if (isDef)
      SymbolKind.Function
    else if (isPrimaryCtor || isSecondaryCtor)
      SymbolKind.Constructor
    else if (isClass || isObject)
      SymbolKind.Class
    else if (isTrait)
      SymbolKind.Interface
    else if (isPackage || isPackageObject)
      SymbolKind.Package
    else if (isType)
      SymbolKind.Namespace // Note: no type related symbol kind exists
    else
      0
  }

}
