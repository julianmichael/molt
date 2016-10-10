package molt
package syntax

/** This is a temporary package for my new approach to agenda-based parsing of CFGs */
package object agenda {

  // lol
  // type Any[T[_]] = T[_]

  import scala.language.higherKinds
  import shapeless._
  import UnaryTCConstraint._
  import ops.hlist._

  class ParseSymbol[+A]

  class SyncProduction[
    ChildSymbols <: HList,
    Children <: HList,
    Result](
    val production: (ChildSymbols, ParseSymbol[Result]),
    val construct: PartialFunction[Children, Stream[Result]]
  )(implicit comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children])
  type SyncProd = SyncProduction[_, _, _]

  object SyncProductionSyntax {
    import syntax.std.tuple._
    implicit class tuple2Children[
      ChildrenTuple <: Product,
      Children <: HList](
      childrenTuple: ChildrenTuple)(
      implicit val gen: Generic.Aux[ChildrenTuple, Children]) {
      def to[Parent](symb: ParseSymbol[Parent]) = (childrenTuple.productElements, symb)
    }

    implicit class ProductionEnhancer[
      ChildSymbols <: HList,
      Children <: HList,
      Result](
      production: (ChildSymbols, ParseSymbol[Result]))(
      implicit comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children]
    ) {
      def using(construct: PartialFunction[Children, Stream[Result]]): SyncProduction[ChildSymbols, Children, Result] =
        new SyncProduction(production, construct)
    }
  }

  import SyncProductionSyntax._

  // example of how to make productions
  case class ExpStr(x: String)
  case class ExpInt(i: Int)

  val StrSymb = new ParseSymbol[ExpStr]
  val StrSymb2 = new ParseSymbol[ExpStr]
  val IntSymb = new ParseSymbol[ExpInt]

  val prod2 = (StrSymb, IntSymb) to StrSymb using {
    case ExpStr(s) :: ExpInt(i) :: HNil => Stream(ExpStr(s))
  }

  trait Cell {
    def getSymbols: Set[ParseSymbol[_]]
    def getNodes[A](symbol: ParseSymbol[A]): Stream[A]
  }

  case class SyncCFG(productions: List[SyncProd])

  class AgendaBasedSyncCFGParser(
    val genlex: List[String => Cell],
    val syncCFG: SyncCFG
  ) {

  }

  // sad futile endeavor
  // object CFGParsableAdaptation {
  //   import molt.syntax.cfg.parsable._
  //   import molt.syntax.cfg._
  //   import shapeless.syntax.typeable._

  //   import scala.language.implicitConversions
  //   import scalaz._
  //   import scalaz.std.list._
  //   import scalaz.std.option._
  //   import scalaz.syntax.traverse._

  //   implicit def convSyncProd[
  //     ChildSymbols <: HList,
  //     Children <: HList : Typeable,
  //     Result](
  //     sp: SyncProduction[ChildSymbols, Children, Result])(
  //     implicit ev1: ToList[ChildSymbols, CFGParsable[_]]): (List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[Result])) = {
  //     (sp.production._1.toList,
  //      ((c: List[AST[CFGParsable[_]]]) => for {
  //         childrenList <- sp.production._1.toList.zip(c).map {
  //           case (parsable, ast) => parsable.fromAST(ast)
  //         }.sequence
  //         children <- childrenList.cast[Children]
  //         result <- sp.construct.lift(children)
  //       } yield result))
  //   }

  //   example intended use (DOESN't WORK BECAUSE DUMBNESS):
  //   import molt.syntax.agenda._
  //   import molt.syntax.agenda.SyncProductionSyntax._
  //   import shapeless._
  //   val syncProduction = convSyncProd(
  //     (NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)) to CFGProductionParser using {
  //       case (head: String) :: "->" :: (children: List[String]) :: HNil => CFGProduction(head, children.map(ASTNormalTag(_)))
  //     })
  // }
}

