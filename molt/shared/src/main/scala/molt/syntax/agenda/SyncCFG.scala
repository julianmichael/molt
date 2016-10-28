package molt
package syntax
package agenda

import scala.annotation.tailrec

import scalaz.Heap

import shapeless._
import UnaryTCConstraint._
import LUBConstraint._
import ops.hlist._

import ordered._

sealed abstract class CFGProduction[ChildSymbols <: HList, Children <: HList, Parent]()(
  implicit val comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children]
) {
  def children: ChildSymbols
  def parent: ParseSymbol[Parent]
}
case class GeneralCFGProduction[ChildSymbols <: HList, Children <: HList, Parent](
  override val children: ChildSymbols,
  override val parent: ParseSymbol[Parent])(
  override implicit val comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children]
) extends CFGProduction[ChildSymbols, Children, Parent]()(comapped) {
  def using(construct: PartialFunction[Children, OrderedStream[Scored[Parent]]]): SyncProduction[ChildSymbols, Children, Parent] =
    new SyncProduction(this, construct)
  def usingSingle(construct: PartialFunction[Children, Scored[Parent]]): SyncProduction[ChildSymbols, Children, Parent] =
    new SyncProduction(this, construct andThen { case x => OrderedStream.unit(x) })
  def usingSingleZ(construct: PartialFunction[Children, Parent]): SyncProduction[ChildSymbols, Children, Parent] =
    new SyncProduction(this, construct andThen { case x => OrderedStream.unit(Scored(x, 0.0)) })
}

// really only exists for nice syntax
case class UnaryCFGProduction[Child, Parent](
  val child: ParseSymbol[Child],
  override val parent: ParseSymbol[Parent]
) extends CFGProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent] {
  override val children = child :: HNil
  def using(construct: PartialFunction[Child, OrderedStream[Scored[Parent]]]): SyncProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent] =
    new SyncProduction(this, ({ case child :: HNil => child }: PartialFunction[Child :: HNil, Child]) andThen construct)
  def usingSingle(construct: PartialFunction[Child, Scored[Parent]]): SyncProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent] =
    new SyncProduction(this, ({ case child :: HNil => child }: PartialFunction[Child :: HNil, Child]) andThen construct andThen { case x => OrderedStream.unit(x)})
  def usingSingleZ(construct: PartialFunction[Child, Parent]): SyncProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent] =
    new SyncProduction(this, ({ case child :: HNil => child }: PartialFunction[Child :: HNil, Child]) andThen construct andThen { case x => OrderedStream.unit(Scored(x, 0.0))})
}

sealed trait SyncCFGProduction
class SyncProduction[
  ChildSymbols <: HList,
  Children <: HList,
  Parent](
  val production: CFGProduction[ChildSymbols, Children, Parent],
  val construct: PartialFunction[Children, OrderedStream[Scored[Parent]]]
)(implicit val comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children]) extends SyncCFGProduction {
  def childSymbols: ChildSymbols = production.children
  def parentSymbol: ParseSymbol[Parent] = production.parent
}

object SyncProductionSyntax {
  import syntax.std.tuple._
  implicit class tuple2Children[
    ChildrenTuple <: Product,
    ChildSymbols <: HList,
    Children <: HList](
    childrenTuple: ChildrenTuple)(
    implicit val gen: Generic.Aux[ChildrenTuple, ChildSymbols],
    comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children]) {
    def to[Parent](symb: ParseSymbol[Parent]) = GeneralCFGProduction[ChildSymbols, Children, Parent](childrenTuple.productElements, symb)
  }

  implicit class symbol2Child[Child](childSymbol: ParseSymbol[Child]) {
    def to[Parent](symb: ParseSymbol[Parent]) = UnaryCFGProduction[Child, Parent](childSymbol, symb)
  }

  implicit class TerminalInterpolator(val sc: StringContext) extends AnyVal {
    def t(args: Any*) = Terminal(sc.s(args: _*))
  }
}

case class SyncCFG[AllProductions <: HList : <<:[SyncCFGProduction]#λ](productions: AllProductions)

object GrammarHelpers {
  import SyncProductionSyntax._

  // best effort at simulating free monoid without introducing unnecessary ambiguity
  def plusMonoid[A](listSymbol: ParseSymbol[List[A]], consSymbol: ParseSymbol[A], appendSymbol: ParseSymbol[List[A]]) = {
    val append = (appendSymbol, listSymbol) to listSymbol usingSingleZ {
      case l1 :: l2 :: HNil => l1 ++ l2
    }
    append :: plusList(listSymbol, consSymbol)
  }
  def plusList[A](listSymbol: ParseSymbol[List[A]], consSymbol: ParseSymbol[A]
  )/*: SyncProduction[ParseSymbol[A] :: HNil, A :: HNil, List[A]] :: SyncProduction[ParseSymbol[A] :: ParseSymbol[List[A]] :: HNil, A :: List[A] :: HNil, List[A]] :: HNil */= {
    val unit = consSymbol to listSymbol usingSingleZ {
      case child => List(child)
    }
    val cons = (consSymbol, listSymbol) to listSymbol usingSingleZ {
      case head :: tail :: HNil => head :: tail
    }
    unit :: cons :: HNil
  }
}
