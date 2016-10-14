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

case class CFGProduction[ChildSymbols <: HList, Children <: HList, Parent](
  children: ChildSymbols,
  parent: ParseSymbol[Parent]
)(implicit val comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children])

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
    def to[Parent](symb: ParseSymbol[Parent]) = CFGProduction(childrenTuple.productElements, symb)
  }

  implicit class symbol2Child[Child](childSymbol: ParseSymbol[Child]) {
    def to[Parent](symb: ParseSymbol[Parent]) = CFGProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent](childSymbol :: HNil, symb)
  }

  implicit class ProductionEnhancer[
    ChildSymbols <: HList,
    Children <: HList,
    Parent](
    production: CFGProduction[ChildSymbols, Children, Parent])(
    implicit comapped: Comapped.Aux[ChildSymbols, ParseSymbol, Children]
  ) {
    def using(construct: PartialFunction[Children, OrderedStream[Scored[Parent]]]): SyncProduction[ChildSymbols, Children, Parent] =
      new SyncProduction(production, construct)
  }

  implicit class TerminalInterpolator(val sc: StringContext) extends AnyVal {
    def t(args: Any*) = Terminal(sc.s(args: _*))
  }
}

case class SyncCFG[AllProductions <: HList : <<:[SyncCFGProduction]#λ](productions: AllProductions)
