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

case class CNFChunk[L <: HList, LSymbols <: HList](symbols: LSymbols)(
  implicit comapped: Comapped.Aux[LSymbols, ParseSymbol, L]
) extends ParseSymbol[L](s"$symbols")

sealed trait SyncCNFProduction
object SyncCNFProduction {
  case class Unary[Child, Parent](
    childSymbol: ParseSymbol[Child],
    parentSymbol: ParseSymbol[Parent],
    construct: PartialFunction[Child :: HNil, OrderedStream[Scored[Parent]]]) extends SyncCNFProduction
  case class Binary[Left, Right, Parent](
    leftChildSymbol: ParseSymbol[Left],
    rightChildSymbol: ParseSymbol[Right],
    parentSymbol: ParseSymbol[Parent],
    construct: PartialFunction[Left :: Right :: HNil, OrderedStream[Scored[Parent]]]) extends SyncCNFProduction
}
import SyncCNFProduction._

trait foldBigCFGProductionFallback extends Poly2 {
  // left is the next child on the production's children list.
  // right is (children already processed, CNF productions so far, original production)
  implicit def caseN[Next, SymbolsSoFar <: HList, ChildrenSoFar <: HList, ProdsSoFar <: HList, ChildSymbols <: HList, Children <: HList, Parent](
    implicit comapped: Comapped.Aux[SymbolsSoFar, ParseSymbol, ChildrenSoFar]
  ) =
    at[ParseSymbol[Next], (SymbolsSoFar, ProdsSoFar, SyncProduction[ChildSymbols, Children, Parent])] {
      case (nextSymbol, (symbolsSoFar, prodsSoFar, sp)) =>
        val newProd: Binary[Next, ChildrenSoFar, Next :: ChildrenSoFar] =
          Binary(
            nextSymbol, CNFChunk(symbolsSoFar), CNFChunk(nextSymbol :: symbolsSoFar), {
              case next :: childrenSoFar :: HNil => OrderedStream.unit(Scored(next :: childrenSoFar, 0.0))
            })
        (nextSymbol :: symbolsSoFar, newProd :: prodsSoFar, sp)
    }
}

object foldBigCFGProduction extends foldBigCFGProductionFallback {
  import SyncCNFProduction._
  implicit def case1[First, Parent, ChildSymbols <: HList, Children <: HList] =
    at[ParseSymbol[First], (HNil, HNil, SyncProduction[ChildSymbols, Children, Parent])] {
      case (firstSymbol, (_, _, sp)) => (firstSymbol :: HNil, HNil: HNil, sp)
    }
  implicit def case2[Left , Right, Parent, ChildSymbols <: HList, Children <: HList] =
    at[ParseSymbol[Left], (ParseSymbol[Right] :: HNil, HNil, SyncProduction[ChildSymbols, Children, Parent])] {
      case (leftSymbol, (rightSymbol :: HNil, HNil, sp)) =>
        val newProd: Binary[Left, Right, Left :: Right :: HNil] = Binary(
          leftSymbol, rightSymbol, CNFChunk(leftSymbol :: rightSymbol :: HNil), {
            case left :: right :: HNil => OrderedStream.unit(Scored(left :: right :: HNil, 0.0))
          })
        (leftSymbol :: rightSymbol :: HNil, newProd :: HNil, sp)
    }
  implicit def caseDone[Parent, ChildSymbols <: HList, Children <: HList, Productions <: HList] =
    at[None.type, (ChildSymbols, Productions, SyncProduction[ChildSymbols, Children, Parent])] {
      case (_, (childSymbols, productions, sp)) =>
        val resolvingProduction: Unary[Children, Parent] = Unary(
          CNFChunk(childSymbols)(sp.comapped), sp.parentSymbol, {
            case children :: HNil => sp.construct(children)
          })
        resolvingProduction :: productions
    }
}

trait transformProductionFallback extends Poly1 {
  implicit def caseNary[Children <: HList, ChildSymbols <: HList, Parent](
    implicit folder: RightFolder[None.type :: ChildSymbols, (HNil, HNil, SyncProduction[ChildSymbols, Children, Parent]), foldBigCFGProduction.type]) =
    at[SyncProduction[ChildSymbols, Children, Parent]] {
      prod => (None :: prod.childSymbols).foldRight((HNil: HNil, HNil: HNil, prod))(foldBigCFGProduction)
    }
}

object transformProduction extends transformProductionFallback {
  import SyncCNFProduction._
  implicit def caseUnary[Child, Parent] =
    at[SyncProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent]] { sp =>
      Unary(sp.childSymbols.head, sp.parentSymbol, sp.construct) :: HNil
    }
  implicit def caseBinary[Left, Right, Parent] =
    at[SyncProduction[ParseSymbol[Left] :: ParseSymbol[Right] :: HNil, Left :: Right :: HNil, Parent]] { sp =>
      Binary(sp.childSymbols.head, sp.childSymbols.tail.head, sp.parentSymbol, sp.construct) :: HNil
    }
}

case class SyncCNFGrammar[AllProductions <: HList : <<:[SyncCNFProduction]#λ](val productions: AllProductions)
object SyncCNFGrammar {
  def productionsFromSyncCFG[AllCFGProductions <: HList : <<:[SyncCFGProduction]#λ](
    syncCFG: SyncCFG[AllCFGProductions])(
    implicit fm: FlatMapper[transformProduction.type, AllCFGProductions]
  ) = syncCFG.productions.flatMap(transformProduction)
}
