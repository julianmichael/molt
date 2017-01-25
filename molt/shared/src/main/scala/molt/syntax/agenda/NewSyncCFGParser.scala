package molt
package syntax
package agenda

import scala.annotation.tailrec

import scalaz.Heap

import shapeless._
import UnaryTCConstraint._
import LUBConstraint._
import NotContainsConstraint._
import ops.hlist._

import ordered._

import monocle._
import monocle.macros._

case class CNFChunk[L <: HList, LSymbols <: HList](symbols: LSymbols)(
  implicit comapped: Comapped.Aux[LSymbols, ParseSymbol, L]
) extends ParseSymbol[L](s"$symbols")
object CNFChunk {
  object breakDownChunkFallback extends Poly1 {
    // implicit def caseN
  }
  object breakDownChunk extends breakDownChunkFallback {
    // implicit def caseN
  }
  def getAllChunks[L <: HList, LSymbols <: HList](chunk: CNFChunk[L, LSymbols]): Set[CNFChunk[_, _]] = {
    chunk.symbols.tails.map(breakDownChunk).toList.reduce(_ union _)
  }
}

@Lenses case class PreCNFCombinators(
  val nullary: NullaryCombinator,
  val unary: DependentMap[ParseSymbol, λ[C => UnaryCombinator[C]]],
  val chunks: Set[CNFChunk[_, _]]) {

  def withUnary[Child, Parent](sp: SyncCFGProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent]): PreCNFCombinators = {
    val childSymbol = sp.childSymbols.head
    val unaryOpt = combinators.unary.get(childSymbol)
    val newUnary = unaryOpt.fold(UnaryCombinator(childSymbol, Vector(sp)))(UnaryCombinator.productions.modify(sp +: _))
    this.copy(unary = combinators.unary.put(childSymbol, newUnary))
  }
  def withChunks(cnfChunks: Set[CNFChunk[_, _]]): PreCNFCombinators = {
    this.copy(chunks = this.chunks union cnfChunks)
  }
}

@Lenses case class NullaryCombinator(
  val productions: Vector[SyncCFGProduction[HNil, HNil, _]]
) extends CNFCombinator {
  val derivations: ScoredStream[Derivation] = productions
      .map(p => p.construct.map(Derivation(p.parentSymbol.asInstanceOf[ParseSymbol[Any]], _)))
      .foldLeft(ScoredStream.empty[Derivation])(_ merge _)
}

@Lenses case class UnaryCombinator[Child](
  val childSymbol: ParseSymbol[Child],
  val productions: Vector[SyncCFGProduction[ParseSymbol[Child] :: HNil, Child :: HNil, _]]
) {
  def apply(d: Derivation { type Result = Child }): ScoredStream[Derivation] = d match {
    case Derivation(`childSymbol`, child) =>
      val vecOfStreams = for {
        p <- productions
        scoredResults <- p.construct.lift(child :: HNil).toVector
      } yield scoredResults.map(Derivation(p.parentSymbol.asInstanceOf[ParseSymbol[Any]], _))
      vecOfStreams.foldLeft(ScoredStream.empty[Derivation])(_ merge _)
    case _ => ScoredStream.empty[Derivation]
  }
}

@Lenses case class BinaryCombinator[Left, Right](
  val leftSymbol: ParseSymbol[Left],
  val rightSymbol: ParseSymbol[Right],
  val productions: Vector[SyncCFGProduction[ParseSymbol[Left] :: ParseSymbol[Right] :: HNil, Left :: Right :: HNil, _]]
) extends BinaryCombinator {
  def apply(left: Derivation { type Result = Left }, right: Derivation { type Result = Right }): ScoredStream[Derivation] = (left, right) match {
    case (Derivation(`leftSymbol`, leftChild), Derivation(`rightSymbol`, rightChild)) =>
      val vecOfStreams = for {
        p <- productions
        scoredResults <- p.construct.lift(leftChild :: rightChild :: HNil)
      } yield scoredResults.map(
        // not really sure why we need this cast...sigh...
        result => Derivation(p.parentSymbol.asInstanceOf[ParseSymbol[Any]], result))
      vecOfStreams.foldLeft(ScoredStream.empty[Derivation])(_ merge _)
    case _ => ScoredStream.empty[Derivation]
  }
}

trait addCFGRuleFallback extends Poly2 {
  implicit def caseNary[Children <: HList, ChildSymbols <: HList, Parent] =
    at[SyncCFGProduction[ChildSymbols, Children, Parent], PreCNFCombinators] { case (sp, combinators) =>
      val chunkedChild = CNFChunk[Children, ChildSymbols](sp.childSymbols)
      val unaryAssemblyProd: SyncCFGProduction[CNFChunk[ChildSymbols] :: HNil, ChildSymbols :: HNil, Parent] = SyncCFGProduction(
        CFGProduction(chunkedChild :: HNil, sp.parentSymbol),
        { case childrenHList :: HNil if sp.construct.isDefinedAt(childrenHList) => sp.construct(childrenHList) })
      combinators.copy(
      )
    }
}

object addCFGRule extends Poly2 {
  implicit def caseNullary[Parent] =
    at[SyncCFGProduction[HNil, HNil, Parent], PreCNFCombinators] { case (sp, combinators) =>
      (PreCNFCombinators.nullary composeLens NullaryCombinator.productions).modify(sp +: _)(combinators)
    }
  implicit def caseUnary[Child, Parent] =
    at[SyncCFGProduction[ParseSymbol[Child] :: HNil, Child :: HNil, Parent]] { case (sp, combinators) =>
      val childSymbol = sp.childSymbols.head
      val unaryOpt = combinators.unary.get(childSymbol)
      val newUnary = unaryOpt.fold(UnaryCombinator(childSymbol, Vector(sp)))(UnaryCombinator.productions.modify(sp +: _))
      combinators.copy(unary = combinators.unary.put(childSymbol, newUnary))
    }
}

object addCFGRule extends Poly2 {
  implicit def caseNullary[Parent] =
    at[Nullary[Parent], CNFCombinators] { case (n @ Nullary(_, _), combinators) =>
      val newProds = n +: combinators.nullary.productions
      val newNullary = combinators.nullary.copy(productions = newProds)
      combinators.copy(nullary = newNullary)
    }
  implicit def caseUnary[Child, Parent] =
    at[Unary[Child, Parent], CNFCombinators] { case (u @ Unary(childSymbol, _, _), combinators) =>
      val curProductions = combinators.unary.get(childSymbol).map(_.productions).getOrElse(Vector.empty[Unary[Child, _]])
      val newUnary = combinators.unary.put(childSymbol, UnaryCombinator(childSymbol, u +: curProductions))
      combinators.copy(unary = newUnary)
    }
  implicit def caseBinary[L, R, Parent] =
    at[Binary[L, R, Parent], CNFCombinators] { case (b @ Binary(leftSymbol, rightSymbol, _, _), combinators) =>
      val rightBinaries = combinators.leftThenRightBinary.get(leftSymbol)
        .getOrElse(DependentMap.empty[ParseSymbol, λ[R => BinaryCombinator { type Left = L; type Right = R }]])
      val leftBinaries = combinators.rightThenLeftBinary.get(rightSymbol)
        .getOrElse(DependentMap.empty[ParseSymbol, λ[L => BinaryCombinator { type Left = L; type Right = R }]])
      // could also do with leftBinaries; doesn't matter since contents are the same (just indexed differently)
      val binaryCombinator = rightBinaries.get(rightSymbol)
        .getOrElse(BinaryCombinator(leftSymbol, rightSymbol, Vector.empty))

      val newLeftBinaries  =  leftBinaries.put(leftSymbol,  BinaryCombinator(leftSymbol, rightSymbol, b +: binaryCombinator.productions))
      val newRightBinaries = rightBinaries.put(rightSymbol, BinaryCombinator(leftSymbol, rightSymbol, b +: binaryCombinator.productions))
      val newLeftThenRightBinary = combinators.leftThenRightBinary.put(leftSymbol, newRightBinaries)
      val newRightThenLeftBinary = combinators.rightThenLeftBinary.put(rightSymbol, newLeftBinaries)
      combinators.copy(
        leftThenRightBinary = newLeftThenRightBinary,
        rightThenLeftBinary = newRightThenLeftBinary)
    }
}

case class CNFCombinators(
  val nullary: NullaryCombinator,
  val unary: DependentMap[ParseSymbol, λ[C => UnaryCombinator[C]]],
  val leftThenRightBinary: DependentMap[ParseSymbol, λ[L => DependentMap[ParseSymbol, λ[R => BinaryCombinator[L, R]]]]],
  val rightThenLeftBinary: DependentMap[ParseSymbol, λ[R => DependentMap[ParseSymbol, λ[L => BinaryCombinator[L, R]]]]])

object CNFCombinators {
  val empty = CNFCombinators(
    nullary = NullaryCombinator(Vector()),
    unary = DependentMap.empty[ParseSymbol, λ[C => UnaryCombinator[C]]],
    leftThenRightBinary = DependentMap.empty[ParseSymbol, λ[L => DependentMap[ParseSymbol, λ[R => BinaryCombinator[L, R]]]]],
    rightThenLeftBinary = DependentMap.empty[ParseSymbol, λ[R => DependentMap[ParseSymbol, λ[L => BinaryCombinator[L, R]]]]])
  def fromSyncCFGProductions[AllProductions <: HList : <<:[SyncCFGProduction[_, _, _]]#λ](
    cfgProductions: AllProductions)(
    implicit folder: RightFolder.Aux[AllProductions, CNFCombinators, addCFGRule.type, CNFCombinators]
  ): CNFCombinators = {
    val chunks = cfgProductions.map(
      prod => {
        val symbols = prod.childSymbols
        val numChildren = symbols.length
        if(numChildren > 1)
      }
    )
    // cnfProductions.foldRight(empty)(addCNFRule)
  }
}
