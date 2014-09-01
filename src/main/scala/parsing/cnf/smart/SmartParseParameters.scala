package parsing.cnf.smart

import parsing.cfg.CNFConversionTag
import parsing.cnf._
import CNFConversionTag._

trait SmartParseParameters[A] {
  type SyntaxTree = A
  implicit val ordering: Ordering[SyntaxTree]
}

class BasicSmartParse[A] extends SmartParseParameters[CNFAST[A]] {
  def score(tree: CNFAST[A]): Int = tree match {
    case CNFEmpty() => 1
    case CNFTerminal(_, _) => 1
    case CNFHole(_) => 1
    case CNFUnaryNonterminal(_, child) => 1 + score(child)
    case CNFBinaryNonterminal(_, left, right) => 1 + score(left) + score(right)
  }
  implicit val ordering: Ordering[SyntaxTree] = Ordering.by[SyntaxTree, Int](score _)
}

/*
trait SmartParseParameters[A] {
  type SyntaxTree = A
  type ScoredRepresentation
  type Score
  implicit val scoreOrdering: Ordering[Score]
  def decorate(tree: SyntaxTree): ScoredRepresentation
  def score(rep: IntermediateRepresentation): Score
}

trait BasicSmartParse[A] extends SmartParseParameters[CNFAST[CNFConversionTag[A]]] {
  type IntermediateRepresentation = (SyntaxTree, Int)
  type Score = Int
  implicit val scoreOrdering = implicitly[Ordering[Score]]
  def decorate(tree: SyntaxTree) = (tree, count(tree))
  def score(rep: IntermediateRepresentation): Int = rep._2

  def count(tree: CNFAST[A]): Int = tree match {
    case CNFEmpty() => 1
    case CNFTerminal(_, _) => 1
    case CNFHole(_) => 1
    case CNFUnaryNonterminal(_, child) => 1 + score(child)
    case CNFBinaryNonterminal(_, left, right) => 1 + score(left) + score(right)
  }
}
*/