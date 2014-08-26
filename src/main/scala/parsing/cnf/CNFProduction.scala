package parsing.cnf

import parsing.cfg.ASTTag
import parsing.cfg.ASTNormalTag

sealed abstract class CNFProduction[A] {
  val tags: Set[ASTTag[A]] = this match {
    case Binary(label, left, right) =>
      Set(ASTNormalTag(label), left, right)
    case Unary(label, child) =>
      Set(ASTNormalTag(label), child)
  }
  val symbols: Set[A] = tags.collect {
    case ASTNormalTag(x) => x
  }
}
case class Binary[A](label: A, left: ASTTag[A], right: ASTTag[A]) extends CNFProduction[A]
case class Unary[A](label: A, child: ASTTag[A]) extends CNFProduction[A]
