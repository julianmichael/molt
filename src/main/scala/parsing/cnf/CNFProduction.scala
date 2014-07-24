package parsing.cnf

import parsing.cfg.ASTTag

sealed abstract class CNFProduction[A] {
  val symbols: Set[CNFTag[A]] = this match {
    case Binary(label, left, right) => Set(label, left, right)
    case Unary(label, child) => Set(label, child)
  }
}
case class Binary[A](
  label: CNFTag[A],//CNFNonemptyTag[A],
  left: CNFTag[A],//CNFUnchunkedTag[A],
  right: CNFTag[A]) extends CNFProduction[A]
case class Unary[A](
  label: CNFTag[A],//CNFNormalTag[A],
  child: CNFTag[A]/*CNFUnchunkedTag[A]*/) extends CNFProduction[A]

sealed trait CNFTag[+A]
sealed trait CNFNonemptyTag[+A] extends CNFTag[A]
sealed trait CNFUnchunkedTag[+A] extends CNFTag[A]

// todo CNFChunkedTag should always have >= 2 labels
case class CNFChunkedTag[+A](labels: List[ASTTag[A]]) extends CNFNonemptyTag[A]
case class CNFNormalTag[+A](label: A) extends CNFNonemptyTag[A] with CNFUnchunkedTag[A]
case object CNFEmptyTag extends CNFUnchunkedTag[Nothing]
