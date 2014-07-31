package parsing.cnf

import parsing._
import parsing.cfg._

sealed abstract class CNFAST[+A] {
  val flattened: List[CNFAST[A]] = this match {
    case CNFBinaryNonterminal(CNFChunkedTag(_), left, right) => left :: right.flattened
    case CNFHole(CNFChunkedTag(names)) => names.map(n => CNFHole(CFGProduction.convertTag(n)))
    case _                             => this :: Nil
  }
  lazy val dechomskify: Option[AST[A]] = this match {
    case CNFHole(CNFChunkedTag(_)) => None
    case CNFBinaryNonterminal(CNFChunkedTag(_), _, _) => None

    case CNFEmpty => Some(ASTEmpty)
    case CNFHole(CNFNormalTag(head)) => Some(ASTHole(head))
    case CNFBinaryNonterminal(CNFNormalTag(head), left, right) => {
      val children = (left :: right.flattened).map(_.dechomskify).flatten
      Some(ASTNonterminal(head, children))
    }
    case CNFUnaryNonterminal(CNFNormalTag(head), child) => {
      val children = child.flattened.map(_.dechomskify).flatten
      Some(ASTNonterminal[A](head, children))
    }
    case CNFTerminal(CNFNormalTag(head), token) => {
      Some(ASTTerminal[A](head, token))
    }
  }

  def attachAtHole[B >: A](subtree: CNFAST[B]): Option[CNFAST[B]] = this match {
    case CNFBinaryNonterminal(label, left, right) => {
      val leftTry = left.attachAtHole(subtree)
      if(!leftTry.isEmpty) Some(CNFBinaryNonterminal(label, leftTry.get, right))
      else for {
        rightTry <- right.attachAtHole(subtree)
      } yield CNFBinaryNonterminal(label, left, rightTry)
    }
    case CNFUnaryNonterminal(label, child) => for {
      childTry <- child.attachAtHole(subtree)
    } yield CNFUnaryNonterminal(label, childTry)
    case CNFHole(label) if label == subtree.label => Some(subtree)
    case _ => None
  }

  def prettyString: String = this match {
    case CNFBinaryNonterminal(head, left, right) => {
      val childString = List(left, right).flatMap(child => {
        child.prettyString.split("\n").map(x => s"  $x")
      }).mkString("\n")
      s"$head\n$childString"
    }
    case CNFUnaryNonterminal(head, child) => {
      val childString = child.prettyString.split("\n").map(x => s"  $x").mkString("\n")
      s"$head\n$childString"
    }
    case CNFTerminal(head, token) => s"$head  $token"
    case CNFHole(head) => s"$head"
    case CNFEmpty => "<e>"
  }

  val label: CNFTag[A]
}

case class CNFBinaryNonterminal[A](
  override val label: CNFTag[A]/*CNFNonemptyTag[A]*/, left: CNFAST[A], right: CNFAST[A]) extends CNFAST[A]
case class CNFUnaryNonterminal[A](
  override val label: CNFTag[A]/*CNFNormalTag[A]*/, child: CNFAST[A]) extends CNFAST[A]
case class CNFHole[A](
  override val label: CNFTag[A]/*CNFNonemptyTag[A]*/) extends CNFAST[A]
case class CNFTerminal[A](
  override val label: CNFTag[A]/*CNFNormalTag[A]*/, token: String) extends CNFAST[A]
case object CNFEmpty extends CNFAST[Nothing] {
  override val label = CNFEmptyTag
}
