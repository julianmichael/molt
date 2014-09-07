package parsing.cnf

import parsing._
import parsing.cfg._

sealed abstract class CNFAST[+A] {
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
    case CNFHole(label) if ASTNormalTag(label) == subtree.tag => Some(subtree)
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
    case CNFEmpty() => "<e>"
  }

  def isNullParse: Boolean = this match {
    case CNFBinaryNonterminal(_, left, right) => left.isNullParse && right.isNullParse
    case CNFUnaryNonterminal(_, child) => child.isNullParse
    case CNFHole(_) => false
    case CNFTerminal(_, _) => false
    case CNFEmpty() => true
  }

  val tag: ASTTag[A] = this match {
    case CNFBinaryNonterminal(label, _, _) => ASTNormalTag(label)
    case CNFUnaryNonterminal(label, _) => ASTNormalTag(label)
    case CNFHole(label) => ASTNormalTag(label)
    case CNFTerminal(label, _) => ASTNormalTag(label)
    case CNFEmpty() => ASTEmptyTag
  }
}

case class CNFBinaryNonterminal[A](
  label: A, left: CNFAST[A], right: CNFAST[A]) extends CNFAST[A]
case class CNFUnaryNonterminal[A](
  label: A, child: CNFAST[A]) extends CNFAST[A]
case class CNFHole[A](
  label: A) extends CNFAST[A]
case class CNFTerminal[A](
  label: A, token: String) extends CNFAST[A]
case class CNFEmpty[A]() extends CNFAST[A]
