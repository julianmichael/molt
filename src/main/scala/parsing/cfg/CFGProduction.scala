package parsing.cfg

import parsing.cnf._

case class CFGProduction[+A](head: A, children: List[ASTTag[A]])

object CFGProduction {
  def toCNF[A](prod: CFGProduction[A]): List[CNFProduction[A]] = prod match {
    case CFGProduction(head, children) =>
      collapseProduction(CNFNormalTag[A](head), children).toList
  }

  def convertTag[A](tag: ASTTag[A]): CNFTag[A] = tag match {
    case ASTNormalTag(x) => CNFNormalTag(x)
    case ASTEmptyTag => CNFEmptyTag
  }

  private[this] def collapseProduction[A](
      label: CNFTag[A],
      symbols: List[ASTTag[A]]): Set[CNFProduction[A]] = symbols match {
    case Nil                    => throw new AssertionError(s"empty production: label $label")
    case child :: Nil           => Set(Unary(label, convertTag(child)))
    case left :: right :: Nil   => Set(Binary(label, convertTag(left), convertTag(right)))
    case left :: remainder      => {
      collapseProduction(CNFChunkedTag(remainder), remainder) +
          Binary(label, convertTag(left), CNFChunkedTag(remainder))
    }
  }
}

sealed abstract class ASTTag[+A]
case class ASTNormalTag[+A](label: A) extends ASTTag[A]
case object ASTEmptyTag extends ASTTag[Nothing]
