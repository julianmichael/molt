package parsing.cfg

import parsing.cnf._

// for any general (finite) production with a bunch of children
case class CFGProduction[+A](
  head: A,
  children: List[A]) {

  val toCNF: List[CNFProduction[A]] = {
    collapseProduction(NormalTag[A](head), children).toList
  }

}
object CFGProduction {
  def toCNF[A](prod: CFGProduction[A]): List[CNFProduction[A]] = prod match {
    case CFGProduction(head, children) =>
      collapseProduction(NormalTag[A](head), children).toList
  }

  private[this] def collapseProduction(
      label: CNFTag[A],
      symbols: List[A]): Set[CNFProduction[A]] = symbols match {
    case Nil                    => throw new AssertionError("Production's children are null")
    case Empty :: Nil           => Set(ZeroEmpty[A](label))
    case child :: Nil           => Set(Unary[A](NormalTag[A](head), NormalTag[A](child)))
    case Empty :: Empty :: Nil  => Set(DoubleEmpty[A](label))
    case left :: Empty :: Nil   => Set(RightEmpty[A](label, NormalTag[A](left)))
    case Empty :: right :: Nil  => Set(LeftEmpty[A](label, NormalTag[A](right)))
    case left :: right :: Nil   => Set(Binary[A](label, NormalTag[A](left), NormalTag[A](right)))
    case Empty :: remainder     => {
      collapseProduction(ChunkedTag[A](remainder), remainder)
      + LeftEmpty[A](label, ChunkedTag[A](remainder))
    }
    case left :: remainder      => {
      collapseProduction(ChunkedTag[A](remainder), remainder)
      + Binary[A](label, NormalTag[A](left), ChunkedTag[A](remainder))
    }
  }
}
