package parsing.cfg

import parsing.cnf._

// for any general (finite) production with a bunch of children
case class CFGProduction[+A](
  head: A,
  children: List[A]) {

  val toCNF: List[CNFProduction[A]] = {
    collapseProduction(NormalTag[A](head), children).toList
  }

  private[this] def collapseProduction(label: CNFTag[A], symbols: List[A]): Set[CNFProduction[A]] = {
    symbols match {
      case Nil                  => throw new AssertionError("Production's children are null")
      case child :: Nil         => Set(Unary[A](NormalTag[A](head), NormalTag[A](child)))
      case left :: right :: Nil =>
        Set(Binary[A](label, NormalTag[A](left), NormalTag[A](right)))
      case left :: remainder    => {
        collapseProduction(ChunkedTag[A](remainder), remainder) + Binary[A](label, NormalTag[A](left), ChunkedTag[A](remainder))
      }
    }
  }
}
