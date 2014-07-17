package parsing.cnf

// for all productions guaranteed to be in Chomsky Normal Form
// plus unary productions
sealed abstract class CNFProduction[+A]
case class Binary[A](
  label: CNFTag[A],
  left: CNFTag[A],
  right: CNFTag[A])
  extends CNFProduction[A]
case class Unary[A](
  label: CNFTag[A],
  child: CNFTag[A])
  extends CNFProduction[A] {
}

sealed abstract class CNFTag[+A]
case class NormalTag[A](label: A) extends CNFTag[A]
case class ChunkedTag[A](labels: List[A]) extends CNFTag[A]
