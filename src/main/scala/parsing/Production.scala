package parsing

// for any general (finite) production with a bunch of children
case class Production[+A](
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
case object Production extends ComplexParsable[Production[String]] {
  case object NonterminalSymbol extends ParsableLexicalCategory(w => w != "->")
  val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[Production[String]])] = Map(
    List(NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)) -> {
      (c => for {
        head <- NonterminalSymbol.fromAST(c(0))
        children <- Plus(NonterminalSymbol).fromAST(c(2))
      } yield Production(head, children))
    })
}

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