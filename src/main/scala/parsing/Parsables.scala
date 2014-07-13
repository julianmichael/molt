package parsing

import parsing.ParserHelpers._

object Parsables {

  implicit object ProductionParser extends ComplexParsable[Production[String]] {

    // for convenience in ProductionParser
    val NonterminalSymbol = new ParsableLexicalCategory(w => w != "->")

    val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[Production[String]])] = Map(
      List(NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)) ->
        (c => for {
          head <- NonterminalSymbol.fromAST(c(0))
          children <- Plus(NonterminalSymbol).fromAST(c(2))
        } yield Production(head, children))
      )
  }
}
object GenericParsables {

  /**********
   * Parameterized parsables: implicitly created from other parsables! Awesome
   * stuff!
**********/

  class SetParser[A](inner: Parsable[A]) extends ComplexParsable[Set[A]] {
    private val InnerList = DelimitedList(",", inner)
    override val synchronousProductions: Map[List[Parsable[_]], List[AST[Parsable[_]]] => Option[Set[A]]] = Map(
      List(Terminal("{"), Terminal("}")) -> (c => Some(Set.empty[A])),
      List(Terminal("{"), InnerList, Terminal("}")) -> (c => for {
        list <- InnerList.fromAST(c(1))
      } yield list.toSet)
    )
  }
  implicit def parserToSetParser[A](implicit parser: Parsable[A]) = new SetParser[A](parser)

  class ListParser[A](inner: Parsable[A]) extends ComplexParsable[List[A]] {
    private val InnerList = DelimitedList(",", inner)
    override val synchronousProductions: Map[List[Parsable[_]], List[AST[Parsable[_]]] => Option[List[A]]] = Map(
      List(Terminal("["), Terminal("]")) -> (c => Some(List.empty[A])),
      List(Terminal("["), InnerList, Terminal("]")) -> (c => for {
        list <- InnerList.fromAST(c(1))
      } yield list)
    )
  }
  implicit def parserToListParser[A](implicit parser: Parsable[A]) = new ListParser[A](parser)

  class MapParser[K, V](key: Parsable[K], value: Parsable[V]) extends ComplexParsable[Map[K, V]] {

    private val KeyValPair = new ComplexParsable[(K, V)] {
      override val synchronousProductions: Map[List[Parsable[_]], List[AST[Parsable[_]]] => Option[(K, V)]] = Map(
        List(key, Terminal("->"), value) -> (c => for {
          k <- key.fromAST(c(0))
          v <- value.fromAST(c(2))
        } yield (k, v))
      )
    }

    private val InnerList = DelimitedList(",", KeyValPair)
    override val synchronousProductions: Map[List[Parsable[_]], List[AST[Parsable[_]]] => Option[Map[K, V]]] = Map(
      List(Terminal("{"), Terminal("}")) -> (c => Some(Map.empty[K, V])),
      List(Terminal("{"), InnerList, Terminal("}")) -> (c => for {
        list <- InnerList.fromAST(c(1))
      } yield list.toMap)
    )
  }
  implicit def parserToMapParser[K, V](implicit keyParser: Parsable[K], valueParser: Parsable[V]) =
    new MapParser[K, V](keyParser, valueParser)
}
