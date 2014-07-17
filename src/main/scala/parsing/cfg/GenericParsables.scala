package parsing.cfg

import parsing.ParserHelpers._

object GenericParsables {

  /**********
   * Parameterized parsables: implicitly created from other parsables! Awesome
   * stuff!
**********/

  class SetParser[A](inner: Parsable[A]) extends ComplexCFGParsable[Set[A]] {
    private val InnerList = DelimitedList(",", inner)
    override val synchronousProductions: Map[List[Parsable[_]], List[AST[Parsable[_]]] => Option[Set[A]]] = Map(
      List(Terminal("{"), Terminal("}")) -> (c => Some(Set.empty[A])),
      List(Terminal("{"), InnerList, Terminal("}")) -> (c => for {
        list <- InnerList.fromAST(c(1))
      } yield list.toSet)
    )
  }
  implicit def parserToSetParser[A](implicit parser: Parsable[A]) = new SetParser[A](parser)

  class ListParser[A](inner: Parsable[A]) extends ComplexCFGParsable[List[A]] {
    private val InnerList = DelimitedList(",", inner)
    override val synchronousProductions: Map[List[Parsable[_]], List[AST[Parsable[_]]] => Option[List[A]]] = Map(
      List(Terminal("["), Terminal("]")) -> (c => Some(List.empty[A])),
      List(Terminal("["), InnerList, Terminal("]")) -> (c => for {
        list <- InnerList.fromAST(c(1))
      } yield list)
    )
  }
  implicit def parserToListParser[A](implicit parser: Parsable[A]) = new ListParser[A](parser)

  class MapParser[K, V](key: Parsable[K], value: Parsable[V]) extends ComplexCFGParsable[Map[K, V]] {

    private val KeyValPair = new ComplexCFGParsable[(K, V)] {
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
