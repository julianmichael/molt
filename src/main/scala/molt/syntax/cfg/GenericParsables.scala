package molt.syntax.cfg

import CFGParserHelpers._

object GenericParsables {

  /**********
   * Parameterized parsables: implicitly created from other parsables! Awesome
   * stuff!
**********/

  class SetParser[A](inner: CFGParsable[A]) extends ComplexCFGParsable[Set[A]] {
    private val InnerList = DelimitedList(",", inner)
    override val synchronousProductions: Map[List[CFGParsable[_]], List[AST[CFGParsable[_]]] => Option[Set[A]]] = Map(
      List(Terminal("{"), Terminal("}")) -> (c => Some(Set.empty[A])),
      List(Optional(Terminal("{")), InnerList, Optional(Terminal("}"))) -> (c => for {
        list <- InnerList.fromAST(c(1))
        leftBrace <- Optional(Terminal("{")).fromAST(c(0))
        rightBrace <- Optional(Terminal("}")).fromAST(c(2))
        if leftBrace.isEmpty == rightBrace.isEmpty
      } yield list.toSet)
    )
  }
  implicit def parserToSetParser[A](implicit parser: CFGParsable[A]) = new SetParser[A](parser)

  class ListParser[A](inner: CFGParsable[A]) extends ComplexCFGParsable[List[A]] {
    private val InnerList = DelimitedList(",", inner)
    override val synchronousProductions: Map[List[CFGParsable[_]], List[AST[CFGParsable[_]]] => Option[List[A]]] = Map(
      List(Terminal("["), Terminal("]")) -> (c => Some(List.empty[A])),
      List(Optional(Terminal("[")), InnerList, Optional(Terminal("]"))) -> (c => for {
        list <- InnerList.fromAST(c(1))
        leftBrace <- Optional(Terminal("[")).fromAST(c(0))
        rightBrace <- Optional(Terminal("]")).fromAST(c(2))
        if leftBrace.isEmpty == rightBrace.isEmpty
      } yield list)
    )
  }
  implicit def parserToListParser[A](implicit parser: CFGParsable[A]) = new ListParser[A](parser)

  class MapParser[K, V](key: CFGParsable[K], value: CFGParsable[V]) extends ComplexCFGParsable[Map[K, V]] {

    private val KeyValPair = new ComplexCFGParsable[(K, V)] {
      override val synchronousProductions: Map[List[CFGParsable[_]], List[AST[CFGParsable[_]]] => Option[(K, V)]] = Map(
        List(key, Terminal("->"), value) -> (c => for {
          k <- key.fromAST(c(0))
          v <- value.fromAST(c(2))
        } yield (k, v))
      )
    }

    private val InnerList = DelimitedList(",", KeyValPair)
    override val synchronousProductions: Map[List[CFGParsable[_]], List[AST[CFGParsable[_]]] => Option[Map[K, V]]] = Map(
      List(Terminal("{"), Terminal("}")) -> (c => Some(Map.empty[K, V])),
      List(Optional(Terminal("{")), InnerList, Optional(Terminal("}"))) -> (c => for {
        list <- InnerList.fromAST(c(1))
        leftBrace <- Optional(Terminal("{")).fromAST(c(0))
        rightBrace <- Optional(Terminal("}")).fromAST(c(2))
        if leftBrace.isEmpty == rightBrace.isEmpty
      } yield list.toMap)
    )
  }
  implicit def parserToMapParser[K, V](implicit keyParser: CFGParsable[K], valueParser: CFGParsable[V]) =
    new MapParser[K, V](keyParser, valueParser)
}
