package parsing.cfg

import parsing.ParserHelpers._
import parsing._

object CFGParsables {

  implicit object CFGProductionParser extends ComplexParsable[CFGProduction[String]] {

    // for convenience in ProductionParser
    val NonterminalSymbol = new ParsableLexicalCategory(w => w != "->")

    val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[CFGProduction[String]])] = Map(
      List(NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)) ->
        (c => for {
          head <- NonterminalSymbol.fromAST(c(0))
          children <- Plus(NonterminalSymbol).fromAST(c(2))
        } yield CFGProduction(head, children))
      )
  }
}
