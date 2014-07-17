package parsing.cfg

import parsing.ParserHelpers._
import parsing._

object CFGParsables {

  implicit object CFGProductionParser extends ComplexCFGParsable[CFGProduction[String]] {

    // for convenience in ProductionParser
    val NonterminalSymbol = new CFGParsableLexicalCategory(w => w != "->")

    val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[CFGProduction[String]])] = Map(
      List(NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)) ->
        (c => for {
          head <- NonterminalSymbol.fromAST(c(0))
          children <- Plus(NonterminalSymbol).fromAST(c(2))
        } yield CFGProduction(head, children))
      )
  }
}
