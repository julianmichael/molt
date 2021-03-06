package molt.syntax.cfg.parsable

import molt.syntax.cfg.CFGProduction
import molt.syntax.cfg.ASTNormalTag
import molt.syntax.cfg.AST
import molt.syntax.cfg.parsable.CFGParserHelpers._

object CFGParsables {

  implicit object CFGProductionParser extends ComplexCFGParsable[CFGProduction[String]] {

    // for convenience in ProductionParser
    object NonterminalSymbol extends CFGParsableLexicalCategory {
      def member(s: String) = s != "->"
    }

    val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[CFGProduction[String]])] = Map(
      List(NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)) ->
        (c => for {
          head <- NonterminalSymbol.fromAST(c(0))
          children <- Plus(NonterminalSymbol).fromAST(c(2))
        } yield CFGProduction(head, children.map(ASTNormalTag(_))))
      )
  }
}
