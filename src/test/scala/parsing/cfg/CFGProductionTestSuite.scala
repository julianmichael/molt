package parsing.cfg

import parsing._
import parsing.ParseCommands._
import parsing.cfg.CFGParsables._
import parsing.cfg.CFGParserHelpers._
import parsing.cnf._

class CFGProductionTestSuite extends ParsableTestSuite[CFGProduction[String]] {
  override val parameters = CFGProductionTestParameters
  override val parsable = CFGProductionParser
}

class CFGProductionPlusTestSuite extends ParsableTestSuite[List[String]] {
  override val parameters = new PlusTestParameters(CFGProductionParser.NonterminalSymbol)
  override val parsable = Plus(CFGProductionParser.NonterminalSymbol)
}

object CFGProductionTestParameters extends ParsableTestParameters[CFGProduction[String]] {
  private[this] val plusTestParameters = new PlusTestParameters(CFGProductionParser.NonterminalSymbol)
  override val children = Some(Set(
    CFGProductionParser.NonterminalSymbol,
    Terminal("->"),
    Plus(CFGProductionParser.NonterminalSymbol)) ++
    plusTestParameters.children.get)
  override val nonterminals = Some(Set(
    CFGProductionParser,
    Plus(CFGProductionParser.NonterminalSymbol)) ++
    plusTestParameters.nonterminals.get)
  override val tokens = Some(Set("->") ++ plusTestParameters.tokens.get)
  override val productions = Some(Set(
    CFGProduction(CFGProductionParser, List(CFGProductionParser.NonterminalSymbol, Terminal("->"), Plus(CFGProductionParser.NonterminalSymbol)))) ++
    plusTestParameters.productions.get)
  override val cnfProductions = Some(Set(
    Binary(NormalTag(CFGProductionParser), NormalTag(CFGProductionParser.NonterminalSymbol), ChunkedTag(List(Terminal("->"), Plus(CFGProductionParser.NonterminalSymbol)))),
    Binary(ChunkedTag(List(Terminal("->"), Plus(CFGProductionParser.NonterminalSymbol))), NormalTag(Terminal("->")), NormalTag(Plus(CFGProductionParser.NonterminalSymbol)))) ++
    plusTestParameters.cnfProductions.get)
  override val testParses = List(
    TestParse(
      Some("S -> NP VP"),
      Some(List("S", "->", "NP", "VP")),
      Some(ASTNonterminal(CFGProductionParser, List(
        ASTTerminal(CFGProductionParser.NonterminalSymbol, "S"),
        ASTTerminal(Terminal("->"), "->"),
        ASTNonterminal(Plus(CFGProductionParser.NonterminalSymbol), List(
          ASTTerminal(CFGProductionParser.NonterminalSymbol, "NP"),
          ASTNonterminal(Plus(CFGProductionParser.NonterminalSymbol), List(
            ASTTerminal(CFGProductionParser.NonterminalSymbol, "VP")
          ))
        ))
      ))),
      Some(CFGProduction("S", List("NP", "VP")))))
}
