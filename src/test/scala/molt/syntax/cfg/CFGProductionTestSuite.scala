package molt.syntax.cfg

import molt.syntax._
import molt.syntax.ParseCommands._
import molt.syntax.cfg.CFGParsables._
import molt.syntax.cfg.CFGParserHelpers._
import molt.syntax.cnf._

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
    CFGProduction(CFGProductionParser, List(ASTNormalTag(CFGProductionParser.NonterminalSymbol), ASTNormalTag(Terminal("->")), ASTNormalTag(Plus(CFGProductionParser.NonterminalSymbol))))) ++
    plusTestParameters.productions.get)

import CNFConversionTag._
  override val cnfProductions = Some(Set[CNFProduction[CNFConversionTag[CFGParsable[_]]]](
    Binary(
      Single(CFGProductionParser),
      ASTNormalTag(Single(CFGProductionParser.NonterminalSymbol)),
      ASTNormalTag(Chunk(List(
        ASTNormalTag(Terminal("->")),
        ASTNormalTag(Plus(CFGProductionParser.NonterminalSymbol)))))),
    Binary(
      Chunk(List(
        ASTNormalTag(Terminal("->")),
        ASTNormalTag(Plus(CFGProductionParser.NonterminalSymbol)))),
      ASTNormalTag(Single(Terminal("->"))),
      ASTNormalTag(Single(Plus(CFGProductionParser.NonterminalSymbol))))) ++
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
      Some(CFGProduction("S", List(ASTNormalTag("NP"), ASTNormalTag("VP"))))))
}
