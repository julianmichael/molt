package parsing

import Parsables._
import ParserHelpers._

class ProductionTestSuite extends ParsableTestSuite[Production[String]] {
  override val parameters = ProductionTestParameters
  override val parsable = ProductionParser
}

class ProductionPlusTestSuite extends ParsableTestSuite[List[String]] {
  override val parameters = new PlusTestParameters(ProductionParser.NonterminalSymbol)
  override val parsable = Plus(ProductionParser.NonterminalSymbol)
}

object ProductionTestParameters extends ParsableTestParameters[Production[String]] {
  private[this] val plusTestParameters = new PlusTestParameters(ProductionParser.NonterminalSymbol)
  override val children = Some(Set(
    ProductionParser.NonterminalSymbol,
    Terminal("->"),
    Plus(ProductionParser.NonterminalSymbol)) ++
    plusTestParameters.children.get)
  override val nonterminals = Some(Set(
    ProductionParser,
    Plus(ProductionParser.NonterminalSymbol)) ++
    plusTestParameters.nonterminals.get)
  override val tokens = Some(Set("->") ++ plusTestParameters.tokens.get)
  override val productions = Some(Set(
    Production(ProductionParser, List(ProductionParser.NonterminalSymbol, Terminal("->"), Plus(ProductionParser.NonterminalSymbol)))) ++
    plusTestParameters.productions.get)
  override val cnfProductions = Some(Set(
    Binary(NormalTag(ProductionParser), NormalTag(ProductionParser.NonterminalSymbol), ChunkedTag(List(Terminal("->"), Plus(ProductionParser.NonterminalSymbol)))),
    Binary(ChunkedTag(List(Terminal("->"), Plus(ProductionParser.NonterminalSymbol))), NormalTag(Terminal("->")), NormalTag(Plus(ProductionParser.NonterminalSymbol)))) ++
    plusTestParameters.cnfProductions.get)
  override val testParses = List(
    TestParse(
      Some("S -> NP VP"),
      Some(List("S", "->", "NP", "VP")),
      Some(ASTNonterminal(ProductionParser, List(
        ASTTerminal(ProductionParser.NonterminalSymbol, "S"),
        ASTTerminal(Terminal("->"), "->"),
        ASTNonterminal(Plus(ProductionParser.NonterminalSymbol), List(
          ASTTerminal(ProductionParser.NonterminalSymbol, "NP"),
          ASTNonterminal(Plus(ProductionParser.NonterminalSymbol), List(
            ASTTerminal(ProductionParser.NonterminalSymbol, "VP")
          ))
        ))
      ))),
      Some(Production("S", List("NP", "VP")))))
}
