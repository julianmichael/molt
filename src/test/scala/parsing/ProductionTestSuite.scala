package parsing

class ProductionTestSuite extends ParsableTestSuite[Production[String]] {
  override val parameters = ProductionTestParameters
  override val parsable = Production
}

class PlusNonterminalSymbolTestSuite extends ParsableTestSuite[List[String]] {
  override val parameters = new PlusTestParameters(NonterminalSymbol)
  override val parsable = Plus(NonterminalSymbol)
}

object ProductionTestParameters extends ParsableTestParameters[Production[String]] {
  private[this] val plusTestParameters = new PlusTestParameters(NonterminalSymbol)
  override val children = Set(
    NonterminalSymbol,
    Terminal("->"),
    Plus(NonterminalSymbol)) ++
    plusTestParameters.children
  override val nonterminals = Set(
    Production,
    Plus(NonterminalSymbol)) ++
    plusTestParameters.nonterminals
  override val tokens = Set("->") ++ plusTestParameters.tokens
  override val productions = Set(
    Production(Production, List(NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)))) ++
    plusTestParameters.productions
  override val cnfProductions = Set(
    Binary(NormalTag(Production), NormalTag(NonterminalSymbol), ChunkedTag(List(Terminal("->"), Plus(NonterminalSymbol)))),
    Binary(ChunkedTag(List(Terminal("->"), Plus(NonterminalSymbol))), NormalTag(Terminal("->")), NormalTag(Plus(NonterminalSymbol)))) ++
    plusTestParameters.cnfProductions
  override val testParses = List(
    TestParse(
      Some("S -> NP VP"),
      Some(List("S", "->", "NP", "VP")),
      None,
      Some(Production("S", List("NP", "VP")))))
}
