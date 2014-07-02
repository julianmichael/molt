package parsing

class ProductionTestSuite extends ParsableTestSuite[Production[String]] {
  override val parameters = ProductionTestParameters
  override val parsable = Production
}

class ProductionPlusTestSuite extends ParsableTestSuite[List[String]] {
  override val parameters = new PlusTestParameters(Production.NonterminalSymbol)
  override val parsable = Plus(Production.NonterminalSymbol)
}

object ProductionTestParameters extends ParsableTestParameters[Production[String]] {
  private[this] val plusTestParameters = new PlusTestParameters(Production.NonterminalSymbol)
  override val children = Set(
    Production.NonterminalSymbol,
    Terminal("->"),
    Plus(Production.NonterminalSymbol)) ++
    plusTestParameters.children
  override val nonterminals = Set(
    Production,
    Plus(Production.NonterminalSymbol)) ++
    plusTestParameters.nonterminals
  override val tokens = Set("->") ++ plusTestParameters.tokens
  override val productions = Set(
    Production(Production, List(Production.NonterminalSymbol, Terminal("->"), Plus(Production.NonterminalSymbol)))) ++
    plusTestParameters.productions
  override val cnfProductions = Set(
    Binary(NormalTag(Production), NormalTag(Production.NonterminalSymbol), ChunkedTag(List(Terminal("->"), Plus(Production.NonterminalSymbol)))),
    Binary(ChunkedTag(List(Terminal("->"), Plus(Production.NonterminalSymbol))), NormalTag(Terminal("->")), NormalTag(Plus(Production.NonterminalSymbol)))) ++
    plusTestParameters.cnfProductions
  override val testParses = List(
    TestParse(
      Some("S -> NP VP"),
      Some(List("S", "->", "NP", "VP")),
      Some(ASTNonterminal(Production, List(
        ASTTerminal(Production.NonterminalSymbol, "S"),
        ASTTerminal(Terminal("->"), "->"),
        ASTNonterminal(Plus(Production.NonterminalSymbol), List(
          ASTTerminal(Production.NonterminalSymbol, "NP"),
          ASTNonterminal(Plus(Production.NonterminalSymbol), List(
            ASTTerminal(Production.NonterminalSymbol, "VP")
          ))
        ))
      ))),
      Some(Production("S", List("NP", "VP")))))
}
