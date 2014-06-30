package parsing.lfg

import parsing.ContextFreeGrammar

class LexicalFunctionalGrammar[A](
  val productions: Set[LFGProduction[A]],
  val lexicalCategories: Set[LFGLexicalCategory[A]],
  val startSymbol: Option[A] = None) {

  private[this] val cfgProductions = productions.map(_.cfgProduction)
  private[this] val cfgLexicalCategories = lexicalCategories.map(_.cfgLexicalCategory)
  private[this] val cfGrammar =
    new ContextFreeGrammar[A](cfgProductions, cfgLexicalCategories.toList, startSymbol)

  def parseTokens(tokens: List[String]): Set[FStructure] = {
    val asts = cfGrammar.parseTokens(tokens)
    ???
  }
}