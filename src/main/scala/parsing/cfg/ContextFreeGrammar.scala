package parsing.cfg

import parsing.Grammar
import parsing.LexicalCategory
import parsing.cnf._

// The type parameter A is the type of symbol used in productions and ASTs. It
// will typically be either String or Parsable[_].
class ContextFreeGrammar[A](
  val productions: Set[CFGProduction[A]],
  val lexicalCategories: Set[LexicalCategory[A]],
  val startSymbols: Set[A] = Set.empty[A]) extends Grammar[AST[A]] {

  // we change the grammar to 2-Normal-Form for parsing
  val cnfProductions = productions.flatMap(productionToCNF).toSet
  val cnfLexicalCategories = lexicalCategories.map(
    CNFProxyLexicalCategory(_): LexicalCategory[CNFConversionTag[A]])
  val cnfStartSymbols = startSymbols.map(
    CNFConversionTag.Single(_): CNFConversionTag[A])

  // nonterminals are just everything that appears at the head of a (non-lexical) production
  lazy val nonterminals = productions.map(_.head)

  lazy val cnfGrammar =
    new CNFGrammar[CNFConversionTag[A]](cnfProductions, cnfLexicalCategories, cnfStartSymbols)

  override def parseTokens(tokens: Seq[String]) = {
    val cnfParses = cnfGrammar.parseTokens(tokens)
    val validParses = cnfParses.map(convertAST).flatten
    validParses
  }
}
