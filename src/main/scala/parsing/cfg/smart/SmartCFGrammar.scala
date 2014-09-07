package parsing.cfg.smart

import parsing.Grammar
import parsing.LexicalCategory
import parsing.cfg._
import parsing.cnf._
import parsing.cnf.smart._

import sortstreams._

// The type parameter A is the type of symbol used in productions and ASTs. It
// will typically be either String or Parsable[_].
class ContextFreeGrammar[A](
  val smartParams: SmartParseParameters[CNFAST[CNFConversionTag[A]]],
  val productions: Set[CFGProduction[A]],
  val lexicalCategories: Set[LexicalCategory[A]],
  val startSymbols: Set[A] = Set.empty[A]) {

  import smartParams._

  // we change the grammar to 2-Normal-Form for parsing
  val cnfProductions = productions.flatMap(productionToCNF).toSet
  val cnfLexicalCategories = lexicalCategories.map(
    CNFProxyLexicalCategory(_): LexicalCategory[CNFConversionTag[A]])
  val cnfStartSymbols = startSymbols.map(
    CNFConversionTag.Single(_): CNFConversionTag[A])

  // nonterminals are just everything that appears at the head of a (non-lexical) production
  lazy val nonterminals = productions.map(_.head)

  lazy val cnfGrammar =
    new SmartCNFGrammar[CNFConversionTag[A]](
      smartParams = smartParams,
      productions = cnfProductions,
      lexicalCategories = cnfLexicalCategories,
      startSymbols = cnfStartSymbols)

  def parseTokens(tokens: Seq[String]) = for {
    cnfParse <- cnfGrammar.parseTokens(tokens).toStream
    validParse <- convertAST(cnfParse)
  } yield validParse
}
