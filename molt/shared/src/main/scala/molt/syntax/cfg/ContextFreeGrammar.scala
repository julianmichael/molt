package molt.syntax.cfg

import molt.syntax.LexicalCategory
import molt.syntax.cnf.CNFGrammar

case class ContextFreeGrammar[A](
  val productions: Set[CFGProduction[A]],
  val lexicalCategories: Set[LexicalCategory[A]],
  val startSymbols: Set[A] = Set.empty[A]) {
  
  // nonterminals are just everything that appears at the head of a (non-lexical) production
  lazy val nonterminals = productions.map(_.head)
  
  def toCNF = {
    val cnfProductions = productions.flatMap(productionToCNF).toSet
    val cnfLexicalCategories = lexicalCategories.map(
          CNFProxyLexicalCategory(_): LexicalCategory[CNFConversionTag[A]])
    val cnfStartSymbols = startSymbols.map(
          CNFConversionTag.Single(_): CNFConversionTag[A])
    new CNFGrammar[CNFConversionTag[A]](cnfProductions, cnfLexicalCategories, cnfStartSymbols)
  }
}