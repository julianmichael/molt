package parsing.cfg

import parsing.Grammar
import parsing.LexicalCategory
import parsing.cnf._

// The type parameter A is the type of symbol used in productions and ASTs. It
// will typically be either String or Parsable[_].
class ContextFreeGrammar[A](
  val productions: Set[CFGProduction[A]],
  val lexicalCategories: List[LexicalCategory[A]],
  val startSymbol: Option[A] = None) extends Grammar[AST[A]] {

  // we change the grammar to Chomsky Normal Form* for parsing
  // * with unary productions 
  lazy val cnfProductions = productions.flatMap(_.toCNF).toSet

  // nonterminals are just everything that appears at the head of a (non-lexical) production
  lazy val nonterminals = productions.map(_.head)

  lazy val cnfGrammar = new CNFGrammar[A](cnfProductions, lexicalCategories)

  override def parseTokens(tokens: Seq[String]) = {
    val cnfParses = cnfGrammar.parseTokens(tokens)
    val validParses = cnfParses.map(_.dechomskify).flatten
    val validProperlyStartingParses = startSymbol match {
      case None => validParses
      case Some(sym) => validParses.filter(_.label == sym)
    }
    validProperlyStartingParses
  }
}
