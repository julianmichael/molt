package molt.syntax.cnf

import molt.syntax.LexicalCategory
import molt.syntax.cfg.ASTTag
import molt.syntax.cfg.ASTNormalTag
import molt.syntax.cfg.ASTEmptyTag
import molt.util.Memoize

case class CNFGrammar[A](
  val productions: Set[CNFProduction[A]],
  val lexicalCategories: Set[LexicalCategory[A]],
  val startSymbols: Set[A] = Set.empty[A])
