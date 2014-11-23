package molt.syntax

import molt.syntax.cfg._
import molt.syntax.cnf._
import molt.syntax.cfg.CFGParserHelpers._

class PlusTestParameters[A](parsable: CFGParsable[A]) extends ParsableTestParameters[List[A]] {
  override val children = Some(Set(
    parsable) ++
    parsable.children)
  override val nonterminals = Some(Set(
    Plus(parsable)) ++
    parsable.grammar.nonterminals)
  override val tokens = Some(parsable.allTokens)
  override val productions = Some(Set(
    CFGProduction(Plus(parsable), List[ASTTag[CFGParsable[_]]](ASTNormalTag(parsable), ASTNormalTag(Plus(parsable)))),
    CFGProduction(Plus(parsable), List[ASTTag[CFGParsable[_]]](ASTNormalTag(parsable)))) ++
    parsable.grammar.productions)

  import CNFConversionTag._
  override val cnfProductions = Some(Set[CNFProduction[CNFConversionTag[CFGParsable[_]]]](
    Binary(Single(Plus(parsable)), ASTNormalTag(Single(parsable)), ASTNormalTag(Single(Plus(parsable)))),
    Unary(Single(Plus(parsable)), ASTNormalTag(Single(parsable)))) ++
    parsable.grammar.cnfProductions)
  override val testParses = Nil
}
