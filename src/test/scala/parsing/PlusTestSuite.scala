package parsing

import parsing.cfg._
import parsing.cnf._
import parsing.cfg.CFGParserHelpers._

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
  override val cnfProductions = Some(Set(
    Binary(CNFNormalTag(Plus(parsable)), CNFNormalTag(parsable), CNFNormalTag(Plus(parsable))),
    Unary(CNFNormalTag(Plus(parsable)), CNFNormalTag(parsable))) ++
    parsable.grammar.cnfProductions)
  override val testParses = Nil
}
