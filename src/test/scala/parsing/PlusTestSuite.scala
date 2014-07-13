package parsing

import ParserHelpers._

class PlusTestParameters[A](parsable: Parsable[A]) extends ParsableTestParameters[List[A]] {
  override val children = Set(
    parsable) ++
    parsable.children
  override val nonterminals = Set(
    Plus(parsable)) ++
    parsable.grammar.nonterminals
  override val tokens = parsable.allTokens
  override val productions = Set(
    Production(Plus(parsable), List(parsable, Plus(parsable))),
    Production(Plus(parsable), List(parsable))) ++
    parsable.grammar.productions
  override val cnfProductions = Set(
    Binary(NormalTag(Plus(parsable)), NormalTag(parsable), NormalTag(Plus(parsable))),
    Unary(NormalTag(Plus(parsable)), NormalTag(parsable))) ++
    parsable.grammar.cnfProductions
  override val testParses = Nil
}
