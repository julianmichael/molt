package parsing

import ParserHelpers._

class PlusTestParameters[A](parsable: Parsable[A]) extends ParsableTestParameters[List[A]] {
  override val children = Some(Set(
    parsable) ++
    parsable.children)
  override val nonterminals = Some(Set(
    Plus(parsable)) ++
    parsable.grammar.nonterminals)
  override val tokens = Some(parsable.allTokens)
  override val productions = Some(Set(
    Production(Plus(parsable), List(parsable, Plus(parsable))),
    Production(Plus(parsable), List(parsable))) ++
    parsable.grammar.productions)
  override val cnfProductions = Some(Set(
    Binary(NormalTag(Plus(parsable)), NormalTag(parsable), NormalTag(Plus(parsable))),
    Unary(NormalTag(Plus(parsable)), NormalTag(parsable))) ++
    parsable.grammar.cnfProductions)
  override val testParses = Nil
}
