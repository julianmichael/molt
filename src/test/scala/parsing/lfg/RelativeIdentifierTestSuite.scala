package parsing.lfg

import parsing._
import parsing.Parsables._
import parsing.lfg.Parsables._
import parsing.ParserHelpers._

class RelativeIdentifierTestSuite extends ParsableTestSuite[RelativeIdentifier] {
  override val parameters = RelativeIdentifierTestParameters
  override val parsable = RelativeIdentifierParser
}

object RelativeIdentifierTestParameters extends ParsableTestParameters[RelativeIdentifier] {
  override val children = Set[Parsable[_]](
    Terminal("up"),
    Terminal("down"))
  override val nonterminals = Set[Parsable[_]](
    RelativeIdentifierParser)
  override val tokens = Set("up", "down")
  override val productions = Set[Production[Parsable[_]]](
    Production(RelativeIdentifierParser, List(Terminal("up"))),
    Production(RelativeIdentifierParser, List(Terminal("down"))))
  override val cnfProductions = Set[CNFProduction[Parsable[_]]](
    Unary(NormalTag(RelativeIdentifierParser), NormalTag(Terminal("up"))),
    Unary(NormalTag(RelativeIdentifierParser), NormalTag(Terminal("down"))))
  override val testParses = List[TestParse[RelativeIdentifier]](
    TestParse(
      Some("up"),
      Some(List("up")),
      None,
      Some(Up)),
    TestParse(
      Some("down"),
      Some(List("down")),
      None,
      Some(Down)))
}
