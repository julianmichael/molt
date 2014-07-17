package parsing.lfg

import parsing._
import parsing.ParseCommands._
import parsing.GenericParsables._
import parsing.ParserHelpers._

import parsing.lfg.Parsables._
import parsing.cnf._
import parsing.cfg._

class RelativeIdentifierTestSuite extends ParsableTestSuite[RelativeIdentifier] {
  override val parameters = RelativeIdentifierTestParameters
  override val parsable = RelativeIdentifierParser
}

object RelativeIdentifierTestParameters extends ParsableTestParameters[RelativeIdentifier] {
  override val children = Some(Set[Parsable[_]](
    Terminal("up"),
    Terminal("down")))
  override val nonterminals = Some(Set[Parsable[_]](
    RelativeIdentifierParser))
  override val tokens = Some(Set("up", "down"))
  override val productions = Some(Set[CFGProduction[Parsable[_]]](
    CFGProduction(RelativeIdentifierParser, List(Terminal("up"))),
    CFGProduction(RelativeIdentifierParser, List(Terminal("down")))))
  override val cnfProductions = Some(Set[CNFProduction[Parsable[_]]](
    Unary(NormalTag(RelativeIdentifierParser), NormalTag(Terminal("up"))),
    Unary(NormalTag(RelativeIdentifierParser), NormalTag(Terminal("down")))))
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
