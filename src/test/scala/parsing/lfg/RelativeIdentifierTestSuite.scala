package parsing.lfg

import parsing._
import parsing.ParseCommands._
import parsing.cfg.GenericParsables._
import parsing.cfg.CFGParserHelpers._
import parsing.lfg.LFGParsables._
import parsing.cnf._
import parsing.cfg._

class RelativeIdentifierTestSuite extends ParsableTestSuite[RelativeIdentifier] {
  override val parameters = RelativeIdentifierTestParameters
  override val parsable = RelativeIdentifierParser
}

object RelativeIdentifierTestParameters extends ParsableTestParameters[RelativeIdentifier] {
  override val children = Some(Set[CFGParsable[_]](
    Terminal("up"),
    Terminal("down")))
  override val nonterminals = Some(Set[CFGParsable[_]](
    RelativeIdentifierParser))
  override val tokens = Some(Set("up", "down"))
  override val productions = Some(Set[CFGProduction[CFGParsable[_]]](
    CFGProduction(RelativeIdentifierParser, List(ASTNormalTag(Terminal("up")))),
    CFGProduction(RelativeIdentifierParser, List(ASTNormalTag(Terminal("down"))))))
  override val cnfProductions = Some(Set[CNFProduction[CFGParsable[_]]](
    Unary(CNFNormalTag(RelativeIdentifierParser), CNFNormalTag(Terminal("up"))),
    Unary(CNFNormalTag(RelativeIdentifierParser), CNFNormalTag(Terminal("down")))))
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
