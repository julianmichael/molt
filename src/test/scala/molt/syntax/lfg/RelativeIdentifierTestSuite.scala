package molt.syntax.lfg

import molt.syntax._
import molt.syntax.cnf._
import molt.syntax.cfg._

import molt.syntax.cfg.parsable.ParseCommands._
import molt.syntax.cfg.parsable.CFGParserHelpers._
import molt.syntax.cfg.parsable.CFGParsables._
import molt.syntax.cfg.parsable.GenericParsables._
import molt.syntax.cfg.parsable._

import molt.syntax.lfg.parsable.LFGParsables._

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

import CNFConversionTag._
  override val cnfProductions = Some(Set[CNFProduction[CNFConversionTag[CFGParsable[_]]]](
    Unary(Single(RelativeIdentifierParser), ASTNormalTag(Single(Terminal("up")))),
    Unary(Single(RelativeIdentifierParser), ASTNormalTag(Single(Terminal("down"))))))
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
