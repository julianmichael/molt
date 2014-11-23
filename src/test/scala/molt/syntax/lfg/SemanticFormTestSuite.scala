package molt.syntax.lfg

import molt.syntax._
import molt.syntax.ParseCommands._
import molt.syntax.cfg._
import molt.syntax.cfg.GenericParsables._
import molt.syntax.cfg.CFGParserHelpers._
import molt.syntax.cnf._
import molt.syntax.lfg.LFGParsables._

class SemanticFormTestSuite extends ParsableTestSuite[SemanticForm] {
  override val parameters = SemanticFormTestParameters
  override val parsable = SemanticFormParser
}

object SemanticFormTestParameters extends ParsableTestParameters[SemanticForm] {
  override val children = Some(Set[CFGParsable[_]](
    Alphabetical,
    Terminal("<"),
    FeatureListParser,
    Optional(FeatureListParser),
    CFGEmptyCategory,
    Terminal(">")) ++
    FeatureListParser.children)
  override val nonterminals = Some(Set[CFGParsable[_]](
    SemanticFormParser,
    Optional(FeatureListParser),
    FeatureListParser) ++
    FeatureListParser.grammar.nonterminals)
  override val tokens = Some(Set("<", ">") ++
    FeatureListParser.allTokens)
  override val productions = None
  override val cnfProductions = None
  override val testParses = List[TestParse[SemanticForm]](
    TestParse(
      Some("John"),
      Some(List("John")),
      None,
      Some(SemanticForm("John", Nil))),
    TestParse(
      Some("kissed<SUBJ,OBJ>"),
      Some(List("kissed", "<", "SUBJ", ",", "OBJ", ">")),
      None,
      Some(SemanticForm("kissed", List("SUBJ", "OBJ")))))
}
