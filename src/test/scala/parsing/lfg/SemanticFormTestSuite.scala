package parsing.lfg

import parsing._
import parsing.Parsables._
import parsing.lfg.Parsables._
import parsing.ParserHelpers._

class SemanticFormTestSuite extends ParsableTestSuite[SemanticForm] {
  override val parameters = SemanticFormTestParameters
  override val parsable = SemanticFormParser
}

object SemanticFormTestParameters extends ParsableTestParameters[SemanticForm] {
  override val children = Some(Set[Parsable[_]](
    Alphabetical,
    Terminal("<"),
    FeatureListParser,
    Terminal(">")) ++
    FeatureListParser.children)
  override val nonterminals = Some(Set[Parsable[_]](
    SemanticFormParser,
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
