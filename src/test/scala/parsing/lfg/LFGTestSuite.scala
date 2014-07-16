package parsing.lfg

import org.scalatest.FunSuite
import Parsables._
import parsing.ParseCommands._
import parsing.Parsables._
import parsing.GenericParsables._

class LFGTestSuite extends FunSuite {

  // a s{i,a}mple grammar
  val partsOfSpeech = parseForced[Set[LFGLexicalCategory[String]]](
    """
    {   N:   {   John:    {   up PRED = 'John'              ,
                              up DEF  = yes                 },

                 Gary:    {   up PRED = 'Gary'              ,
                              up DEF  = yes                 },

                 man:     {   up PRED = 'man'               }},

        V:  {    kissed:  {   up PRED  = 'kiss<SUBJ,OBJ>'   ,
                              up TENSE = PAST               }},

        D:  {    the:     {   up DEF = yes                  },

                 a:       {   up DEF = no                   }}}
    """)

  val productions = Set(
    LFGProduction[String]("NP",
      List(
        ("N", parseForced[Specification]("{ up = down }"))
      )
    ),
    LFGProduction[String]("NP",
      List(
        ("D", parseForced[Specification]("{ up = down }")),
        ("N", parseForced[Specification]("{ up = down }"))
      )
    ),
    LFGProduction[String]("VP",
      List(
        ("V", parseForced[Specification]("{ up = down }")),
        ("NP", parseForced[Specification]("{ up OBJ = down }"))
      )
    ),
    LFGProduction[String]("S",
      List(
        ("NP", parseForced[Specification]("{ up SUBJ = down }")),
        ("VP", parseForced[Specification]("{ up = down }"))
      )
    )
  )

  val grammar = new LexicalFunctionalGrammar[String](
    productions = productions,
    lexicalCategories = partsOfSpeech,
    startSymbol = Some("S"))

  def testSentence(tokens: List[String]) = {
    println(tokens.mkString(" "))
    val fstructs = grammar.parseTokens(tokens)
    fstructs.map(FStructureParser.makeString).foreach(println)
    println
    assert(!fstructs.isEmpty)
  }

  test(s"John kissed Gary") {
    testSentence(List("John", "kissed", "Gary"))
  }
  test(s"a man kissed Gary") {
    testSentence(List("a", "man", "kissed", "Gary"))
  }
  test(s"the man kissed Gary") {
    testSentence(List("the", "man", "kissed", "Gary"))
  }
}