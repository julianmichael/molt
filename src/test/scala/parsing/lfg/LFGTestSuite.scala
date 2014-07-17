package parsing.lfg

import org.scalatest.FunSuite
import parsing._
import parsing.ParseCommands._
import parsing.GenericParsables._
import parsing.ParserHelpers._
import parsing.lfg.Parsables._

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

  val productions = parseForced[Set[LFGProduction[String]]]("""
    {
      NP -> [
        N: { up = down }
      ],

      NP -> [
        D: { up = down },
        N: { up = down }
      ],

      VP -> [
        V:  { up = down },
        NP: { up OBJ = down }
      ],

      S -> [
        NP: { up SUBJ = down },
        VP: { up = down }
      ]
    }
  """)

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