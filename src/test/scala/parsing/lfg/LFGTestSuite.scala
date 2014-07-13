package parsing.lfg

import org.scalatest.FunSuite
import Parsables._
import parsing.ParseCommands._
import parsing.Parsables._

class LFGTestSuite extends FunSuite {

  // a s{i,a}mple grammar
  val noun = parseForced[LFGLexicalCategory[String]](
    """N:   {   John:  up PRED = 'John'  ,
                       up DEF  = yes     ,

                Gary:  up PRED = 'Gary'  ,
                       up DEF  = yes     ,

                man:   up PRED = 'man'    }""")

  val verb = parseForced[LFGLexicalCategory[String]](
    """V:  {    kissed:  up PRED  = 'kiss<SUBJ,OBJ>' ,
                         up TENSE = PAST             }""")

  val determiner = parseForced[LFGLexicalCategory[String]](
    """D:  {    the:  up DEF = yes    ,
                a:    up DEF = no     }""")

  val productions = Set(
    LFGProduction[String]("NP",
      List(
        ("N", Set(
          "up = down"
        ).map(parseForced[Equation[RelativeIdentifier]]))
      )
    ),
    LFGProduction[String]("NP",
      List(
        ("D", Set(
          "up = down"
        ).map(parseForced[Equation[RelativeIdentifier]])),
        ("N", Set(
          "up = down"
        ).map(parseForced[Equation[RelativeIdentifier]]))
      )
    ),
    LFGProduction[String]("VP",
      List(
        ("V", Set(
          "up = down"
        ).map(parseForced[Equation[RelativeIdentifier]])),
        ("NP", Set(
          "up OBJ = down"
        ).map(parseForced[Equation[RelativeIdentifier]]))
      )
    ),
    LFGProduction[String]("S",
      List(
        ("NP", Set(
          "up SUBJ = down"
        ).map(parseForced[Equation[RelativeIdentifier]])),
        ("VP", Set(
          "up = down"
        ).map(parseForced[Equation[RelativeIdentifier]]))
      )
    )
  )

  val grammar = new LexicalFunctionalGrammar[String](
    productions = productions,
    lexicalCategories = Set(noun, verb, determiner),
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