package parsing.lfg

import org.scalatest.FunSuite
import Parsables._
import parsing.ParseCommands._

class LFGTestSuite extends FunSuite {

  println(parse[Equation[RelativeIdentifier]]("up PRED = 'John'"))
  println(Expression.fromString("'John'"))
  println(Expression.fromString("up PRED"))
  println(RelativeIdentifier.fromString("up"))
  println(parse[Equation[RelativeIdentifier]]("up PRED = 'kiss<SUBJ,OBJ>'"))
  println(Expression.fromString("yes"))
  println(parse[Equation[RelativeIdentifier]]("up = down"))
  // a s{i,a}mple grammar
  val noun = LFGLexicalCategory[String](
    Set(
      """John:  up PRED = 'John'  ;
                up DEF  = yes     """,

      """Gary:  up PRED = 'Gary'  ;
                up DEF  = yes     """,

      """man:  up PRED = 'man'    """
    ).map(parseForced[LexicalEntry]),
    "N"
  )
  val verb = LFGLexicalCategory[String](
    Set(
      """kissed:  up PRED  = 'kiss<SUBJ,OBJ>' ;
                  up TENSE = PAST             """
    ).map(parseForced[LexicalEntry]),
    "V"
  )
  val determiner = LFGLexicalCategory[String](
    Set(
      "the: up DEF = yes",
      "a:   up DEF = no"
    ).map(parseForced[LexicalEntry]),
    "D"
  )
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