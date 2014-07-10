package parsing.lfg

import org.scalatest.FunSuite

class LFGTestSuite extends FunSuite {

  println(Equation.fromString("up PRED = 'John'"))
  println(Expression.fromString("'John'"))
  println(Expression.fromString("up PRED"))
  println(RelativeIdentifier.fromString("up"))
  println(Equation.fromString("up PRED = 'kiss<SUBJ,OBJ>'"))
  println(Expression.fromString("yes"))
  println(Equation.fromString("up = down"))
  // a s{i,a}mple grammar
  val noun = LFGLexicalCategory[String](
    Set[LexicalEntry](
      ("John", Set(
        Equation.fromStringUnique("up PRED = 'John'").get,
        Equation.fromStringUnique("up DEF = yes").get
      )),
      ("Gary", Set(
        Equation.fromStringUnique("up PRED = 'Gary'").get,
        Equation.fromStringUnique("up DEF = yes").get
      )),
      ("man", Set(
        Equation.fromStringUnique("up PRED = 'man'").get
      ))
    ),
    "N"
  )
  val verb = LFGLexicalCategory[String](
    Set[LexicalEntry](
      ("kissed", Set(
        Equation.fromStringUnique("up PRED = 'kiss<SUBJ,OBJ>'").get,
        Equation.fromStringUnique("up TENSE = PAST").get
      ))
    ),
    "V"
  )
  val determiner = LFGLexicalCategory[String](
    Set[LexicalEntry](
      ("the", Set(
        Equation.fromStringUnique("up DEF = yes").get
      )),
      ("a", Set(
        Equation.fromStringUnique("up DEF = no").get
      ))
    ),
    "D"
  )
  val productions = Set(
    LFGProduction[String]("NP",
      List(
        ("N", Set(
          Equation.fromStringUnique("up = down").get
        ))
      )
    ),
    LFGProduction[String]("NP",
      List(
        ("D", Set(
          Equation.fromStringUnique("up = down").get
        )),
        ("N", Set(
          Equation.fromStringUnique("up = down").get
        ))
      )
    ),
    LFGProduction[String]("VP",
      List(
        ("V", Set(
          Equation.fromStringUnique("up = down").get
        )),
        ("NP", Set(
          Equation.fromStringUnique("up OBJ = down").get
        ))
      )
    ),
    LFGProduction[String]("S",
      List(
        ("NP", Set(
          Equation.fromStringUnique("up SUBJ = down").get
        )),
        ("VP", Set(
          Equation.fromStringUnique("up = down").get
        ))
      )
    )
  )

  val grammar = new LexicalFunctionalGrammar[String](
    productions = productions,
    lexicalCategories = Set(noun, verb, determiner),
    startSymbol = Some("S"))

  test(s"John kissed Gary") {
    val fstructs = grammar.parseTokens(List("John", "kissed", "Gary"))
    fstructs.map(_.pretty).foreach(println)
    assert(!fstructs.isEmpty)
  }
  test(s"a man kissed Gary") {
    val fstructs = grammar.parseTokens(List("a", "man", "kissed", "Gary"))
    fstructs.map(_.pretty).foreach(println)
    assert(!fstructs.isEmpty)
  }
  test(s"the man kissed Gary") {
    val fstructs = grammar.parseTokens(List("the", "man", "kissed", "Gary"))
    fstructs.map(_.pretty).foreach(println)
    assert(!fstructs.isEmpty)
  }
}