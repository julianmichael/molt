package parsing.lfg

import org.scalatest.FunSuite

class LFGTestSuite extends FunSuite {

  // a s{i,a}mple grammar
  // need: productions, lexical categories, start symbol?
  val noun = LFGLexicalCategory[String](
    Set[LexicalEntry](
      ("John", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "PRED"),
          SemanticFormExpression("`John'"))),
        Defining(Assignment(
          Application(IdentifierExpression(Up), "DEF"),
          ValueExpression("+")))
      )),
      ("Gary", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "PRED"),
          SemanticFormExpression("`Gary'"))),
        Defining(Assignment(
          Application(IdentifierExpression(Up), "DEF"),
          ValueExpression("+")))
      )),
      ("man", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "PRED"),
          SemanticFormExpression("`man'")))
      ))
    ),
    "N"
  )
  val verb = LFGLexicalCategory[String](
    Set[LexicalEntry](
      ("kissed", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "PRED"),
          SemanticFormExpression("`kiss<subj, obj>'")))
      ))
    ),
    "V"
  )
  val determiner = LFGLexicalCategory[String](
    Set[LexicalEntry](
      ("the", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "DEF"),
          ValueExpression("+")))
      )),
      ("a", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "DEF"),
          ValueExpression("-")))
      ))
    ),
    "D"
  )
  val productions = Set(
    LFGProduction[String]("NP",
      List(
        ("N", Set(Defining(Assignment(
          IdentifierExpression(Up),
          IdentifierExpression(Down)
        ))))
      )
    ),
    LFGProduction[String]("NP",
      List(
        ("D", Set(Defining(Assignment(
          IdentifierExpression(Up),
          IdentifierExpression(Down)
        )))),
        ("N", Set(Defining(Assignment(
          IdentifierExpression(Up),
          IdentifierExpression(Down)
        ))))
      )
    ),
    LFGProduction[String]("VP",
      List(
        ("V", Set(Defining(Assignment(
          IdentifierExpression(Up),
          IdentifierExpression(Down)
        )))),
        ("NP", Set(Defining(Assignment(
          Application(IdentifierExpression(Up), "OBJ"),
          IdentifierExpression(Down)
        ))))
      )
    ),
    LFGProduction[String]("S",
      List(
        ("NP", Set(Defining(Assignment(
          Application(IdentifierExpression(Up), "SUBJ"),
          IdentifierExpression(Down)
        )))),
        ("VP", Set(Defining(Assignment(
          IdentifierExpression(Up),
          IdentifierExpression(Down)
        ))))
      )
    )
  )

  val grammar = new LexicalFunctionalGrammar[String](
    productions = productions,
    lexicalCategories = Set(noun, verb, determiner),
    startSymbol = Some("S"))

  test(s"John kissed Gary") {
    val fstructs = grammar.parseTokens(List("John", "kissed", "Gary"))
    println("John kissed Gary")
    fstructs.map(_.pretty).foreach(println)
    fstructs.foreach(_.map.foreach(println))
    assert(!fstructs.isEmpty)
  }
  test(s"a man kissed Gary") {
    println("a man kissed Gary")
    val fstructs = grammar.parseTokens(List("a", "man", "kissed", "Gary"))
    fstructs.map(_.pretty).foreach(println)
    fstructs.foreach(_.map.foreach(println))
    assert(!fstructs.isEmpty)
  }
  test(s"the man kissed Gary") {
    println("the man kissed Gary")
    val fstructs = grammar.parseTokens(List("the", "man", "kissed", "Gary"))
    fstructs.map(_.pretty).foreach(println)
    fstructs.foreach(_.map.foreach(println))
    assert(!fstructs.isEmpty)
  }
}