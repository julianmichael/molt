package parsing.lfg

import org.scalatest.FunSuite

class LFGTestSuite extends FunSuite {

  // a s{i,a}mple grammar
  // need: productions, lexical categories, start symbol?
  lazy val noun = LFGLexicalCategory[String](
    Set[LexicalEntry](
      ("John", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "PRED"),
          SemanticFormExpression("`John'")))
      )),
      ("Gary", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "PRED"),
          SemanticFormExpression("`Gary'")))
      ))
    ),
    "N"
  )
  lazy val verb = LFGLexicalCategory[String](
    Set[LexicalEntry](
      ("kissed", Set(
        Defining(Assignment(
          Application(IdentifierExpression(Up), "PRED"),
          SemanticFormExpression("`kiss<subj, obj>'")))
      ))
    ),
    "V"
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
    lexicalCategories = Set(noun, verb),
    startSymbol = Some("S"))

  test(s"John kissed Gary") {
    println(grammar.parseTokens(List("John", "kissed", "Gary")))
  }
}