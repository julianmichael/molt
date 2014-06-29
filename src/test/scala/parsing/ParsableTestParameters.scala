package parsing

import org.scalatest.FunSuite

case class TestParse[A](
    string: Option[String],
    tokens: Option[List[String]],
    ast: Option[AST[Parsable[_]]],
    symbolic: Option[A])

trait ParsableTestParameters[A] {
  def children: Set[Parsable[_]]
  def nonterminals: Set[Parsable[_]]
  def tokens: Set[String]
  def productions: Set[Production[Parsable[_]]]
  def cnfProductions: Set[CNFProduction[Parsable[_]]]
  def testParses: List[TestParse[A]]
}

abstract class ParsableTestSuite[A] extends FunSuite {

  val parameters: ParsableTestParameters[A]
  val parsable: Parsable[A]

  test(s"children") {
    assert(parsable.children === parameters.children)
  }

  test(s"grammar productions") {
    assert(parsable.grammar.productions === parameters.productions)
  }

  test(s"CNF productions") {
    assert(parsable.grammar.cnfProductions === parameters.cnfProductions)
  }

  test(s"nonterminals") {
    assert(parsable.grammar.nonterminals === parameters.nonterminals)
  }

  test(s"tokens") {
    assert(parsable.allTokens === parameters.tokens)
  }

  test("Test parses") {
    parameters.testParses.foreach {
      case TestParse(string, tokens, astree, symbol) => {

        for {
          str <- string
          tok <- tokens
        } yield assert(parsable.tokenizer.tokenize(str) === tok)

        /*
        def testASTSanity(ast: AST): Unit = {
          ast.children match {
            case Nil => assert(ast.production === None)
            case xs => {
              ast.production match {
                case None    => assert(false)
                case Some(p) => assert(parsable.productions(p))
                // TODO make work with lexical categories
              }
            }
          }
          // ast.children.foreach(testASTSanity)
        }
        for {
          ast <- astree
        } yield testASTSanity(ast)
        */

        for {
          tok <- tokens
          ast <- astree
        } yield assert(parsable.grammar.parseTokens(tok).head === ast)

        for {
          str <- string
          ast <- astree
        } yield assert(
          parsable.grammar.parseTokens(parsable.tokenizer.tokenize(str)).head === ast)

        for {
          ast <- astree
          sym <- symbol
        } yield assert(parsable.fromAST(ast) === Some(sym))

        for {
          tok <- tokens
          sym <- symbol
        } yield assert(parsable.fromAST(parsable.grammar.parseTokens(tok).head) === Some(sym))

        for {
          str <- string
          sym <- symbol
        } yield assert(parsable.fromString(str) === Set(sym))

      }
    }
  }
}