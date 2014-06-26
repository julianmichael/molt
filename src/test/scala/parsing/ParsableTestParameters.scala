package parsing

import org.scalatest.FunSuite

case class TestParse[A](
    string: Option[String],
    tokens: Option[List[String]],
    ast: Option[AST],
    symbolic: Option[A])

trait ParsableTestParameters[A] {
  def children: Set[Parsable[_]]
    def nonterminals: Set[String]
    def tokens: Set[String]
    def productions: Set[Production]
    def cnfProductions: Set[CNFProduction]
    def testParses: List[TestParse[A]]
}

abstract class ParsableTestSuite[A] extends FunSuite {

  val parameters: ParsableTestParameters[A]
  val parsable: Parsable[A]

  test(s"$parsable children") {
    assert(parsable.children === parameters.children)
  }

  test(s"$parsable grammar productions") {
    assert(parsable.grammar.productions === parameters.productions)
  }

  test(s"$parsable CNF productions") {
    assert(parsable.grammar.cnfProductions === parameters.cnfProductions)
  }

  test(s"$parsable nonterminals") {
    assert(parsable.grammar.nonterminals === parameters.nonterminals)
  }

  test(s"$parsable tokens") {
    assert(parsable.allTokens === parameters.tokens)
  }

  /*
  parameters.testParses.foreach {
    case TestParse(string, tokens, astree, symbol) => {

      for {
        str <- string
        tok <- tokens
      } yield test(s"$string tokenizing") {
        assert(parsable.tokenizer.tokenize(str) === tok)
      }

      for {
        tok <- tokens
        ast <- astree
      } yield test(s"AST for $parsable tokens $tok") {
        assert(parsable.grammar.parseTokens(tok).head === ast)
      }

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
        //      ast.children.foreach(testASTSanity)
      }
      for {
        ast <- astree
      } yield testASTSanity(ast)

      for {
        ast <- astree
        sym <- symbol
      } yield test(s"Symbolic representation of AST for $parsable $symbol") {
        assert(parsable.fromAST(ast) === Some(sym))
      }

      for {
        str <- string
        sym <- symbol
      } yield test(s"Symbolic representation of $parsable $string") {
        assert(parsable.fromString(str) === Set(sym))
      }

    }
  }
  */
  test("Test parses") {
    parameters.testParses.foreach {
      case TestParse(string, tokens, astree, symbol) => {

        for {
          str <- string
          tok <- tokens
        } yield assert(parsable.tokenizer.tokenize(str) === tok)

        for {
          tok <- tokens
          ast <- astree
        } yield assert(parsable.grammar.parseTokens(tok).head === ast)

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
          //      ast.children.foreach(testASTSanity)
        }
        for {
          ast <- astree
        } yield testASTSanity(ast)

        for {
          ast <- astree
          sym <- symbol
        } yield assert(parsable.fromAST(ast) === Some(sym))

        for {
          str <- string
          sym <- symbol
        } yield assert(parsable.fromString(str) === Set(sym))

      }
    }
  }
}