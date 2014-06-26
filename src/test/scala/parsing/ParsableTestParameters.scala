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

object ParsableTestSuite extends FunSuite {
  def testParsableWithParameters[A](params: ParsableTestParameters[A])(parsable: Parsable[A]) = {
    import params._
    test(s"$parsable children") {
      assert(parsable.children === children)
    }
    test(s"$parsable grammar productions") {
      assert(parsable.grammar.productions === productions)
    }
    test(s"$parsable CNF productions") {
      assert(parsable.grammar.cnfProductions === cnfProductions)
    }
    test(s"$parsable nonterminals") {
      assert(parsable.grammar.nonterminals === nonterminals)
    }
    test(s"$parsable tokens") {
      assert(parsable.allTokens === tokens)
    }

    def testASTSanity(ast: AST): Unit = {
      test(s"AST $ast has properly formed productions") {
        ast.children match {
          case Nil => assert(ast.production === None)
          case xs => {
            ast.production match {
              case None    => assert(false)
              case Some(p) => assert(productions(p)) // TODO make work with lexical categories
            }
          }
        }
      }
      //      ast.children.foreach(testASTSanity)
    }

    testParses.foreach {
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
  }
}