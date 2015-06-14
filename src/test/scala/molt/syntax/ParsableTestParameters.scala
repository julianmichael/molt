package molt.syntax

import org.scalatest.FunSuite

import molt.syntax.cfg.parsable.ParseCommands._
import molt.syntax.cfg.parsable.CFGParserHelpers._
import molt.syntax.cfg.parsable.CFGParsables._
import molt.syntax.cfg.parsable._
import molt.syntax.cfg._
import molt.syntax.cnf._

case class TestParse[A](
    string: Option[String],
    tokens: Option[List[String]],
    ast: Option[AST[CFGParsable[_]]],
    symbolic: Option[A])

trait ParsableTestParameters[A] {
  def children: Option[Set[CFGParsable[_]]]
  def nonterminals: Option[Set[CFGParsable[_]]]
  def tokens: Option[Set[String]]
  def productions: Option[Set[CFGProduction[CFGParsable[_]]]]
  def cnfProductions: Option[Set[CNFProduction[CNFConversionTag[CFGParsable[_]]]]]
  def testParses: List[TestParse[A]]
}

abstract class ParsableTestSuite[A] extends FunSuite {

  val parameters: ParsableTestParameters[A]
  val parsable: CFGParsable[A]

  test(s"children") { parameters.children foreach (children => 
    assert(parsable.children === children)
  )}

  test(s"grammar productions") { parameters.productions foreach (productions =>
    assert(parsable.grammar.productions === productions)
  )}

  test(s"CNF productions") { parameters.cnfProductions foreach (cnfProductions =>
    assert(parsable.grammar.toCNF.productions === cnfProductions)
  )}

  test(s"nonterminals") { parameters.nonterminals foreach (nonterminals =>
    assert(parsable.grammar.nonterminals === nonterminals)
  )}

  test(s"tokens") { parameters.tokens foreach (tokens =>
    assert(parsable.allTokens === tokens)
  )}

  test("Test parses") {
    parameters.testParses.foreach {
      case TestParse(string, tokens, astree, symbol) => {

        for {
          str <- string
          tok <- tokens
        } yield assert(parsable.tokenizer.tokenizations(str).head === tok)

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
        } yield {
          val asts = parsable.parser.parseTokens(tok)
          assert(!asts.isEmpty)
          assert(asts.head === ast)
        }

        for {
          str <- string
          ast <- astree
        } yield assert(
          parsable.parser.parseTokens(parsable.tokenizer.tokenizations(str).head).head === ast)

        for {
          ast <- astree
          sym <- symbol
        } yield assert(parsable.fromAST(ast) === Some(sym))

        for {
          tok <- tokens
          sym <- symbol
        } yield {
          val asts = parsable.parser.parseTokens(tok)
          assert(!asts.isEmpty)
          assert(parsable.fromAST(asts.head) === Some(sym))
        }

        for {
          str <- string
          sym <- symbol
        } yield assert(parse(str)(parsable).toSet === Set(sym))

      }
    }
  }
}