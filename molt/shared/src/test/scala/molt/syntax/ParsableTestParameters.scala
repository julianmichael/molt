package molt.syntax

import utest._

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

abstract class ParsableTestSuite[A] extends TestSuite {

  val parameters: ParsableTestParameters[A]
  val parsable: CFGParsable[A]

  val tests = this {
    "children" - parameters.children.foreach(_ ==> parsable.children)
    "grammar productions" - parameters.productions.foreach(_ ==> parsable.grammar.productions)
    "CNF productions" - parameters.cnfProductions.foreach(_ ==> parsable.grammar.toCNF.productions)
    "nonterminals" - parameters.nonterminals.foreach(_ ==> parsable.grammar.nonterminals)
    "tokens" - parameters.tokens.foreach(_ ==> parsable.allTokens)
    "Test parses" - {
      parameters.testParses.foreach {
        case TestParse(string, tokens, astree, symbol) => {

          for {
            str <- string
            tok <- tokens
          } yield assert(parsable.tokenizer.tokenizations(str).head == tok)

          /*
           def testASTSanity(ast: AST): Unit = {
           ast.children match {
           case Nil => assert(ast.production == None)
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
            assert(asts.head == ast)
          }

          for {
            str <- string
            ast <- astree
          } yield assert(
            parsable.parser.parseTokens(parsable.tokenizer.tokenizations(str).head).head == ast)

          for {
            ast <- astree
            sym <- symbol
          } yield assert(parsable.fromAST(ast) == Some(sym))

          for {
            tok <- tokens
            sym <- symbol
          } yield {
            val asts = parsable.parser.parseTokens(tok)
            assert(!asts.isEmpty)
            assert(parsable.fromAST(asts.head) == Some(sym))
          }

          for {
            str <- string
            sym <- symbol
          } yield assert(parse(str)(parsable).toSet == Set(sym))

        }
      }
    }
  }
}
