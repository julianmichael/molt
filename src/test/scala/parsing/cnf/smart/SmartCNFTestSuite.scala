package parsing.cnf.smart

import org.scalatest.FunSuite
import parsing._
import parsing.ParseCommands._
import parsing.cfg.GenericParsables._
import parsing.cfg.CFGParserHelpers._
import parsing.cfg.CFGParsables._
import parsing.cfg.CFGProduction
import parsing.cfg._
import parsing.cnf._

class SmartCNFTestSuite extends FunSuite {
  // a s{i,a}mple grammar
  val lexCats = Set(new LexicalCategory[String] {
    val symbol = "A"
    def member(str: String): Boolean = true
  })
  val productions = Set[CNFProduction[String]](
    Unary("A", ASTEmptyTag),
    Unary("A", ASTNormalTag("A")),
    Binary("A", ASTNormalTag("A"), ASTNormalTag("A")))
  val grammar = new SmartCNFGrammar[String](
    new BasicSmartParse[String],
    productions = productions,
    lexicalCategories = lexCats,
    startSymbols = Set("A"))

  def testSentence(tokens: List[String], good: Boolean = true) = {
    println(tokens.mkString(" "))
    val trees = grammar.parseTokens(tokens)
    trees.take(20).toList.foreach(x => println(s"${x.prettyString}\n"))
  }

  test(s"word") {
    testSentence(List("word"))
  }

  test(s"two words") {
    testSentence(List("two", "words"))
  }

  test(s"THREE WHOLE WORDS!") {
    testSentence(List("THREE", "WHOLE", "WORDS!"))
  }

}