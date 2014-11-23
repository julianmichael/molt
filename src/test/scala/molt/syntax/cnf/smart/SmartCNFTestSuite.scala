package molt.syntax.cnf.smart

import org.scalatest.FunSuite
import molt.syntax._
import molt.syntax.ParseCommands._
import molt.syntax.cfg.GenericParsables._
import molt.syntax.cfg.CFGParserHelpers._
import molt.syntax.cfg.CFGParsables._
import molt.syntax.cfg._
import molt.syntax.cnf._

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