package molt.syntax.cnf.smart

import utest._

import molt.syntax._

import molt.syntax.cfg.parsable.ParseCommands._
import molt.syntax.cfg.parsable.CFGParserHelpers._
import molt.syntax.cfg.parsable.CFGParsables._
import molt.syntax.cfg.parsable.GenericParsables._
import molt.syntax.cfg.parsable._
import molt.syntax.cfg._
import molt.syntax.cnf._

object SmartCNFTestSuite extends TestSuite {
  // a s{i,a}mple grammar
  val lexCats = Set(new LexicalCategory[String] {
    val symbol = "A"
    def member(str: String): Boolean = true
  })
  val prods = Set[CNFProduction[String]](
    Unary("A", ASTEmptyTag),
    Unary("A", ASTNormalTag("A")),
    Binary("A", ASTNormalTag("A"), ASTNormalTag("A")))
  val grammar = new CNFGrammar[String](
    productions = prods,
    lexicalCategories = lexCats,
    startSymbols = Set("A"))
  val parser = new SchedulingCYKParser[String](
    grammar,
    new BasicSmartParse[String])

  def testSentence(tokens: List[String], good: Boolean = true) = {
    // println(tokens.mkString(" "))
    val trees = parser.parseTokens(tokens)
    // trees.take(20).toList.foreach(x => println(s"${x.prettyString}\n"))
  }

  val tests = this {
    "word" - testSentence(List("word"))
    "two words" - testSentence(List("two", "words"))
    "THREE WHOLE WORDS!" - testSentence(List("THREE", "WHOLE", "WORDS!"))
  }
}
