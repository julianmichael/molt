package molt.syntax.cfg

import molt.syntax.LexicalCategory
import molt.syntax.cfg._
import molt.syntax.cnf._

import ordered._

// The type parameter A is the type of symbol used in productions and ASTs. It
// will typically be either String or Parsable[_].
class SchedulingCFGParser[A](
  val cfg: ContextFreeGrammar[A],
  val schedulingParams: SmartParseParameters[CNFAST[CNFConversionTag[A]]]) {
  
  val cnfGrammar = cfg.toCNF
  val cnfParser = new SchedulingCYKParser(cnfGrammar, schedulingParams)

  def parseTokens(tokens: Seq[String]) = for {
    cnfParse <- cnfParser.parseTokens(tokens).toStream
    validParse <- convertAST(cnfParse)
  } yield validParse

  def parseTokensFirst(tokens: Seq[String]) = for {
    cnfParse <- cnfParser.parseTokens(tokens) match {
      case ONil() => Stream.empty
      case head :< tail => head #:: tail().takeWhile(tail().order.equiv(head, _)).toStream
    }
    validParse <- convertAST(cnfParse)
  } yield validParse
}

trait CFGSmartParse[A] extends SmartParseParameters[CNFAST[CNFConversionTag[A]]] {
  def score(tree: CNFAST[CNFConversionTag[A]]): Double = convertAST(tree) match {
    case None => (tree: @unchecked) match {
      case CNFBinaryNonterminal(_, left, right) => math.max(score(left), score(right))
      case CNFUnaryNonterminal(_, child) => score(child)
    }
    case Some(ast) => score(ast)
  }

  def score(tree: AST[A]): Double

  implicit val ordering: Ordering[SyntaxTree] = Ordering.by[SyntaxTree, Double](score _)
}

trait PenaltyBasedCFGSmartParse[A] extends CFGSmartParse[A] {
  def score(tree: AST[A]): Double =
    penalties.map(_(tree)).sum + (tree match {
      case ASTNonterminal(_, children) => children.map(score _).sum
      case _ => 0
    })

  val penalties: List[(AST[A] => Double)]
}
