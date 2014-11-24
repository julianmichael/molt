package molt.syntax.cfg.smart

import molt.syntax.LexicalCategory
import molt.syntax.cfg._
import molt.syntax.cnf._

import sortstreams._

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
}
