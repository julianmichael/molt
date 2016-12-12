package molt.syntax.cfg

import molt.syntax.LexicalCategory
import molt.syntax.cnf._

class CFGParser[A](
  cfg: ContextFreeGrammar[A]) {

  val cnfGrammar = cfg.toCNF
  val cnfParser = new CKYParser(cnfGrammar)

  def parseTokens(tokens: Seq[String]) = for {
    cnfParse <- cnfParser.parseTokens(tokens)
    validParse <- convertAST(cnfParse)
  } yield validParse
}
