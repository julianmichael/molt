package molt.syntax.lfg

import molt.syntax.cfg.CFGParser

class LFGParser[A](
  val lfg: LexicalFunctionalGrammar[A]) {
  
  private[this] val cfgParser = new CFGParser(lfg.cfGrammar)
  private[this] val solve = new LFGSolver(lfg.wildcards)
  
  def parseTokens(tokens: Seq[String]): Set[FStructure] = for {
    ast <- cfgParser.parseTokens(tokens)
    annotatedAST <- lfg.annotations(ast)
    (fdesc, rootID) = annotatedAST.fDescription
    fStruct <- solve(fdesc, rootID)
    if !lfg.requireCompleteness || fStruct.isComplete
    if !lfg.requireCoherence || fStruct.isCoherent(lfg.argumentFunctions)
  } yield fStruct
}