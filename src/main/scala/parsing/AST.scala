package parsing

trait AST {
  def label: String
  def children: List[AST]
  def isLeaf = children.isEmpty
  def production: Option[Production] =
    if (isLeaf) None
    else Some(RawProduction(label, children.map(_.label)))
  override def toString: String = children match {
    case Nil          => label
    case child :: Nil => s"$label($child)"
    case children     => s"$label(${children.mkString(",")})"
  }
  // borrowed from dhgarrette
  def pretty: String = prettyLines.mkString("\n")
  private def prettyLines: List[String] = {
    children.flatMap(_.prettyLines) match {
      case List(childLine) => List(label + " " + childLine)
      case childLines      => label :: childLines.map("  " + _)
    }
  }
}

/*
 * Most general AST designed to be produced by a CFG parser
 */
case class BasicAST(
  val label: String,
  val children: List[BasicAST]) extends AST

/*
 * AST with only binary and unary productions (encoded in
 * the types) to be used in parsing a CNF* grammar.
 * Distinction is made between productions native to the
 * grammar and nonterminals that were chunked together by the
 * conversion from CFG to CNF grammar. We also have `undo`
 * which converts back to a BasicAST.
 */
sealed abstract class CNFGrammarAST extends AST {
  def flattened: List[CNFGrammarAST]
  def undo: Option[BasicAST]
}
case class CNFParent(
  val cnfProduction: CNFProduction,
  val children: List[CNFGrammarAST]) extends CNFGrammarAST {
  def label = cnfProduction.label
  lazy val flattened = cnfProduction match {
    case ChunkedBinary(_, _, _) => children.flatMap(_.flattened)
    case _                      => this :: Nil
  }
  lazy val undo = cnfProduction match {
    case ChunkedBinary(_, _, _) => None
    case _                      => Some(BasicAST(label, children.flatMap(_.flattened).map(_.undo).flatten))
  }
}
case class CNFLeaf(val label: String) extends CNFGrammarAST {
  val children = Nil
  val flattened = this :: Nil
  val undo = Some(BasicAST(label, Nil))
}