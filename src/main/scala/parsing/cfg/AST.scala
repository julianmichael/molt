package parsing.cfg

sealed abstract class AST[+A] {
  def prettyString: String = this match {
    case ASTNonterminal(head, children) => {
      val childString = children.flatMap(child => {
        child.prettyString.split("\n").map(x => s"  $x")
      }).mkString("\n")
      s"$head\n$childString"
    }
    case ASTTerminal(head, token) => s"$head\t$token"
    case ASTHole(head) => s"$head"
    case ASTEmpty => "<e>"
  }
  def tag: ASTTag[A] = this match {
    case ASTNonterminal(head, _) => ASTNormalTag(head)
    case ASTTerminal(head, _) => ASTNormalTag(head)
    case ASTHole(head) => ASTNormalTag(head)
    case ASTEmpty => ASTEmptyTag
  }
}
case class ASTNonterminal[+A](head: A, children: List[AST[A]]) extends AST[A]
case class ASTTerminal[+A](head: A, token: String) extends AST[A]
case class ASTHole[+A](head: A) extends AST[A]
case object ASTEmpty extends AST[Nothing]
