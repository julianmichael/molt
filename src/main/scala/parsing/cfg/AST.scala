package parsing.cfg

sealed abstract class AST[A] {
  val label: A = this match {
    case ASTNonterminal(head, _) => head
    case ASTTerminal(head, _) => head
  }

  def prettyString: String = this match {
    case ASTNonterminal(head, children) => {
      val childString = children.flatMap(child => {
        child.prettyString.split("\n").map(x => s"\t$x")
      }).mkString("\n")
      s"$head\n$childString"
    }
    case ASTTerminal(head, token) => s"$head\t$token"
  }
}
// TODO children should be nonempty list.......maybe? actually maybe not!
// depends on how I want to implement producing the empty string. if at all.
// could either do it with child token "" or no child at all.
case class ASTNonterminal[A](head: A, children: List[AST[A]]) extends AST[A]
case class ASTTerminal[A](head: A, token: String) extends AST[A]
