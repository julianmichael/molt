package molt.syntax.cfg

sealed abstract class AST[+A] {
  def prettyString: String = this match {
    case ASTNonterminal(label, children) => {
      val childString = children.flatMap(child => {
        child.prettyString.split("\n").map(x => s"  $x")
      }).mkString("\n")
      s"$label\n$childString"
    }
    case ASTTerminal(label, token) => s"$label\t$token"
    case ASTHole(label) => s"$label"
    case ASTEmpty => "<e>"
  }
  def tag: ASTTag[A] = this match {
    case ASTNonterminal(label, _) => ASTNormalTag(label)
    case ASTTerminal(label, _) => ASTNormalTag(label)
    case ASTHole(label) => ASTNormalTag(label)
    case ASTEmpty => ASTEmptyTag
  }
}
case class ASTNonterminal[+A](label: A, children: List[AST[A]]) extends AST[A]
case class ASTTerminal[+A](label: A, token: String) extends AST[A]
case class ASTHole[+A](label: A) extends AST[A]
case object ASTEmpty extends AST[Nothing]

sealed abstract class ASTTag[+A] {
  def toOption: Option[A] = this match {
    case ASTNormalTag(x) => Some(x)
    case ASTEmptyTag => None
  }
}
case class ASTNormalTag[+A](label: A) extends ASTTag[A]
case object ASTEmptyTag extends ASTTag[Nothing]
