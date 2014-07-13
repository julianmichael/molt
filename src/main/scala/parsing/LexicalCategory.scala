package parsing

trait LexicalCategory[S] {
  def member(str: String): Boolean
  val symbol: S
  // Lexical categories will always have this structure, looking like POS tags.
  // So even individual terminal symbols will get their own POS-tag type things.
  // I think this makes sense. We don't lose any expressivity.
  def fromAST(ast: AST[S]): Option[String] = ast match {
    case ASTTerminal(`symbol`, str) if member(str) =>
      Some(str)
    case _ => None
  }
}

/*****
 * Parsable type lexical categories
****/

// lexical category consisting only of one string
case class Terminal(token: String)
  extends ParsableLexicalCategory(Set(token)) {
  override val tokens = Set(token)
}

class RegexLexicalCategory(regex: String)
  extends ParsableLexicalCategory(_.matches(regex))
object RegexLexicalCategory {
  def apply(regex: String): RegexLexicalCategory =
    new RegexLexicalCategory(regex)
}

case object Alphabetical
  extends RegexLexicalCategory("[a-zA-Z]+")
