package parsing

trait LexicalCategory[S] extends {
  val subLexicon: (String => Boolean)
  val startSymbol: S
  // Lexical categories will always have this structure, looking like POS tags.
  // So even individual terminal symbols will get their own POS-tag type things.
  // I think this makes sense.
  def fromAST(ast: AST[S]): Option[String] = ast match {
    case ASTParent(`startSymbol`, List(ASTLeaf(str))) if subLexicon(str) =>
      Some(str)
    case _ => None
  }
}
