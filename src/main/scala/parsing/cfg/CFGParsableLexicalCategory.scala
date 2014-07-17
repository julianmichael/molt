package parsing.cfg

trait CFGParsableLexicalCategory extends LexicalCategory[CFGParsable[_]] with CFGParsable[String] {
  final override val symbol = this
  final override val synchronousProductions =
    Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[String])]()
  final override def fromAST(ast: AST[Parsable[_]]): Option[String] = ast match {
    case ASTTerminal(`symbol`, str) if member(str) => Some(str)
    case _ => None
  }
}
