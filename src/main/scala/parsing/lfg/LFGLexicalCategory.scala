package parsing.lfg

import parsing.LexicalCategory

case class LFGLexicalCategory[S](
  lexicalEntries: Set[LexicalEntry],
  symbol: S) {

  val cfgLexicalCategory = new LexicalCategory[S] {
    val startSymbol = symbol
    val subLexicon = lexicalEntries.map {
      case (word, _) => word
    }
  }
}