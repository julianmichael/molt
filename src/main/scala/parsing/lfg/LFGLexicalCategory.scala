package parsing.lfg

import parsing.LexicalCategory

class LFGLexicalCategory[S](
  lexicalEntries: Set[LexicalEntry],
  symbol: S) {

  val cfgLexicalCategory = new LexicalCategory[S] {
    val startSymbol = symbol
    val subLexicon = lexicalEntries.map {
      case LexicalEntry(word, _) => word
    }
  }
}