package parsing

import util.Memoize
// Contains everything we need in order to parse, and also parses!
// We let the stuff that appears in the productions inform us what
// symbols are terminal and non-terminal. However, we allow that
// one of the terminal symbols may be ambiguous and allow ANY token
// to be its child. This allows, say, a formula with any propositional
// signature to be parsed without regard for the particular signature.
class Grammar(
  val productions: Set[Production],
  val preLexicalCategories: Set[LexicalCategory],
  val startSymbol: Option[String] = None) {

  // TODO find some solution other than this hackish one to make Word match only
  // the unmatched nonterminals
  // once it's fixed, change the original parameter back to lexicalCategories
  // and get rid of the nonsense below during the level 0 firstEntryIteration.
  val hasWord = preLexicalCategories(Word)
  val lexicalCategories = preLexicalCategories - Word

  // we change the grammar to Chomsky Normal Form* for parsing
  // * with unary productions 
  lazy val cnfProductions = productions.flatMap(_.toCNF)

  // nonterminals are just everything that appears at the head of a (non-lexical) production
  lazy val nonterminals = productions.map(_.label)

  def validate = {
    // TODO determine if there are cycles in the graph of unary productions. would cause inf. loop
  }

  // parse using the CKY algorithm and a memoized function for the DP table.
  def parseTokens(tokens: List[String]) = {
    def getCellGen(recurse: (((Int, Int)) => Set[CNFGrammarAST]))(levelOffset: (Int, Int)): Set[CNFGrammarAST] = {
      val (level, offset) = levelOffset
      // the first entry-iteration determines either lexical or binary productions,
      // depending on where we are in the DP table.
      val firstEntryIteration: Set[CNFGrammarAST] = {
        if (level == 0) { // lexical
          val tok = tokens(offset)
          // (hack for Word is continued here)
          val lexcats: Set[CNFGrammarAST] = for {
            category <- lexicalCategories
            if category.subLexicon(tok)
          } yield CNFParent(Unary(category.startSymbol, tok), List(CNFLeaf(tok)))
          if(lexcats.isEmpty)
            Set[CNFGrammarAST](CNFParent(Unary(Word.startSymbol, tok), List(CNFLeaf(tok))))
          else
            lexcats
          // (end Word hack)
        } else { // binary
          // pivotPairs: a list of all of the pairs of table cells that could correspond
          // to the children of the current table cell
          val pivotPairs = (1 to level).map(i => ((i - 1, offset), (level - i, offset + i)))
          // productionsForPair gives us all of the ASTs that could be parent to
          // a given pair of table cells
          def productionsForPair(pair: ((Int, Int), (Int, Int))): Set[CNFGrammarAST] = {
            val leftCell = recurse(pair._1)
            val rightCell = recurse(pair._2)
            def getBinaryEntries(prod: CNFProduction, left: String, right: String) = {
              val validLeftEntries = leftCell.filter(_.label == left)
              val validRightEntries = rightCell.filter(_.label == right)
              val rootEntries: Set[CNFGrammarAST] = for (l <- validLeftEntries; r <- validRightEntries) yield {
                CNFParent(prod, List(l, r))
              }
              rootEntries
            }
            val binaryEntries = cnfProductions.flatMap {
              case prod @ Binary(_, left, right) => {
                getBinaryEntries(prod, left, right)
              }
              case prod @ ChunkedBinary(_, left, right) => {
                getBinaryEntries(prod, left, right)
              }
              case _ => {
                Set[CNFGrammarAST]()
              }
            }
            binaryEntries
          }
          val allBinaryEntries = pivotPairs.flatMap(productionsForPair).toSet
          allBinaryEntries
        }
      }

      // now after out first brush, we need to repeatedly iterate on our possible ASTs
      // to see if they can be produced by yet higher unary production-entries.
      def unaryEntriesForEntries(entries: Set[CNFGrammarAST]): Set[CNFGrammarAST] = {
        if (entries.isEmpty)
          Set()
        else {
          val unaryEntries: Set[CNFGrammarAST] = cnfProductions.flatMap {
            case prod @ Unary(_, child) => {
              val validEntries = entries.filter(_.label == child)
              val rootEntries: Set[CNFGrammarAST] = validEntries.map(x => CNFParent(prod, List(x)))
              rootEntries
            }
            case _ => Set[CNFGrammarAST]()
          }
          unaryEntries ++ unaryEntriesForEntries(unaryEntries)
        }
      }
      firstEntryIteration ++ unaryEntriesForEntries(firstEntryIteration)
    }

    lazy val getCell: (((Int, Int)) => Set[CNFGrammarAST]) = Memoize(getCellGen(getCell))
    val topCell = getCell(tokens.length - 1, 0)
    val validParses = topCell.map(_.undo).flatten
    val validProperlyStartingParses = startSymbol match {
      case None => validParses
      case Some(sym) => validParses.filter(_.label == sym)
    }
    validProperlyStartingParses
  }
}
