package parsing.cnf

import parsing.LexicalCategory
import parsing.Grammar
import util.Memoize
// Contains everything we need in order to parse, and also parses!
// We let the stuff that appears in the productions inform us what
// symbols are terminal and non-terminal. However, we allow that
// one of the terminal symbols may be ambiguous and allow ANY token
// to be its child. This allows, say, a formula with any propositional
// signature to be parsed without regard for the particular signature.

// The type parameter A is the type of symbol used in productions and ASTs. It
// will typically be either String or Parsable[_].
class CNFGrammar[A](
  val productions: Set[CNFProduction[A]],
  val lexicalCategories: List[LexicalCategory[A]],
  val startSymbol: Option[A] = None) extends Grammar[CNFAST[A]] {

  val warning = {
    // TODO determine if there are cycles in the graph of unary productions. would cause inf. loop
  }

  // parse using the CKY algorithm and a memoized function for the DP table.
  override def parseTokens(tokens: Seq[String]) = {
    def getCellGen(recurse: (((Int, Int)) => Set[CNFAST[A]]))(levelOffset: (Int, Int)): Set[CNFAST[A]] = {
      val (level, offset) = levelOffset
      // the first entry-iteration determines either lexical or binary productions,
      // depending on where we are in the DP table.
      val firstEntryIteration: Set[CNFAST[A]] = {
        if (level == 0) { // lexical
          val tok = tokens(offset)
          (for {
            category <- lexicalCategories
            if category.member(tok)
          } yield CNFTerminal[A](category.symbol, tok)).toSet
        } else { // binary
          // pivotPairs: a list of all of the pairs of table cells that could correspond
          // to the children of the current table cell
          val pivotPairs = (1 to level).map(i => ((i - 1, offset), (level - i, offset + i)))
          // productionsForPair gives us all of the ASTs that could be parent to
          // a given pair of table cells
          def productionsForPair(pair: ((Int, Int), (Int, Int))): Set[CNFAST[A]] = {
            val leftCell = recurse(pair._1)
            val rightCell = recurse(pair._2)
            def getBinaryEntries(label: CNFTag[A], left: CNFTag[A], right: CNFTag[A]) = {
              val validLeftEntries = leftCell.filter(_.label == left)
              val validRightEntries = rightCell.filter(_.label == right)
              val rootEntries: Set[CNFAST[A]] = for {
                l <- validLeftEntries
                r <- validRightEntries
              } yield label match {
                case NormalTag(name) => CNFBinaryNonterminal[A](name, l, r)
                case ChunkedTag(list) => CNFChunkedNonterminal[A](list, l, r)
              }
              rootEntries
            }
            val binaryEntries = productions.flatMap {
              case Binary(label, left, right) => {
                getBinaryEntries(label, left, right)
              }
              case _ => {
                Set[CNFAST[A]]()
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
      def unaryEntriesForEntries(entries: Set[CNFAST[A]]): Set[CNFAST[A]] = {
        if (entries.isEmpty) Set()
        else {
          val unaryEntries: Set[CNFAST[A]] = productions.flatMap {
            case Unary(NormalTag(label), child) => {
              val validEntries = entries.filter(_.label == child)
              val rootEntries: Set[CNFAST[A]] =
                validEntries.map(x => CNFUnaryNonterminal[A](label, x))
              rootEntries
            }
            case _ => Set[CNFAST[A]]()
          }
          unaryEntries ++ unaryEntriesForEntries(unaryEntries)
        }
      }
      firstEntryIteration ++ unaryEntriesForEntries(firstEntryIteration)
    }

    lazy val getCell: (((Int, Int)) => Set[CNFAST[A]]) = Memoize(getCellGen(getCell))
    val validParses = getCell(tokens.length - 1, 0)
    val validProperlyStartingParses = startSymbol match {
      case None => validParses
      case Some(sym) => validParses.filter(_.label == sym)
    }
    validProperlyStartingParses
  }
}
