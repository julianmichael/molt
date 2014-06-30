package parsing

import util.Memoize
// Contains everything we need in order to parse, and also parses!
// We let the stuff that appears in the productions inform us what
// symbols are terminal and non-terminal. However, we allow that
// one of the terminal symbols may be ambiguous and allow ANY token
// to be its child. This allows, say, a formula with any propositional
// signature to be parsed without regard for the particular signature.

// The type parameter A is the type of symbol used in productions and ASTs. It
// will typically be either String or Parsable[_].
class Grammar[A](
  val productions: Set[Production[A]],
  val lexicalCategories: List[LexicalCategory[A]],
  val startSymbol: Option[A] = None) {

  // we change the grammar to Chomsky Normal Form* for parsing
  // * with unary productions 
  lazy val cnfProductions = productions.flatMap(_.toCNF).toSet

  // nonterminals are just everything that appears at the head of a (non-lexical) production
  lazy val nonterminals = productions.map(_.head)

  def validate = {
    // TODO determine if there are cycles in the graph of unary productions. would cause inf. loop
  }

  // parse using the CKY algorithm and a memoized function for the DP table.
  def parseTokens(tokens: List[String]) = {
    def getCellGen(recurse: (((Int, Int)) => Set[CNFAST[A]]))(levelOffset: (Int, Int)): Set[CNFAST[A]] = {
      val (level, offset) = levelOffset
      // the first entry-iteration determines either lexical or binary productions,
      // depending on where we are in the DP table.
      val firstEntryIteration: Set[CNFAST[A]] = {
        if (level == 0) { // lexical
          val tok = tokens(offset)
          (for {
            category <- lexicalCategories
            if category.subLexicon(tok)
          } yield CNFUnaryParent[A](category.startSymbol, CNFLeaf(tok))).toSet
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
              val validLeftEntries = for {
                cnfAst <- leftCell
                tag <- cnfAst.label
                if tag == left
              } yield cnfAst
              val validRightEntries = for {
                cnfAst <- rightCell
                tag <- cnfAst.label
                if tag == right
              } yield cnfAst
              val rootEntries: Set[CNFAST[A]] = for {
                l <- validLeftEntries
                r <- validRightEntries
              } yield label match {
                case NormalTag(name) => CNFBinaryParent[A](name, l, r)
                case ChunkedTag(list) => CNFChunkedParent[A](list, l, r)
              }
              rootEntries
            }
            val binaryEntries = cnfProductions.flatMap {
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
          val unaryEntries: Set[CNFAST[A]] = cnfProductions.flatMap {
            case Unary(NormalTag(label), child) => {
              val validEntries = for {
                cnfAst <- entries
                tag <- cnfAst.label
                if tag == child
              } yield cnfAst
              val rootEntries: Set[CNFAST[A]] =
                validEntries.map(x => CNFUnaryParent[A](label, x))
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
    val topCell = getCell(tokens.length - 1, 0)
    val validParses = topCell.map(_.dechomskify).flatten
    val validProperlyStartingParses = startSymbol match {
      case None => validParses
      case Some(sym) => for {
        ast <- validParses
        tag <- ast.label
        if tag == sym
      } yield ast
    }
    validProperlyStartingParses
  }
}
