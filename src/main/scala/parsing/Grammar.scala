package parsing

import util.Memoize
// Contains everything we need in order to parse, and also parses!
// We let the stuff that appears in the productions inform us what
// symbols are terminal and non-terminal. However, we allow that
// one of the terminal symbols may be ambiguous and allow ANY token
// to be its child. This allows, say, a formula with any propositional
// signature to be parsed without regard for the particular signature.

// Tokenization is also included: Basically, we find all of the
// terminal symbols and make sure we split on them, then assume
// everything in between is contiguous. As in the definition of
// propositional logic, we are restricting atoms from containing
// any of our canonical terminal symbols.

class Grammar(
  val productions: Set[Production],
  val startSymbol: Option[String] = None,
  val openSymbols: Set[String] = Set()) {

  // we change the grammar to Chomsky Normal Form* for parsing
  // * with unary productions 
  lazy val cnfProductions = productions.flatMap(_.toCNF)

  // nonterminals are just everything that appears at the head of a production
  lazy val nonterminals = productions.map(_.label)

  // anything else (as long as it isn't the wordSymbol) is assumed to be a terminal
  lazy val terminals = productions.flatMap(_.children) -- nonterminals -- openSymbols
  // TODO warn if wordSymbol is not part of the computed set of terminals

  def validate = {
    // TODO warn if terminals can have overlap. leads to ambiguous tokenization
    // TODO determine if there are cycles in the graph of unary productions. would cause inf. loop
    
  }
  
  // tokenization as described above
  def tokenize(s: String) = {
	// split on spaces. this is a reversible design decision.
    val unTokenizedStringVector = s.split("\\s+").toList

    // to turn a single string into a list with the specified terminal split out
    def splitSingleString(str: String, terminal: String): List[String] = {
      if (str.isEmpty)
        Nil
      else if (!str.contains(terminal) || str.equals(terminal))
        List(str)
      else {
        val (head, tail) = str.splitAt(str.indexOf(terminal))
        val remainder = terminal :: splitSingleString(tail.substring(terminal.length), terminal)
        if (head.isEmpty)
          remainder
        else
          head :: remainder
      }
    }

    // does the above function over a list of strings to get a new list with all of the tokens
    def splitOutTerminal(strs: List[String], terminal: String): List[String] = {
      strs.flatMap(splitSingleString(_, terminal))
    }

    // we do the splitting for every terminal and get our final token list 
    val tokens = terminals.foldLeft(unTokenizedStringVector)(splitOutTerminal)
    tokens
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
          if (terminals(tok)) {
            Set(CNFLeaf(tok))
          } else openSymbols.map(
            sym => (CNFParent(Unary(sym, tok), List(CNFLeaf(tok))))
          )
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
  
  def parsings(s: String) = {
    val tokens = tokenize(s)
    parseTokens(tokens)
  }
  
  def parse(s: String): Option[AST] = {
    val parseTrees = parsings(s).toList
    parseTrees match {
      case Nil => None
      case ast :: _ => Some(ast)
    }
  }

}
