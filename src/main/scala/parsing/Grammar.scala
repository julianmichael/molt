package parsing

import util.Memoize
// encodes a single production in a grammar. Also gives us
// a convenient String representation.
trait Production {
  def label: String
  def children: List[String]
  def toCNF: Set[CNFProduction]
  override def toString = s"$label -> ${children.mkString(" ")}"
}
object Production {
  def fromString(s: String): Option[Production] = {
    val tokens = s.split("\\s+")
    if (tokens.length < 3 || tokens(1) != "->")
      None
    else {
      Some(RawProduction(tokens(0), tokens.drop(2).toList))
    }
  }
}

// for any general production with a bunch of children
case class RawProduction(
  label: String,
  children: List[String])
  extends Production {

  private def makeChunkedSymbol(symbols: List[String]) = symbols match {
    case Nil         => ""
    case head :: Nil => head
    case list        => s"{${list.mkString("+")}}"
  }
  private def collapseChildren(symbols: List[String]): Set[CNFProduction] = symbols match {
    case Nil | _ :: Nil => Set()
    case left :: right :: Nil => {
      val name = makeChunkedSymbol(symbols)
      Set(ChunkedBinary(name, left, right))
    }
    case head :: tail => {
      val name = makeChunkedSymbol(symbols)
      val left = head
      val right = makeChunkedSymbol(tail)
      collapseChildren(tail) + ChunkedBinary(name, left, right)
    }
  }

  override lazy val toCNF: Set[CNFProduction] = children match {
    case child :: Nil         => Set(Unary(label, child))
    case left :: right :: Nil => Set(Binary(label, left, right))
    case left :: remainder => {
      val nextHead = makeChunkedSymbol(remainder)
      collapseChildren(remainder) + Binary(label, left, nextHead)
    }
  }
}

// for all productions guaranteed to be in Chomsky Normal Form
// plus unary productions
sealed abstract class CNFProduction extends Production {
  lazy val toCNF = Set(this)
}
case class ChunkedBinary(label: String, left: String, right: String) extends CNFProduction {
  lazy val children = List(left, right)
}
case class Binary(label: String, left: String, right: String) extends CNFProduction {
  lazy val children = List(left, right)
}
case class Unary(label: String, child: String) extends CNFProduction {
  lazy val children = List(child)
}

trait AST {
  def label: String
  def children: List[AST]
  def isLeaf = children.isEmpty
  def production: Option[Production] =
    if (isLeaf) None
    else Some(RawProduction(label, children.map(_.label)))
  override def toString: String = children match {
    case Nil          => label
    case child :: Nil => s"$label($child)"
    case children     => s"$label(${children.mkString(",")})"
  }
  // borrowed from dhgarrette
  def pretty: String = prettyLines.mkString("\n")
  private def prettyLines: List[String] = {
    children.flatMap(_.prettyLines) match {
      case List(childLine) => List(label + " " + childLine)
      case childLines      => label :: childLines.map("  " + _)
    }
  }
}

/*
 * Most general AST designed to be produced by a CFG parser
 */
case class BasicAST(
  val label: String,
  val children: List[BasicAST]) extends AST

/*
 * AST with only binary and unary productions (encoded in
 * the types) to be used in parsing a CNF* grammar.
 * Distinction is made between productions native to the
 * grammar and nonterminals that were chunked together by the
 * conversion from CFG to CNF grammar. We also have `undo`
 * which converts back to a BasicAST.
 */
sealed abstract class CNFGrammarAST extends AST {
  def flattened: List[CNFGrammarAST]
  def undo: Option[BasicAST]
}
case class CNFParent(
  val cnfProduction: CNFProduction,
  val children: List[CNFGrammarAST]) extends CNFGrammarAST {
  def label = cnfProduction.label
  lazy val flattened = cnfProduction match {
    case ChunkedBinary(_, _, _) => children.flatMap(_.flattened)
    case _                      => this :: Nil
  }
  lazy val undo = cnfProduction match {
    case ChunkedBinary(_, _, _) => None
    case _                      => Some(BasicAST(label, children.flatMap(_.flattened).map(_.undo).flatten))
  }
}
case class CNFLeaf(val label: String) extends CNFGrammarAST {
  val children = Nil
  val flattened = this :: Nil
  val undo = Some(BasicAST(label, Nil))
}

// Contains everything we need in order to parse, and also parses!
// We let the stuff that appears in the productions inform us what
// symbols are terminal and non-terminal. However, we allow that
// one of the terminal symbols may be ambiguous and allow ANY token
// to be its child. This allows a formula with any propositional
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
