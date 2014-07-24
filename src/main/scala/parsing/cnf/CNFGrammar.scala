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
  val lexicalCategories: Set[LexicalCategory[A]],
  val startSymbols: Set[A] = Set.empty[A]) extends Grammar[CNFAST[A]] {

  type S = CNFTag[A]

  private[this] val allSymbols =
    productions.flatMap(_.symbols) ++ lexicalCategories.map(cat => CNFNormalTag(cat.symbol))

  private[this] lazy val labelToDerivationsWithHole: Map[CNFTag[A], Set[CNFAST[A]]] = {
    def derivationsWithProhibitedRoots(prohib: Set[CNFTag[A]], subtree: CNFAST[A]): Set[CNFAST[A]] = {
      val symbol = subtree.label
      val oneLevelTrees = productions.flatMap (prod => prod match {
        case Unary(label, child) if child == symbol && !prohib(label) =>
          Set[CNFAST[A]](CNFUnaryNonterminal(label, subtree))
        case Binary(label, left, right) if !prohib(label) => {
          val lefters = if(left == subtree.label) { for {
            emptyRightDerivation <- nonterminalsToNullParseTrees(right)
          } yield CNFBinaryNonterminal(label, subtree, emptyRightDerivation) }
          else Set.empty[CNFAST[A]]
          val righters = if(right == subtree.label) { for {
            emptyLeftDerivation <- nonterminalsToNullParseTrees(left)
          } yield CNFBinaryNonterminal(label, emptyLeftDerivation, subtree) }
          else Set.empty[CNFAST[A]]
          lefters ++ righters
        }
        case _ => Set.empty[CNFAST[A]]
      })
      oneLevelTrees.flatMap(tree =>
        derivationsWithProhibitedRoots(prohib + tree.label, tree)) + subtree
    }
    allSymbols.filter {
      case CNFEmptyTag => false
      case _ => true
    }.map {
      case label => (label, derivationsWithProhibitedRoots(Set(label), CNFHole(label)))
    }.toMap
  }

  private[this] def unitParses(subtree: CNFAST[A]): Set[CNFAST[A]] = {
    val derivations = labelToDerivationsWithHole(subtree.label)
    derivations.flatMap(_.attachAtHole(subtree))
  }

  private[this] val nonterminalsToNullParseTrees: Map[S, Set[CNFAST[A]]] = {
    var symbolToProducers = allSymbols.map(symbol => (symbol, productions.filter {
      case Unary(_, c) if c == symbol => true
      case Binary(_, l, r) if l == symbol || r == symbol => true
      case _ => false
    })).toMap
    var nullable = 
      if(allSymbols(CNFEmptyTag)) Set[(Set[S], CNFAST[A])]((Set.empty[S], CNFEmpty))
      else Set.empty[(Set[S], CNFAST[A])]
    var todo = nullable
    while(!todo.isEmpty) {
      val b = todo.head
      todo = todo - b
      val (prohib, subtree) = b
      symbolToProducers(subtree.label).foreach {
        case Unary(a, _) if !prohib(a) => {
          val entry = (prohib + a, CNFUnaryNonterminal(a, subtree))
          nullable = nullable + entry
          todo = todo + entry
        }
        case Binary(a, x, y) if !prohib(a) => {
          if(x == subtree.label) {
            nullable.foreach {
              case (proh, rightTree) if !proh(a) && rightTree.label == y => {
                val entry = (prohib ++ proh + a, CNFBinaryNonterminal(a, subtree, rightTree))
                nullable = nullable + entry
                todo = todo + entry
              }
              case _ => ()
            }
          }
          if(y == subtree.label) {
            nullable.foreach {
              case (proh, leftTree) if !proh(a) && leftTree.label == x => {
                val entry = (prohib ++ proh + a, CNFBinaryNonterminal(a, leftTree, subtree))
                nullable = nullable + entry
                todo = todo + entry
              }
              case _ => ()
            }
          }
        }
      }
    }
    val nullParses = nullable.map(_._2)
    nullParses.groupBy(_.label).withDefaultValue(Set.empty[CNFAST[A]])
  }

  private[this] val unitAncestors: Map[S, Set[S]] =
    labelToDerivationsWithHole.map { case (sym, set) => (sym, set.map(_.label)) }

  private[this] def cykTable(tokens: Seq[String]): (((Int, Int)) => Set[S]) = {
    def getEntryGen(recurse: (((Int, Int)) => Set[S]))(indices: (Int, Int)): Set[S] = {
      val (level, offset) = indices
      val lexicalOrBinary: Set[S] = {
        if(level == 0) for { // lexical
          cat <- lexicalCategories
          if cat.member(tokens(offset))
        } yield CNFNormalTag(cat.symbol)
        else for { // binary
          (leftIndices, rightIndices) <-
            (1 to level).map(i => ((i - 1, offset), (level - i, offset + i))).toSet
          leftCell = recurse(leftIndices)
          rightCell = recurse(rightIndices)
          Binary(label, leftChild, rightChild) <- productions
          if leftCell(leftChild) && rightCell(rightChild)
        } yield label
      }
      lexicalOrBinary.flatMap(unitAncestors)
    }
    lazy val getEntry: (((Int, Int)) => Set[S]) = Memoize(getEntryGen(getEntry))
    getEntry
  }

  private[this] def makeExtract(
      tokens: Seq[String], table: (((Int, Int)) => Set[S])): (((S, Int, Int)) => Set[CNFAST[A]]) = {
    def extractGen(
        other: (((S, Int, Int)) => Set[CNFAST[A]]))
        (input: (S, Int, Int)): Set[CNFAST[A]] = {
      val (root, level, offset) = input
      other(root, level, offset) ++ (for {
        symb <- table(level, offset)
        tree <- other((symb, level, offset))
        augTree <- unitParses(tree)
        if augTree.label == root
      } yield augTree)
    }
    def extractAuxGen(
        other: (((S, Int, Int)) => Set[CNFAST[A]]))
        (input: (S, Int, Int)): Set[CNFAST[A]] = {
      val (root, level, offset) = input
      if(table(level, offset)(root)) {
        if (level == 0) { // lexical
          val tok = tokens(offset)
          (for {
            category <- lexicalCategories
            if root == CNFNormalTag(category.symbol)
            if category.member(tok)
          } yield CNFTerminal[A](root, tok)).toSet
        }
        else for { // binary
          ((leftLevel, leftOffset), (rightLevel, rightOffset)) <-
            (1 to level).map(i => ((i - 1, offset), (level - i, offset + i))).toSet
          leftSymbol <- table((leftLevel, leftOffset))
          rightSymbol <- table((rightLevel, rightOffset))
          if productions(Binary(root, leftSymbol, rightSymbol))
          leftSubtree <- other((leftSymbol, leftLevel, leftOffset))
          rightSubtree <- other((rightSymbol, rightLevel, rightOffset))
        } yield CNFBinaryNonterminal(root, leftSubtree, rightSubtree)
      }
      else Set.empty[CNFAST[A]]
    }
    lazy val (extract: (((S, Int, Int)) => Set[CNFAST[A]]),
              extractAux: (((S, Int, Int)) => Set[CNFAST[A]])) =
      (Memoize(extractGen(extractAux)), Memoize(extractAuxGen(extract)))
    extract
  }

  override def parseTokens(tokens: Seq[String]): Set[CNFAST[A]] = {
    val table = cykTable(tokens)
    val extract = makeExtract(tokens, table)
    startSymbols.map(CNFNormalTag(_)).flatMap(sym => extract((sym, tokens.length - 1, 0)))
  }
}
