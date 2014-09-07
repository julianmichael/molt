package parsing.cnf.smart

import parsing.LexicalCategory
import parsing.Grammar
import parsing.cfg.ASTTag
import parsing.cfg.ASTNormalTag
import parsing.cfg.ASTEmptyTag
import util.Memoize

import parsing.cnf._

import sortstreams._
// Contains everything we need in order to parse, and also parses!
// We let the stuff that appears in the productions inform us what
// symbols are terminal and non-terminal.

// The type parameter A is the type of symbol used in productions and ASTs. It
// will typically be either String or Parsable[_].
class SmartCNFGrammar[A](
  val smartParams: SmartParseParameters[CNFAST[A]],
  val productions: Set[CNFProduction[A]],
  val lexicalCategories: Set[LexicalCategory[A]],
  val startSymbols: Set[A] = Set.empty[A]) {

  import smartParams._

  private[this] type Tag = ASTTag[A]

  private[this] val allTags =
    productions.flatMap(_.tags) ++ lexicalCategories.map(x => ASTNormalTag(x.symbol))

  private[this] val allLabels =
    productions.flatMap(_.symbols) ++ lexicalCategories.map(_.symbol)

  // almost exactly the algorithm from Lange and Leiss
  private[this] val nullableTags: Set[Tag] = {
    var tagToProducers = allTags.map(tag => (tag, productions.filter {
      case Unary(_, c) if c == tag => true
      case Binary(_, l, r) if l == tag || r == tag => true
      case _ => false
    })).toMap
    var nullable =
      if(allTags(ASTEmptyTag)) Set[Tag](ASTEmptyTag)
      else Set.empty[Tag]
    var todo = nullable
    while(!todo.isEmpty) {
      val b = todo.head
      todo = todo - b
      tagToProducers(b).foreach {
        case Unary(a, _) if !nullable(ASTNormalTag(a)) => {
          nullable = nullable + ASTNormalTag(a)
          todo = todo + ASTNormalTag(a)
        }
        case Binary(a, x, y) if !nullable(ASTNormalTag(a)) => {
          if((x == b && nullable(y)) || (y == b && nullable(x))) {
            nullable = nullable + ASTNormalTag(a)
            todo = todo + ASTNormalTag(a)
          }
        }
        case _ => ()
      }
    }
    nullable
  }

  private[this] lazy val nullableTrees: SortedStream[CNFAST[A]] = {
    if(allTags(ASTEmptyTag)) unitNullParses(CNFEmpty())
    else SortedStream.empty
  }

  private[this] def unitNullParses(subtree: CNFAST[A]): SortedStream[CNFAST[A]] = {
    // assumption: subtree is a null parse tree
    val tag = subtree.tag
    // only pair tree with smaller trees
    val smallerNullableTrees =
      if(subtree == CNFEmpty()) SortedStream.empty
      else nullableTrees.takeWhile(_ != subtree)

    val oneLevelTreesList = productions.map (prod => prod match {
      case Unary(label, child) if(child == tag) =>
        SortedStream.unit[CNFAST[A]](CNFUnaryNonterminal(label, subtree))
      case Binary(label, left, right) => {
        // need to check that `right` is nullable tag because filtering on a
        // predicate that will never be satisfied will cause an infinite loop
        val rightNulls = if(left == tag && nullableTags(right)) {
          smallerNullableTrees.filter(_.tag == right).map(
            nullTree => CNFBinaryNonterminal(label, subtree, nullTree): CNFAST[A])
        } else {
          SortedStream.empty[CNFAST[A]]
        }

        val leftNulls = if(right == tag && nullableTags(left)) {
          smallerNullableTrees.filter(_.tag == left).map(
            nullTree => CNFBinaryNonterminal(label, nullTree, subtree): CNFAST[A])
        } else {
          SortedStream.empty[CNFAST[A]]
        }

        val doubleNull = if(right == tag && left == tag) {
          SortedStream.unit(CNFBinaryNonterminal(label, subtree, subtree): CNFAST[A])
        } else {
          SortedStream.empty[CNFAST[A]]
        }

        leftNulls.merge(rightNulls).merge(doubleNull)
        doubleNull
      }
      case _ => SortedStream.empty
    })
    val oneLevelTrees = oneLevelTreesList.foldLeft(SortedStream.empty)(_ merge _)
    // we know that subtree must be the first one, because of monotonicity with
    // respect to tree inclusion, and the fact that all parses of subtree will
    // contain subtree.
    SortedStream.SimpleStream {
      Some(subtree, oneLevelTrees.flatMap(unitParses))
    }
  }

  private[this] def unitNonNullParses(subtree: CNFAST[A]): SortedStream[CNFAST[A]] = {
    // assumption: subtree is not a null parse tree
    val tag = subtree.tag
    val oneLevelTreesList = productions.map (prod => prod match {
      case Unary(label, child) if(child == tag) =>
        SortedStream.unit[CNFAST[A]](CNFUnaryNonterminal(label, subtree))
      case Binary(label, left, right) => {
        val rightNulls = if(left == tag && nullableTags(right)) {
          nullableTrees.filter(_.tag == right).map(
            nullTree => CNFBinaryNonterminal(label, subtree, nullTree): CNFAST[A])
        } else {
          SortedStream.empty[CNFAST[A]]
        }

        val leftNulls = if(right == tag && nullableTags(left)) {
          nullableTrees.filter(_.tag == left).map(
            nullTree => CNFBinaryNonterminal(label, nullTree, subtree): CNFAST[A])
        } else {
          SortedStream.empty[CNFAST[A]]
        }

        leftNulls.merge(rightNulls)
      }
      case _ => SortedStream.empty
    })

    val oneLevelTrees = oneLevelTreesList.foldLeft(SortedStream.empty)(_ merge _)
    SortedStream.SimpleStream {
      Some(subtree, oneLevelTrees.flatMap(unitParses))
    }
  }

  def unitParses(subtree: CNFAST[A]): SortedStream[CNFAST[A]] = {
    if(subtree.isNullParse) unitNullParses(subtree)
    else unitNonNullParses(subtree)
  }

  def parseTokens(tokens: Seq[String]): SortedStream[CNFAST[A]] = {
    val parses: SortedStream[CNFAST[A]] = if(tokens.isEmpty) {
      nullableTrees
    }
    else {
      val getEntry: (((Int, Int)) => SortedStream[CNFAST[A]]) = {
        def getEntryGen(recurse: (((Int, Int)) => SortedStream[CNFAST[A]]))(indices: (Int, Int)): SortedStream[CNFAST[A]] = {
          val (level, offset) = indices
          val lexicalOrBinary: SortedStream[CNFAST[A]] = {
            if(level == 0) SortedStream.fromList((for { // lexical
              cat <- lexicalCategories
              if cat.member(tokens(offset))
            } yield CNFTerminal(cat.symbol, tokens(offset))).toList)
            else {
              val indexPairPairs = (1 to level).map(i => ((i - 1, offset), (level - i, offset + i)))
              def treesProducingIndexPair(indexPair: ((Int, Int), (Int, Int))): SortedStream[CNFAST[A]] = indexPair match { case (leftIndices, rightIndices) =>
                val leftCell = recurse(leftIndices)
                val rightCell = recurse(rightIndices)

                val ord = implicitly[Ordering[CNFAST[A]]]
                implicit val pairOrd =
                  Ordering.by[(CNFAST[A], CNFAST[A]), CNFAST[A]](pair => ord.max(pair._1, pair._2))

                val cellItemPairs = leftCell.flatMap(l => rightCell.map(r => (l, r)))
                val treeStream = cellItemPairs.flatMap {
                  case (left, right) => {
                    val leftTag = left.tag
                    val rightTag = right.tag
                    SortedStream.fromList(productions.collect {
                      case p@Binary(root, `leftTag`, `rightTag`) =>
                        CNFBinaryNonterminal(root, left, right): CNFAST[A]
                    }.toList)
                  }
                }
                treeStream
              }
              indexPairPairs.map(treesProducingIndexPair _).foldLeft(
                SortedStream.empty)(_ merge _)
            }
          }
          lexicalOrBinary.flatMap(unitParses)
        }
        lazy val getEntryFunc: (((Int, Int)) => SortedStream[CNFAST[A]]) = Memoize(getEntryGen(getEntryFunc))
        getEntryFunc
      }
      // indices for the root of the trees in the table
      getEntry(tokens.length - 1, 0)
    }
    parses.filter(ast => ast.tag match {
      case ASTNormalTag(sym) => startSymbols(sym)
      case ASTEmptyTag => false
    })
  }
}
