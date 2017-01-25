package molt.syntax.cnf

import molt.syntax.cfg.ASTTag
import molt.syntax.cfg.ASTNormalTag
import molt.syntax.cfg.ASTEmptyTag
import molt.util.Memoize
import molt.syntax.cnf._
import molt.syntax.cfg.CNFConversionTag
import CNFConversionTag._

import ordered._

// some silliness hehe. categorification for semiring parsing
// additionally requires the times to respect the product. maybe this is necessary from first principles?
// ...this seems way sketchy actually. hmmm. challenge is to generalize semiring parsing to streams.
// The main interesting thing that I'm trying to generalize is that it's NOT that each production is assigned an element of the semiring.
// rather, it is assigned an ENDOMORPHISM on the semiring (which, as it happens, you can get by multiplying by an element.)
// but now we see that the categorical generalization would simply allow you morphisms to different types. But which ones?
// well, we can product anything in the CKY algorithm, and the shape of what things we have determines what morphisms (production rules) we can apply.
// but we actually ONLY use + when we have the SAME symbol / same type we're working with.
// So we only require that each OBJECT in the category has a COMMUTATIVE SEMIGROUP associated with it.
import scalaz.\/
trait MonoidalCategoryOfCommutativeSemigroups[F[_]] {
  // identity of plus, annihilates times
  def empty[A]: F[A]
  // commutative and associative
  def plus[A](fa1: F[A], fa2: F[A]): F[A]
  // distributes over plus
  def times[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
// a production rule corresponds to a function F[A] -> F[B] (for unary productions) or F[(A, B)] -> F[C] (for binary productions).
// This means If F is a functor, you can define your production rule functions as just A -> B or (A, B) -> C.
// and if you really want to go crazy you can put monadic production stuff on TOP of this... woof.

// note: the above is not quite induced by MonadPlus.
// But it is induced by Const of a Semiring? Seems so to me...

class GeneralizedCKYParser[A](
  // val cnfGrammar: CNFGrammar[A],
  // val schedulingParams: SmartParseParameters[CNFAST[A]]
) {

  // val productions = cnfGrammar.productions
  // val lexicalCategories = cnfGrammar.lexicalCategories
  // val startSymbols = cnfGrammar.startSymbols

  // import schedulingParams._

  // private[this] type Tag = ASTTag[A]

  // private[this] val allTags =
  //   productions.flatMap(_.tags) ++ lexicalCategories.map(x => ASTNormalTag(x.symbol))

  // private[this] val allLabels =
  //   productions.flatMap(_.symbols) ++ lexicalCategories.map(_.symbol)

  // // almost exactly the algorithm from Lange and Leiss
  // private[this] val nullableTags: Set[Tag] = {
  //   var tagToProducers = allTags.map(tag => (tag, productions.filter {
  //     case Unary(_, c) if c == tag => true
  //     case Binary(_, l, r) if l == tag || r == tag => true
  //     case _ => false
  //   })).toMap
  //   var nullable =
  //     if(allTags(ASTEmptyTag)) Set[Tag](ASTEmptyTag)
  //     else Set.empty[Tag]
  //   var todo = nullable
  //   while(!todo.isEmpty) {
  //     val b = todo.head
  //     todo = todo - b
  //     tagToProducers(b).foreach {
  //       case Unary(a, _) if !nullable(ASTNormalTag(a)) => {
  //         nullable = nullable + ASTNormalTag(a)
  //         todo = todo + ASTNormalTag(a)
  //       }
  //       case Binary(a, x, y) if !nullable(ASTNormalTag(a)) => {
  //         if((x == b && nullable(y)) || (y == b && nullable(x))) {
  //           nullable = nullable + ASTNormalTag(a)
  //           todo = todo + ASTNormalTag(a)
  //         }
  //       }
  //       case _ => ()
  //     }
  //   }
  //   nullable
  // }

  // private[this] lazy val nullableTrees: OrderedStream[CNFAST[A]] = {
  //   if(allTags(ASTEmptyTag)) (CNFEmpty(): CNFAST[A]) :< unitNullParses(CNFEmpty())
  //   else OrderedStream.empty
  // }

  // private[this] def unitNullParses(subtree: CNFAST[A]): OrderedStream[CNFAST[A]] = {
  //   // assumption: subtree is a null parse tree
  //   val tag = subtree.tag
  //   // only pair a NULL tree with smaller trees for the first level
  //   val smallerNullableTrees =
  //     if(subtree == CNFEmpty()) OrderedStream.empty
  //     else nullableTrees.takeWhile(_ != subtree)

  //   val oneLevelTreesList = productions.map (prod => prod match {
  //     case Unary(label, child) if(child == tag) =>
  //       OrderedStream.unit[CNFAST[A]](CNFUnaryNonterminal(label, subtree))
  //     case Binary(label, left, right) => {
  //       // need to check that `right` is nullable tag because filtering on a
  //       // predicate that will never be satisfied will cause an infinite loop
  //       val rightNulls = if(left == tag && nullableTags(right)) {
  //         smallerNullableTrees.filter(_.tag == right).map(
  //           nullTree => CNFBinaryNonterminal(label, subtree, nullTree): CNFAST[A])
  //       } else {
  //         OrderedStream.empty[CNFAST[A]]
  //       }

  //       val leftNulls = if(right == tag && nullableTags(left)) {
  //         smallerNullableTrees.filter(_.tag == left).map(
  //           nullTree => CNFBinaryNonterminal(label, nullTree, subtree): CNFAST[A])
  //       } else {
  //         OrderedStream.empty[CNFAST[A]]
  //       }

  //       val doubleNull = if(right == tag && left == tag) {
  //         OrderedStream.unit(CNFBinaryNonterminal(label, subtree, subtree): CNFAST[A])
  //       } else {
  //         OrderedStream.empty[CNFAST[A]]
  //       }

  //       leftNulls.merge(rightNulls).merge(doubleNull)
  //     }
  //     case _ => OrderedStream.empty
  //   })
  //   // TODO can more efficiently take it from an indexedSeq
  //   val oneLevelTrees: OrderedStream[CNFAST[A]] = oneLevelTreesList.foldLeft(OrderedStream.empty)(_ merge _)
  //   oneLevelTrees match {
  //     case ONil() => ONil[CNFAST[A]]
  //     case tree :< remainder => (tree: CNFAST[A]) :< remainder().merge(unitNullParses(tree))
  //   }
  // }

  // private[this] def unitNonNullParses(subtree: CNFAST[A]): OrderedStream[CNFAST[A]] = {
  //   // assumption: subtree is not a null parse tree
  //   val tag = subtree.tag
  //   val oneLevelTreesList = productions.map (prod => prod match {
  //     case Unary(label, child) if(child == tag) =>
  //       OrderedStream.unit[CNFAST[A]](CNFUnaryNonterminal(label, subtree))
  //     case Binary(label, left, right) => {
  //       val rightNulls = if(left == tag && nullableTags(right)) {
  //         nullableTrees.filter(_.tag == right).map(
  //           nullTree => CNFBinaryNonterminal(label, subtree, nullTree): CNFAST[A])
  //       } else {
  //         OrderedStream.empty[CNFAST[A]]
  //       }

  //       val leftNulls = if(right == tag && nullableTags(left)) {
  //         nullableTrees.filter(_.tag == left).map(
  //           nullTree => CNFBinaryNonterminal(label, nullTree, subtree): CNFAST[A])
  //       } else {
  //         OrderedStream.empty[CNFAST[A]]
  //       }

  //       leftNulls.merge(rightNulls)
  //     }
  //     case _ => OrderedStream.empty
  //   })

  //   // TODO once again, more efficiently make it from an IndexedSeq
  //   val oneLevelTrees = oneLevelTreesList.foldLeft(OrderedStream.empty)(_ merge _)
  //   oneLevelTrees match {
  //     case ONil() => ONil[CNFAST[A]]
  //     case tree :< remainder => (tree: CNFAST[A]) :< remainder().merge(unitNonNullParses(tree))
  //   }
  // }

  // def parseTokens(tokens: Seq[String]): OrderedStream[CNFAST[A]] = {
  //   val parses: OrderedStream[CNFAST[A]] = if(tokens.isEmpty) {
  //     nullableTrees
  //   }
  //   else {
  //     val getEntry: (((Int, Int)) => OrderedStream[CNFAST[A]]) = {
  //       def getEntryGen(recurse: (((Int, Int)) => OrderedStream[CNFAST[A]]))(indices: (Int, Int)): OrderedStream[CNFAST[A]] = {
  //         val (level, offset) = indices
  //         val lexicalOrBinary: OrderedStream[CNFAST[A]] = {
  //           if(level == 0) OrderedStream.fromSeq((for { // lexical
  //             cat <- lexicalCategories
  //             if cat.member(tokens(offset))
  //           } yield CNFTerminal(cat.symbol, tokens(offset))).toVector)
  //           else {
  //             val indexPairPairs = (1 to level).map(i => ((i - 1, offset), (level - i, offset + i)))
  //             def treesProducingIndexPair(indexPair: ((Int, Int), (Int, Int))): OrderedStream[CNFAST[A]] = indexPair match { case (leftIndices, rightIndices) =>
  //               val leftCell = recurse(leftIndices)
  //               val rightCell = recurse(rightIndices)

  //               val ord = implicitly[Ordering[CNFAST[A]]]
  //               implicit val pairOrd =
  //                 Ordering.by[(CNFAST[A], CNFAST[A]), CNFAST[A]](pair => ord.max(pair._1, pair._2))

  //               val cellItemPairs = leftCell.flatMap(l => rightCell.map(r => (l, r)))
  //               val treeStream = cellItemPairs.flatMap {
  //                 case (left, right) => {
  //                   val leftTag = left.tag
  //                   val rightTag = right.tag
  //                   OrderedStream.fromSeq(productions.collect {
  //                     case p@Binary(root, `leftTag`, `rightTag`) =>
  //                       CNFBinaryNonterminal(root, left, right): CNFAST[A]
  //                   }.toVector)
  //                 }
  //               }
  //               treeStream
  //             }
  //             indexPairPairs.map(treesProducingIndexPair _).foldLeft(
  //               OrderedStream.empty)(_ merge _)
  //           }
  //         }
  //         lexicalOrBinary match {
  //           case ONil() => ONil[CNFAST[A]]
  //           case tree :< remainder => tree :< remainder().merge(unitNonNullParses(tree))
  //         }
  //       }
  //       lazy val getEntryFunc: (((Int, Int)) => OrderedStream[CNFAST[A]]) = Memoize(getEntryGen(getEntryFunc))
  //       getEntryFunc
  //     }
  //     // indices for the root of the trees in the table
  //     getEntry(tokens.length - 1, 0)
  //   }
  //   parses.filter(ast => ast.tag match {
  //     case ASTNormalTag(sym) => startSymbols(sym)
  //     case ASTEmptyTag => false
  //   })
  // }
}
