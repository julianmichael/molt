package molt
package syntax

import scala.annotation.tailrec

import scalaz.Heap

import shapeless._
import UnaryTCConstraint._
import LUBConstraint._
import ops.hlist._

import molt.util.DependentMap
import molt.util.MutableDependentMap

import ordered._

/** This is a temporary package for my new approach to agenda-based parsing of CFGs */
package object agenda {

  // why is this not in scalaz? TODO maybe I just need to import it
  implicit def scalazOrder[A : Ordering]: scalaz.Order[A] =
    scalaz.Order.fromScalaOrdering[A]

  // equals is NOT done by string---we don't want to cause unwanted collisions
  class ParseSymbol[A](label: String) {
    override def toString = label
  }
  object Terminal {
    private[this] case class Terminal(val token: String) extends ParseSymbol[String](token)
    def apply(token: String): ParseSymbol[String] = new Terminal(token)
  }


  case class Scored[A](a: A, score: Double)
  implicit def scoreOrdering[A]: Ordering[Scored[A]] = Ordering.by[Scored[A], Double](_.score)

  /* One of the main datatypes in the parser; also involved in how we translate a CFG */
  sealed trait Derivation {
    type Result
    val symbol: ParseSymbol[Result]
    val item: Result
    val score: Double
  }
  object Derivation {
    implicit val ordering: Ordering[Derivation] = Ordering.by[Derivation, Double](_.score)
    implicit def anyOrdering[R]: Ordering[Derivation { type Result = R }] = Ordering.by[Derivation { type Result = R }, Double](_.score)

    private[this] case class DerivationImpl[A](
      override val symbol: ParseSymbol[A],
      override val item: A,
      override val score: Double) extends Derivation {
      override type Result = A
    }

    def apply[A](symbol: ParseSymbol[A], item: A, score: Double): Derivation = DerivationImpl(symbol, item, score)
    def unapply(d: Derivation): Some[(ParseSymbol[d.Result], d.Result, Double)] = Some((d.symbol, d.item, d.score))
  }

  import SyncCNFProduction._

  // TODO maybe change combinator type members to type parameters
  sealed trait CNFCombinator
  sealed trait UnaryCombinator extends CNFCombinator {
    type Child
    def childSymbol: ParseSymbol[Child]
    def productions: Vector[Unary[Child, _]]
    def apply(d: Derivation { type Result = Child }): OrderedStream[Derivation]
  }
  object UnaryCombinator {
    def apply[C](childSymbol: ParseSymbol[C], productions: Vector[Unary[C, _]]): UnaryCombinator { type Child = C } =
      UnaryCombinatorImpl(childSymbol, productions)

    private[this] case class UnaryCombinatorImpl[C](
      override val childSymbol: ParseSymbol[C],
      override val productions: Vector[Unary[C, _]]
    ) extends UnaryCombinator {
      override type Child = C
      override def apply(d: Derivation { type Result = Child }): OrderedStream[Derivation] = d match {
        // assume the symbol is the same TODO maybe check it again..
        case Derivation(_, child, score) =>
          val vecOfStreams = for {
            p <- productions
            scoredResults <- p.construct.lift(child :: HNil)
            resultDerivations = scoredResults.mapMonotone {
              // not really sure why we need this cast...sigh...
              case Scored(result, penalty) => Derivation(p.parentSymbol.asInstanceOf[ParseSymbol[Any]], result, score + penalty)
            }
          } yield resultDerivations
          OrderedStream.fromIndexedSeq[OrderedStream[Derivation]](vecOfStreams).flatten
      }
    }
  }

  sealed trait BinaryCombinator {
    type Left
    type Right
    def leftSymbol: ParseSymbol[Left]
    def rightSymbol: ParseSymbol[Right]
    def productions: Vector[Binary[Left, Right, _]]
    def apply(left: Derivation { type Result = Left }, right: Derivation { type Result = Right }): OrderedStream[Derivation]
  }
  object BinaryCombinator {
    def apply[L, R](leftSymbol: ParseSymbol[L], rightSymbol: ParseSymbol[R], productions: Vector[Binary[L, R, _]]): BinaryCombinator { type Left = L; type Right = R } =
      BinaryCombinatorImpl(leftSymbol, rightSymbol, productions)

    private[this] case class BinaryCombinatorImpl[L, R](
      override val leftSymbol: ParseSymbol[L],
      override val rightSymbol: ParseSymbol[R],
      override val productions: Vector[Binary[L, R, _]]
    ) extends BinaryCombinator {
      override type Left = L
      override type Right = R
      override def apply(left: Derivation { type Result = Left }, right: Derivation { type Result = Right }): OrderedStream[Derivation] = (left, right) match {
        // assume the symbols are the same TODO maybe check it again..
        case (Derivation(_, leftChild, leftScore), Derivation(_, rightChild, rightScore)) =>
          val vecOfStreams = for {
            p <- productions
            scoredResults <- p.construct.lift(leftChild :: rightChild :: HNil)
            resultDerivations = scoredResults.mapMonotone {
              // not really sure why we need this cast...sigh... TODO figure this out!!!!
              case Scored(result, penalty) => Derivation(p.parentSymbol.asInstanceOf[ParseSymbol[Any]], result, leftScore + rightScore + penalty)
            }
          } yield resultDerivations
          val result = OrderedStream.fromIndexedSeq[OrderedStream[Derivation]](vecOfStreams).flatten[Derivation]
          result
      }
    }
  }

  object addCNFRule extends Poly2 {
    implicit def caseUnary[Child, Parent] =
      at[Unary[Child, Parent], CNFCombinators] { case (u @ Unary(childSymbol, _, _), combinators) =>
        val curProductions = combinators.unary.get(childSymbol).map(_.productions).getOrElse(Vector.empty[Unary[Child, _]])
        val newUnary = combinators.unary.put(childSymbol, UnaryCombinator(childSymbol, u +: curProductions))
        combinators.copy(unary = newUnary)
      }
    implicit def caseBinary[L, R, Parent] =
      at[Binary[L, R, Parent], CNFCombinators] { case (b @ Binary(leftSymbol, rightSymbol, _, _), combinators) =>
        val rightBinaries = combinators.leftThenRightBinary.get(leftSymbol)
          .getOrElse(DependentMap.empty[ParseSymbol, λ[R => BinaryCombinator { type Left = L; type Right = R }]])
        val leftBinaries = combinators.rightThenLeftBinary.get(rightSymbol)
          .getOrElse(DependentMap.empty[ParseSymbol, λ[L => BinaryCombinator { type Left = L; type Right = R }]])
        // could also do with leftBinaries; doesn't matter since contents are the same (just indexed differently)
        val binaryCombinator = rightBinaries.get(rightSymbol)
          .getOrElse(BinaryCombinator(leftSymbol, rightSymbol, Vector.empty))

        val newLeftBinaries  =  leftBinaries.put(leftSymbol,  BinaryCombinator(leftSymbol, rightSymbol, b +: binaryCombinator.productions))
        val newRightBinaries = rightBinaries.put(rightSymbol, BinaryCombinator(leftSymbol, rightSymbol, b +: binaryCombinator.productions))
        val newLeftThenRightBinary = combinators.leftThenRightBinary.put(leftSymbol, newRightBinaries)
        val newRightThenLeftBinary = combinators.rightThenLeftBinary.put(rightSymbol, newLeftBinaries)
        combinators.copy(
          leftThenRightBinary = newLeftThenRightBinary,
          rightThenLeftBinary = newRightThenLeftBinary)
      }
  }

  case class CNFCombinators(
    val unary: DependentMap[ParseSymbol, λ[C => UnaryCombinator { type Child = C }]],
    val leftThenRightBinary: DependentMap[ParseSymbol, λ[L => DependentMap[ParseSymbol, λ[R => BinaryCombinator { type Left = L; type Right = R }]]]],
    val rightThenLeftBinary: DependentMap[ParseSymbol, λ[R => DependentMap[ParseSymbol, λ[L => BinaryCombinator { type Left = L; type Right = R }]]]])
  object CNFCombinators {
    val empty = CNFCombinators(
      unary = DependentMap.empty[ParseSymbol, λ[C => UnaryCombinator { type Child = C }]],
      leftThenRightBinary = DependentMap.empty[ParseSymbol, λ[L => DependentMap[ParseSymbol, λ[R => BinaryCombinator { type Left = L; type Right = R }]]]],
      rightThenLeftBinary = DependentMap.empty[ParseSymbol, λ[R => DependentMap[ParseSymbol, λ[L => BinaryCombinator { type Left = L; type Right = R }]]]])
    def fromSyncCNFProductions[AllProductions <: HList](
      cnfProductions: AllProductions)(
      implicit folder: RightFolder.Aux[AllProductions, CNFCombinators, addCNFRule.type, CNFCombinators]
    ): CNFCombinators = {
      cnfProductions.foldRight(empty)(addCNFRule)
    }
  }

  final class Chart(length: Int) {
    // just doing a square because I'm lazy. TODO change
    private[this] val cells = Array.fill(length * length){ new Cell }
    // assume end > begin and they fit in the sentence
    private[this] def cellIndex(begin: Int, end: Int): Int = (begin * length) + end - 1

    def cell(begin: Int, end: Int): Cell = cells(cellIndex(begin, end))
  }

  final class Cell {
    private[this] val map = MutableDependentMap.empty[ParseSymbol, λ[A => Heap[Derivation { type Result = A }]]]
    def getDerivations[A](ps: ParseSymbol[A]): Option[Heap[Derivation { type Result = A }]] = map.get(ps)
    def getDerivationStream[A](ps: ParseSymbol[A]): Option[OrderedStream[Derivation { type Result = A }]] = map.get(ps).map { heap =>
      OrderedStream.unfold(heap, (h: Heap[Derivation { type Result = A}]) => h.uncons)
    }
    def add(derivation: Derivation): Unit = {
      val heap = map.get(derivation.symbol).getOrElse(Heap.Empty[Derivation { type Result = derivation.Result }])
      map.put(derivation.symbol, heap.insert(derivation))
    }
  }

  case class Edge(
    derivation: Derivation,
    begin: Int,
    end: Int)
  object Edge {
    implicit val ordering: Ordering[Edge] = Ordering.by[Edge, Derivation](_.derivation)
  }

  object AgendaBasedSyncCNFParser {
    def buildFromSyncCFG[AllCFGProductions <: HList : <<:[SyncCFGProduction]#λ,
                         AllCNFProductions <: HList](
      genlex: String => OrderedStream[Derivation],
      cfg: SyncCFG[AllCFGProductions])(
      implicit fm: FlatMapper.Aux[transformProduction.type, AllCFGProductions, AllCNFProductions],
      folder: RightFolder.Aux[AllCNFProductions, CNFCombinators, addCNFRule.type, CNFCombinators]
    ): AgendaBasedSyncCNFParser = {
      val cnfProductions = SyncCNFGrammar.productionsFromSyncCFG(cfg)
      val combinators = CNFCombinators.fromSyncCNFProductions(cnfProductions)
      new AgendaBasedSyncCNFParser(genlex, combinators)
    }
  }
  class AgendaBasedSyncCNFParser(
    val genlex: String => OrderedStream[Derivation],
    val combinators: CNFCombinators) {

    def parse[A](tokens: Vector[String], rootSymbol: ParseSymbol[A]): OrderedStream[Derivation { type Result = A }] = {
      val chart = new Chart(tokens.size)
      var agenda = Heap.Empty[:<[Edge]]
      // cache lexical scores for the A* heuristic
      // TODO should I just initialize the agenda with lexical stuff? test this---I suspect it'd be slightly worse to do so.
      val lexicalScores = for ((word, i) <- tokens.zipWithIndex) yield {
        genlex(word).ifNonEmpty match {
          case None => ??? // this shouldn't happen... the stream should never be empty? or we should just return None from here and be done with it
          case Some(dStream) =>
            val edges = dStream.map(d => (Edge(d, i, i + 1)))
            agenda = agenda.insert(edges)
            dStream.head.score
        }
      }

      @tailrec
      def nextRootNode: Option[Derivation { type Result = A }] = agenda.uncons match {
        case None => None
        case Some((headStream, newAgenda)) => headStream match {
          case edge :<+ remainingEdges =>
            agenda = remainingEdges.ifNonEmpty match {
              case None => newAgenda
              case Some(re) => newAgenda.insert(re)
            }
            val Edge(curDeriv, begin, end) = edge
            val symbol = curDeriv.symbol
            val item = curDeriv.item
            val score = curDeriv.score
            chart.cell(begin, end).add(curDeriv)

            // find matching guys and put them in the agenda
            for {
              leftCombinators <- combinators.rightThenLeftBinary.get(symbol).toSeq
              newBegin <- 0 to (begin - 1)
              cell = chart.cell(newBegin, begin)
              leftSymbol <- leftCombinators.keys
              leftCombinator <- leftCombinators.get(leftSymbol)
              leftTargets <- cell.getDerivationStream(leftCombinator.leftSymbol)
              newDerivations = leftTargets.flatMap(leftDeriv => leftCombinator(leftDeriv, curDeriv))
              nonEmptyNewDerivations <- newDerivations.ifNonEmpty
              newEdges = nonEmptyNewDerivations.map(d => Edge(d, newBegin, end))
            } yield agenda = agenda.insert(newEdges)

            for {
              rightCombinators <- combinators.leftThenRightBinary.get(symbol).toSeq
              newEnd <- (end + 1) to tokens.length
              cell = chart.cell(end, newEnd)
              rightSymbol <- rightCombinators.keys
              rightCombinator <- rightCombinators.get(rightSymbol)
              rightTargets <- cell.getDerivationStream(rightCombinator.rightSymbol)
              newDerivations = rightTargets.flatMap(rightDeriv => rightCombinator(curDeriv, rightDeriv))
              nonEmptyNewDerivations <- newDerivations.ifNonEmpty
              newEdges = nonEmptyNewDerivations.map(d => Edge(d, begin, newEnd))
            } yield agenda = agenda.insert(newEdges)

            for {
              unaryCombinator <- combinators.unary.get(symbol).toSeq
              newDerivations = unaryCombinator(curDeriv)
              nonEmptyNewDerivations <- newDerivations.ifNonEmpty
              newEdges = nonEmptyNewDerivations.map(d => Edge(d, begin, end))
            } yield agenda = agenda.insert(newEdges)

            // assume that if the symbol is the same, the type works out... TODO make sure this is ok
            if(begin == 0 && end == tokens.size && symbol == rootSymbol) {
              Some(curDeriv.asInstanceOf[Derivation { type Result = A }])
            } else {
              nextRootNode
            }
        }
      }
      OrderedStream.exhaustively(nextRootNode)
    }
  }


  // WITH TREES!!

  implicit def edgeASTOrdering[A]: Ordering[(Derivation { type Result = A }, EdgeAST)] =
    Ordering.by[(Derivation { type Result = A }, EdgeAST), Double](_._1.score)
  implicit def edgeNodeOrdering[A]: Ordering[EdgeNode] =
    Ordering.by[EdgeNode, Double](_.edge.derivation.score)

  sealed trait EdgeAST {
    def toStringPretty: String = toStringPretty(0)
    def toStringPretty(tabs: Int): String = this match {
      case EdgeTerminal(token, index) => ("\t" * tabs) + token
      case EdgeNode(Edge(Derivation(symbol, item, score), begin, end), children) =>
        ("\t" * tabs) + s"$symbol: $item\n" + children.map(_.toStringPretty(tabs + 1)).mkString("\n")
    }
  }
  case class EdgeTerminal(
    token: String,
    index: Int
  ) extends EdgeAST
  case class EdgeNode(
    edge: Edge,
    children: List[EdgeAST]
  ) extends EdgeAST

  final class TreeChart(length: Int) {
    // just doing a square because I'm lazy. TODO change
    private[this] val cells = Array.fill(length * length){ new TreeCell }
    // assume end > begin and they fit in the sentence
    private[this] def cellIndex(begin: Int, end: Int): Int = (begin * length) + end - 1

    def cell(begin: Int, end: Int): TreeCell = cells(cellIndex(begin, end))
  }

  final class TreeCell {
    private[this] val map = MutableDependentMap.empty[ParseSymbol, λ[A => Heap[(Derivation { type Result = A }, EdgeAST)]]]
    def getDerivations[A](ps: ParseSymbol[A]): Option[Heap[(Derivation { type Result = A }, EdgeAST)]] = map.get(ps)
    def getDerivationStream[A](ps: ParseSymbol[A]): Option[OrderedStream[(Derivation { type Result = A }, EdgeAST)]] = map.get(ps).map { heap =>
      OrderedStream.unfold(heap, (h: Heap[(Derivation { type Result = A}, EdgeAST)]) => h.uncons)
    }
    def add[A](pair: (Derivation { type Result = A }, EdgeAST)): Unit = {
      val heap = map.get(pair._1.symbol).getOrElse(Heap.Empty[(Derivation { type Result = A }, EdgeAST)])
      map.put(pair._1.symbol, heap.insert(pair))
    }
  }

  // TODO: Make one generalized version that encompasses all of the ways you could generalize the initial parser.
  object AgendaBasedSyncCNFParserWithTrees {
    def buildFromSyncCFG[AllCFGProductions <: HList : <<:[SyncCFGProduction]#λ,
                         AllCNFProductions <: HList](
      genlex: String => OrderedStream[Derivation],
      cfg: SyncCFG[AllCFGProductions])(
      implicit fm: FlatMapper.Aux[transformProduction.type, AllCFGProductions, AllCNFProductions],
      folder: RightFolder.Aux[AllCNFProductions, CNFCombinators, addCNFRule.type, CNFCombinators]
    ): AgendaBasedSyncCNFParserWithTrees = {
      val cnfProductions = SyncCNFGrammar.productionsFromSyncCFG(cfg)
      val combinators = CNFCombinators.fromSyncCNFProductions(cnfProductions)
      new AgendaBasedSyncCNFParserWithTrees(genlex, combinators)
    }
  }
  class AgendaBasedSyncCNFParserWithTrees(
    val genlex: String => OrderedStream[Derivation],
    val combinators: CNFCombinators) {

    final class ParseProcessWithTrees(val tokens: Vector[String]) {
      val chart: TreeChart = new TreeChart(tokens.size)
      // immutable agenda means we could easily keep track of the agenda history... could be useful in learning, hypothetically
      var agenda: Heap[:<[EdgeNode]] = Heap.Empty[:<[EdgeNode]]
      // cache lexical scores for the A* heuristic
      // TODO should I just initialize the agenda with lexical stuff? test this---I suspect it'd be slightly worse to do so.
      val lexicalScores = for ((word, i) <- tokens.zipWithIndex) yield {
        genlex(word).ifNonEmpty match {
          case None => ??? // this shouldn't happen... the stream should never be empty? or we should just return None from here and be done with it
          case Some(dStream) =>
            val edgeNodes = dStream.mapMonotone(d => (EdgeNode(Edge(d, i, i + 1), List[EdgeAST](EdgeTerminal(word, i)))))
            agenda = agenda.insert(edgeNodes)
            edgeNodes.head.edge.derivation.score
        }
      }

      def step: Option[EdgeNode]= agenda.uncons map {
        case (headStream, newAgenda) => headStream match {
          case edgeNode :<+ remainingEdges =>
            agenda = remainingEdges.ifNonEmpty match {
              case None => newAgenda
              case Some(re) => newAgenda.insert(re)
            }
            val Edge(curDeriv, begin, end) = edgeNode.edge
            val symbol = curDeriv.symbol
            val item = curDeriv.item
            val score = curDeriv.score
            chart.cell(begin, end).add((curDeriv: Derivation { type Result = curDeriv.Result }, edgeNode))

            for {
              leftCombinators <- combinators.rightThenLeftBinary.get(symbol).toSeq
              newBegin <- 0 to (begin - 1)
              cell = chart.cell(newBegin, begin)
              leftSymbol <- leftCombinators.keys
              leftCombinator <- leftCombinators.get(leftSymbol)
              leftTargets <- cell.getDerivationStream(leftCombinator.leftSymbol)
              newEdgeASTs = leftTargets.flatMap {
                case (leftDeriv, leftEdgeAST) =>
                  val derivStream = leftCombinator(leftDeriv, curDeriv)
                  derivStream.mapMonotone(deriv => EdgeNode(Edge(deriv, newBegin, end), List(leftEdgeAST, edgeNode)))
              }
              nonEmptyNewEdgeASTs <- newEdgeASTs.ifNonEmpty
            } yield agenda = agenda.insert(nonEmptyNewEdgeASTs)

            for {
              rightCombinators <- combinators.leftThenRightBinary.get(symbol).toSeq
              newEnd <- (end + 1) to tokens.length
              cell = chart.cell(end, newEnd)
              rightSymbol <- rightCombinators.keys
              rightCombinator <- rightCombinators.get(rightSymbol)
              rightTargets <- cell.getDerivationStream(rightCombinator.rightSymbol)
              newEdgeASTs = rightTargets.flatMap {
                case (rightDeriv, rightEdgeAST) =>
                  val derivStream = rightCombinator(curDeriv, rightDeriv)
                  derivStream.mapMonotone(deriv => EdgeNode(Edge(deriv, begin, newEnd), List(edgeNode, rightEdgeAST)))
              }
              nonEmptyNewEdgeASTs <- newEdgeASTs.ifNonEmpty
            } yield agenda = agenda.insert(nonEmptyNewEdgeASTs)

            for {
              unaryCombinator <- combinators.unary.get(symbol).toSeq
              newEdgeASTs = unaryCombinator(curDeriv).mapMonotone(deriv => EdgeNode(Edge(deriv, begin, end), List(edgeNode)))
              nonEmptyNewEdgeASTs <- newEdgeASTs.ifNonEmpty
            } yield agenda = agenda.insert(nonEmptyNewEdgeASTs)

            edgeNode
        }
      }

      def evalNode[A](begin: Int, end: Int): Option[(Derivation, EdgeAST)] = {
        var next: Option[EdgeNode] = None
        var done = false
        do {
          next = step
          done = next.map(n => n.edge.begin == begin && n.edge.end == end).getOrElse(true)
        } while(!done)

        next.map(n => (n.edge.derivation, n))
      }

      def evalNode[A](symbol: ParseSymbol[A], begin: Int, end: Int): Option[(Derivation { type Result = A }, EdgeAST)] = {
        var next: Option[EdgeNode] = None
        var done = false
        do {
          next = step
          done = next.map(n => n.edge.derivation.symbol == symbol && n.edge.begin == begin && n.edge.end == end).getOrElse(true)
        } while(!done)

        next.map(n => (n.edge.derivation.asInstanceOf[Derivation { type Result = A }], n))
      }

      def evalRoot[A](symbol: ParseSymbol[A]): Option[(Derivation { type Result = A }, EdgeAST)] =
        evalNode(symbol, 0, tokens.size)
    }

    def parseProcess(tokens: Vector[String]): ParseProcessWithTrees =
      new ParseProcessWithTrees(tokens)

    def parse[A](
      tokens: Vector[String],
      rootSymbol: ParseSymbol[A]
    ): OrderedStream[(Derivation { type Result = A }, EdgeAST)] = {
      val process = parseProcess(tokens)
      OrderedStream.exhaustively(process.evalRoot(rootSymbol))
    }
  }

  // sad futile endeavor
  // object CFGParsableAdaptation {
  //   import molt.syntax.cfg.parsable._
  //   import molt.syntax.cfg._
  //   import shapeless.syntax.typeable._

  //   import scala.language.implicitConversions
  //   import scalaz._
  //   import scalaz.std.list._
  //   import scalaz.std.option._
  //   import scalaz.syntax.traverse._

  //   implicit def convSyncProd[
  //     ChildSymbols <: HList,
  //     Children <: HList : Typeable,
  //     Result](
  //     sp: SyncProduction[ChildSymbols, Children, Result])(
  //     implicit ev1: ToList[ChildSymbols, CFGParsable[_]]): (List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[Result])) = {
  //     (sp.production._1.toList,
  //      ((c: List[AST[CFGParsable[_]]]) => for {
  //         childrenList <- sp.production._1.toList.zip(c).map {
  //           case (parsable, ast) => parsable.fromAST(ast)
  //         }.sequence
  //         children <- childrenList.cast[Children]
  //         result <- sp.construct.lift(children)
  //       } yield result))
  //   }

  //   example intended use (DOESN't WORK BECAUSE DUMBNESS):
  //   import molt.syntax.agenda._
  //   import molt.syntax.agenda.SyncProductionSyntax._
  //   import shapeless._
  //   val syncProduction = convSyncProd(
  //     (NonterminalSymbol, Terminal("->"), Plus(NonterminalSymbol)) to CFGProductionParser using {
  //       case (head: String) :: "->" :: (children: List[String]) :: HNil => CFGProduction(head, children.map(ASTNormalTag(_)))
  //     })
  // }

  // some silliness hehe. categorification for semiring parsing
  // additionally requires the times to respect the product. maybe this is necessary from first principles?
  // ...this seems way sketchy actually. hmmm. challenge is to generalize semiring parsing to streams.
  // The main interesting thing that I'm trying to generalize is that it's NOT that each production is assigned an element of the semiring.
  // rather, it is assigned an ENDOMORPHISM on the semiring (which, as it happens, you can get by multiplying by an element.)
  // but now we see that the categorical generalization would simply allow you morphisms to different types. But which ones?
  // well, we can product anything in the CKY algorithm, and the shape of what things we have determines what morphisms (production rules) we can apply.
  // but we actually ONLY use + when we have the SAME symbol / same type we're working with.
  // So we only require that each OBJECT in the category has a COMMUTATIVE SEMIGROUP associated with it.
  // import scalaz.\/
  // trait MonoidalCategoryOfCommutativeSemigroups[F[_]] {
  //   // commutative and associative
  //   def plus[A](fa1: F[A], fa2: F[A]): F[A]
  //   // distributes over plus
  //   def times[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  // }
  // a production rule corresponds to a function F[A] -> F[B] (for unary productions) or F[(A, B)] -> F[C] (for binary productions).
  // This means If F is a functor, you can define your production rule functions as just A -> B or (A, B) -> C.
  // and if you really want to go crazy you can put monadic production stuff on TOP of this... woof.
}

