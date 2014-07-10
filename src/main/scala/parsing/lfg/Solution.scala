package parsing.lfg

/*
 * Here is how I have done the solution:
 *
 * I maintain:
 *  - A map from "names" (unique identifiers) to FStructureParts (i.e., an
 *    F-Structure)
 *  - A set of equivalence classes of names, telling me which are identified
 *    with each other.
 *
 * The processing of a defining equation proceeds as follows:
 *   First, I "instantiate" each expression, by giving every single substructure
 * a NEW UNIQUE NAME, and mapping that name in my map to something that
 * represents its minimal structure (with respect to other names).
 *   Then, the "equation" is enforced by placing the names representing the
 * values of both expressions into the same equivalence class.
 *   Finally, the changed
 * equivalence classes should be "unifed." All of the F-Structures referred to
 * in them should be made the same. If the F-Structures contain names, the
 * corresponding names should be unified into the same equivalence class and
 * unification should be performed on that class as well. If any of this causes
 * a violation of Uniqueness, we can stop altogether and return a failure.
 *
 * The equivalence class data structure is implemented using union-find.
 * It's probably be best that way. TODO: implement efficient persistent
 * union-find data structure from (Conchon and Filliatre, 2007).
 */

// for union-find data structure and StateT MonadPlus instance
import util._
// for monadic goodness
import scalaz._
import Scalaz._

object Solution {
  type PartialSolution = (SetUnionFind[AbsoluteIdentifier], FStructure)
  def emptySolution(fdesc: FDescription, rootID: AbsoluteIdentifier) = {
    val ids = fdesc.flatMap(_.identifiers) + rootID
    val uf = ids.foldLeft(SetUnionFind.empty[AbsoluteIdentifier])(_ add _)
    val map = ids.map(i => (i -> Empty)).toMap
    (uf, FStructure(map, rootID))
  }

  // transformer to be used with liftM
  type SolutionStateT[M[+_], +A] = StateT[M, PartialSolution, A]
  // solution state monad to represent the branching computation
  type SolutionState[A] = SolutionStateT[List, A]
  // convenience because we're always working in the same monad here
  val get: SolutionState[PartialSolution] = State.get[PartialSolution].lift[List]
  val getGroups: SolutionState[SetUnionFind[AbsoluteIdentifier]] = get map (_._1)
  val getFStructure: SolutionState[FStructure] = get map (_._2)
  def put(x: PartialSolution): SolutionState[Unit] = State.put(x).lift[List]
  def putGroups(x: SetUnionFind[AbsoluteIdentifier]): SolutionState[Unit] = for {
    psol <- get
    _ <- put(psol.copy(_1 = x))
  } yield ()
  def putFStructure(x: FStructure): SolutionState[Unit] = for {
    psol <- get
    _ <- put(psol.copy(_2 = x))
  } yield ()
  val failure: SolutionState[Nothing] = List[Nothing]().liftM[SolutionStateT]

  val freshID: SolutionState[AbsoluteIdentifier] = for {
    fStructure <- getFStructure
    newID = AbsoluteIdentifier.freshID(fStructure.map.keySet)
    groups <- getGroups
    _ <- putGroups(groups.add(newID))
    newMap = fStructure.map + (newID -> Empty)
    _ <- putFStructure(fStructure.copy(map = newMap))
  } yield newID

  def getRepresentativeID(id: AbsoluteIdentifier): SolutionState[AbsoluteIdentifier] =
    getGroups map (_.find(id).get)

  def getFStructurePart(id: AbsoluteIdentifier): SolutionState[FStructurePart] = for {
    FStructure(map, _) <- getFStructure
    rep <- getRepresentativeID(id)
  } yield map(rep)

  def equateIDs(
      id1: AbsoluteIdentifier,
      id2: AbsoluteIdentifier): SolutionState[AbsoluteIdentifier] = for {
    uf <- getGroups
    newUF = uf.union(id1, id2).get
    _ <- putGroups(newUF)
    rep <- getRepresentativeID(id1)
  } yield rep

  // end "pure convenience" methods, begin functionality

  def addMapping(id: AbsoluteIdentifier, fstruct: FStructurePart): SolutionState[Unit] = for {
    fStructure <- getFStructure
    repID <- getRepresentativeID(id)
    oldPart <- getFStructurePart(repID)
    newPart <- unifyFStructureParts(oldPart, fstruct)
    newMap = fStructure.map + (repID -> newPart)
    _ <- putFStructure(fStructure.copy(map = newMap))
  } yield ()

  def unifyIDs(
      id1: AbsoluteIdentifier,
      id2: AbsoluteIdentifier): SolutionState[AbsoluteIdentifier] =
    // "if" to mitigate unnecessary corecursion
    if(id1 == id2) state(id1).lift[List]
    else for {
      fstruct1 <- getFStructurePart(id1)
      fstruct2 <- getFStructurePart(id2)
      newFStruct <- unifyFStructureParts(fstruct1, fstruct2)
      newID <- equateIDs(id1, id2)
      _ <- addMapping(newID, newFStruct)
    } yield newID

  def unifyFStructureParts(one: FStructurePart, two: FStructurePart): SolutionState[FStructurePart] = {
    // "if" to mitigate unnecessary corecursion
    if(one == two) state(one).lift[List]
    else (one, two) match {
      case (Empty, x) => state(x).lift[List]
      case (x, Empty) => state(x).lift[List]
      case (FMapping(m1), FMapping(m2)) => {
        val features = m1.keySet intersect m2.keySet
        val unities = features.map(k => unifyIDs(m1(k), m2(k)))
        println(s"m1: $m1")
        println(s"m2: $m2")
        for {
          _ <- unities.foldLeft(freshID)((x, y) => (for {_ <- x; b <- y} yield b))
        } yield FMapping(m1 ++ m2)
      }
      case (FSet(s1), FSet(s2)) =>
        state(FSet(s1 ++ s2)).lift[List]
      case (FValue(v1), FValue(v2)) if(v1 == v2) =>
        state(FValue(v1)).lift[List]
      case (FSemanticForm(s1), FSemanticForm(s2)) if(s1 == s2) =>
        state(FSemanticForm(s1)).lift[List]
      // THIS CASE is where violations of Uniqueness cause failure!
      case _ => failure
    }
  }

  def makeExpression(exp: Expression[AbsoluteIdentifier]): SolutionState[AbsoluteIdentifier] =
    exp match {
      case FunctionalExpression(ex) => ex match {
        case BareIdentifier(id) => state(id).lift[List]
        case Application(e, feat) => for {
          subExpressionID <- makeExpression(FunctionalExpression(e))
          id <- freshID
          // the subexpression maps to the total expression via the feature
          _ <- addMapping(subExpressionID, FMapping(Map(feat -> id)))
          // perhaps fix this and/or update addMapping code to do FUSIONZ but
          // actually this is ok right now because it'd only possibly get
          // overwritten in the containing expression and that's great
          _ <- addMapping(id, Empty)
        } yield id
      }
      case ValueExpression(v) => for {
        id <- freshID
        _ <- addMapping(id, FValue(v))
      } yield id
      case SemanticFormExpression(s) => for {
        id <- freshID
        _ <- addMapping(id, FSemanticForm(s))
      } yield id
    }

  def addDefine(eq: DefiningEquation[AbsoluteIdentifier]): SolutionState[Unit] = eq match {
    case Assignment(l, r) => for {
      leftID <- makeExpression(l)
      rightID <- makeExpression(r)
      _ <- unifyIDs(leftID, rightID)
    } yield ()
    case Containment(e, c) => for {
      elemID <- makeExpression(e)
      contID <- makeExpression(c)
      _ <- addMapping(contID, FSet(Set(elemID)))
    } yield ()
  }

  def testExpression(
      exp: Expression[AbsoluteIdentifier],
      fstruct: FStructure,
      groups: SetUnionFind[AbsoluteIdentifier]): List[AbsoluteIdentifier] = exp match {
    case FunctionalExpression(ex) => ex match {
      case BareIdentifier(id) => List(groups.find(id).get)
      case Application(e, feat) => for {
        subID <- testExpression(FunctionalExpression(e), fstruct, groups)
        fstructPart = fstruct.map(subID)
        map = fstructPart match {
          case FMapping(m) => m
          case _ => Map.empty[Feature, AbsoluteIdentifier]
        }
        expID = map(feat)
        expRepID = groups.find(expID).get
      } yield expRepID
    }
    case ValueExpression(v) => for {
      id <- (fstruct.map collect { case (k, FValue(`v`)) => k }).toList
      rep = groups.find(id).get
    } yield rep
    case SemanticFormExpression(s) => for {
      id <- (fstruct.map collect { case (k, FSemanticForm(`s`)) => k }).toList
      rep = groups.find(id).get
    } yield rep
  }

  def satisfied(
      eq: ConstraintEquation[AbsoluteIdentifier],
      fstruct: FStructure,
      groups: SetUnionFind[AbsoluteIdentifier]): Boolean = eq match {
    case Equals(pos, l, r) => {
      val tests = for {
        leftID <- testExpression(l, fstruct, groups)
        rightID <- testExpression(r, fstruct, groups)
      } yield (pos == (leftID == rightID))
      !tests.filter(identity).isEmpty
    }
    case Contains(pos, e, c) => {
      val tests = for {
        elemID <- testExpression(e, fstruct, groups)
        contID <- testExpression(c, fstruct, groups)
        contStruct = fstruct.map(contID)
        set = contStruct match {
          case FSet(s) => s.map(groups.find(_).get)
          case _ => Set.empty[AbsoluteIdentifier]
        }
      } yield (pos == (set(elemID)))
      !tests.filter(identity).isEmpty
    }
    case Exists(pos, e) => {
      val tests = testExpression(e, fstruct, groups)
      pos == !tests.isEmpty
    }
  }

  val makeFStructure: SolutionState[FStructure] = for {
    FStructure(map, rootID) <- getFStructure
    uf <- getGroups
    rootRep <- getRepresentativeID(rootID)
  } yield FStructure(map.collect {
    case (k, v) if uf.find(k).get == k => (k, v match {
      case FMapping(m) => FMapping(m.map { case (feat, id) => (feat, uf.find(id).get) })
      case FSet(s) => FSet(s.map(uf.find(_).get))
      case other => other
    })
  }, rootRep)

  def solvePartial(fdesc: FDescription): SolutionState[FStructure] = {
    val definingEqs = fdesc collect { case Defining(eq) => eq }
    val compoundEqs = fdesc collect { case Compound(eq) => eq }
    val constraintEqs = fdesc collect { case Constraint(eq) => eq }

    definingEqs.headOption match {
      // first, find the minimal f-structure satisfying the non-compound defining equations
      case Some(head) => for {
        _ <- addDefine(head)
        sol <- solvePartial(fdesc - Defining(head))
      } yield sol
      // no more defining equations left, so process compound equations. We
      // delayed the branching until now for efficiency. Recurse because some of
      // the inner equations may be defining equations.
      case None => compoundEqs.headOption match {
        case Some(head@Conjunction(l, r)) =>
          solvePartial(fdesc - Compound(head) + l + r)
        case Some(head@Disjunction(l, r)) => for {
          // this is where the branching happens!
          disjunct <- List[Equation[AbsoluteIdentifier]](l, r).liftM[SolutionStateT]
          sol <- solvePartial(fdesc - Compound(head) + disjunct)
        } yield sol
        // no more compound equations left, so verify that the constraints hold
        case None => constraintEqs.headOption match {
          case Some(head) => for {
            fstruct <- getFStructure
            groups <- getGroups
            if satisfied(head, fstruct, groups)
            sol <- solvePartial(fdesc - Constraint(head))
          } yield sol
          case None => makeFStructure
        }
      }
    }
  }

  def solve(fdesc: FDescription, rootID: AbsoluteIdentifier): List[FStructure] = {
    solvePartial(fdesc).eval(emptySolution(fdesc, rootID))
  }
}
