package molt.syntax.lfg

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

// for union-find data structure
import molt.util._
// for monadic goodness
import scalaz._
import Scalaz._

class LFGSolver(
    wildcards: Map[Feature, List[Feature]] = Map.empty[Feature, List[Feature]]) {
  type PartialSolution = (SetUnionFind[AbsoluteIdentifier], FStructure)
  def emptySolution(fdesc: FDescription, rootID: AbsoluteIdentifier) = {
    val ids = fdesc.flatMap(_.identifiers) + rootID
    val uf = ids.foldLeft(SetUnionFind.empty[AbsoluteIdentifier])(_ add _)
    val map = ids.map(i => (i -> Empty)).toMap
    (uf, FStructure(map, rootID))
  }

  // transformer to be used with liftM
  type SolutionStateT[M[+_], A] = StateT[M, PartialSolution, A]
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
  def failure[A]: SolutionState[A] = List[A]().liftM[SolutionStateT]

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
        for {
          _ <- unities.foldLeft(freshID)((x, y) => (for {_ <- x; b <- y} yield b))
        } yield FMapping(m1 ++ m2)
      }
      case (FSet(s1), FSet(s2)) =>
        state(FSet(s1 ++ s2): FStructurePart).lift[List]
      case (FValue(v1), FValue(v2)) if(v1 == v2) =>
        state(FValue(v1): FStructurePart).lift[List]
      case (s1@FSemanticForm(_, _), s2@FSemanticForm(_, _)) if(s1 == s2) =>
        state(s1: FStructurePart).lift[List]
      // THIS CASE is where violations of Uniqueness cause failure!
      case _ => failure[FStructurePart]
    }
  }

  def makeExpression(exp: Expression[AbsoluteIdentifier]): SolutionState[AbsoluteIdentifier] =
    exp match {
      case FunctionalExpression(ex) => ex match {
        case BareIdentifier(id) => state(id).lift[List]
        case Application(e, f) => for {
          feat <- wildcards.getOrElse(f, List(f)).liftM[SolutionStateT]
          subExpressionID <- makeExpression(FunctionalExpression(e))
          id <- freshID
          _ <- addMapping(subExpressionID, FMapping(Map(feat -> id)))
        } yield id
        case InverseApplication(f, e) => for {
          feat <- wildcards.getOrElse(f, List(f)).liftM[SolutionStateT]
          subExpressionID <- makeExpression(FunctionalExpression(e))
          id <- freshID
          _ <- addMapping(id, FMapping(Map(feat -> subExpressionID)))
          groups <- getGroups
          // branch here: go through all groups.representatives, and make the
          // choice of unifying `id` with each one---or none of them.
          unitee <- (groups.representatives).toList.liftM[SolutionStateT]
          rep <- unifyIDs(id, unitee)
        } yield rep
      }
      case ValueExpression(v) => for {
        id <- freshID
        _ <- addMapping(id, FValue(v))
      } yield id
      case SemanticFormExpression(s) => for {
        id <- freshID
        semID <- freshID
        _ <- addMapping(id, FSemanticForm(semID, s))
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
        expID <- map.get(feat)
        expRepID = groups.find(expID).get
      } yield expRepID
      case InverseApplication(feat, e) => for {
        // should be the representative ID
        subID <- testExpression(FunctionalExpression(e), fstruct, groups)
        // next we need to find every ID that corresponds to a map that maps by
        // `feat` to an id whose rep. ID is subID.
        inverseApplyID <- groups.representatives
        FMapping(m) <- fstruct.map.get(inverseApplyID)
        mappedTo <- m.get(feat)
        if groups.find(mappedTo).get == subID
      } yield inverseApplyID
    }
    case ValueExpression(v) => for {
      id <- (fstruct.map collect { case (k, FValue(`v`)) => k }).toList
      rep = groups.find(id).get
    } yield rep
    // TODO consider making this case automatically false!
    case SemanticFormExpression(s) => for {
      id <- (fstruct.map collect { case (k, FSemanticForm(_, `s`)) => k }).toList
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

  /*
   * TODO: define method on all equations that returns approx. branching factor?
   * Preferred order of resolving equations to minimize branching:
   * 1. Defining/Assignment equations w/o inside-out application
   * 2. Defining/Containment equations w/o inside-out application
   * 3. Conjunctions
   * 4. Disjunctions: some branching
   * 5. Defining equations with inside-out application: CRAZY branching
   * And finally, constraint equations.
   */
  def solvePartial(fdesc: FDescription): SolutionState[FStructure] = {
    val definingEqs = fdesc collect { case Defining(eq) => eq }
    val (insideOut, nonInsideOut) = definingEqs.partition(_.hasInsideOutApplication)
    val conjunctions = fdesc collect { case Compound(eq@Conjunction(_)) => eq }
    val disjunctions = fdesc collect { case Compound(eq@Disjunction(_)) => eq }
    val constraintEqs = fdesc collect { case Constraint(eq) => eq }

    nonInsideOut.headOption match {
      case Some(head) => for {
        _ <- addDefine(head)
        sol <- solvePartial(fdesc - Defining(head))
      } yield sol
      case None => conjunctions.headOption match {
        case Some(Conjunction(eqs)) =>
          solvePartial(fdesc - Compound(Conjunction(eqs)) ++ eqs)
        case None => disjunctions.headOption match {
          case Some(Disjunction(eqs)) => for {
            // this is where the branching happens!
            disjunct <- eqs.toList.liftM[SolutionStateT]
            sol <- solvePartial(fdesc - Compound(Disjunction(eqs)) + disjunct)
          } yield sol
          case None => insideOut.headOption match {
            case Some(head) => for {
              _ <- addDefine(head)
              sol <- solvePartial(fdesc - Defining(head))
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
    }
  }

  def solve(fdesc: FDescription, rootID: AbsoluteIdentifier): Set[FStructure] = {
    solvePartial(fdesc).eval(emptySolution(fdesc, rootID)).toSet
  }

  def apply(fdesc: FDescription, rootID: AbsoluteIdentifier): Set[FStructure] =
    solve(fdesc, rootID)
}
