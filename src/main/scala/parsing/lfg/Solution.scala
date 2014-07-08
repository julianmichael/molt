package parsing.lfg

/*
 * Here is how I think I must do the solution:
 * 
 * I introduce a new type, which is an F-Structure with no nested structure.
 * Instead, where any other F-Structure would appear, it only has a "name."
 *
 * I maintain:
 *  - A map from "names" (unique identifiers) to instance of my new F-Structure
 *  - A set of equivalence classes of names, telling me which are identified
 *    with each other.
 *
 * The processing of a defining equation proceeds as follows:
 *   First, I "instantiate" each expression, by giving every single substructure
 * a NEW UNIQUE NAME, and mapping that name in my map to something that
 * represents its minimal structure (with respect to other names).
 *   Then, the "equation" is enforced by placing the names representing the
 * values of both expressions into the same equivalence class.
 *   Finally---and this is the part I'm not totally sure of---the changed
 * equivalence classes should be "unifed." All of the F-Structures referred to
 * in them should be made the same. If the F-Structures contain names, the
 * corresponding names should be unified into the same equivalence class and
 * unification should be performed on that class as well. If any of this causes
 * a violation of Uniqueness, we can stop altogether and return a failure.
 *
 * The equivalence class data structure COULD be implemented using union-find.
 * It'd probably be best that way. TODO: implement persistent union-find data
 * structure from (Conchon and Filliatre, 2007).
 */

// for union-find data structure
import util._
import scalaz._
import Scalaz._

object Solution {
  type PartialSolution = (SetUnionFind[AbsoluteIdentifier], FStructure)

  // transformer to be used with liftM
  type SolutionStateT[M[+_], +A] = StateT[M, PartialSolution, A]
  // solution state monad to represent the branching computation
  type SolutionState[A] = SolutionStateT[List, A]
  // convenience because we're always working in the same monad here
  val get: SolutionState[PartialSolution] = State.get[PartialSolution].lift[List]
  val getGroups: SolutionState[SetUnionFind[AbsoluteIdentifier]] = get flatMap (_._1)
  val getFStructure: SolutionState[FStructure] = get flatMap (_._2)
  val getNames: SolutionState[Set[AbsoluteIdentifier]] = for {
    FStructure(map, _) <- getFStructure
  } yield map.keys.toSet
  def put(x: PartialSolution): SolutionState[Unit] = State.put(x).lift[List]
  def putGroups(x: SetUnionFind[AbsoluteIdentifier]]): SolutionState[Unit] = for {
    psol <- get
    _ <- put(psol.copy(_1 = x))
  } yield ()
  def putFStructure(x: FStructure): SolutionState[Unit] = for {
    psol <- get
    _ <- put(psol.copy(_2 = x))
  } yield ()
  val failure: SolutionState[Nothing] = List[Nothing]().liftM[SolutionStateT]

  val freshID: SolutionState[AbsoluteIdentifier] = for {
    names <- getNames
  } yield AbsoluteIdentifier.freshID(names)

  def addMapping(id: AbsoluteIdentifier, fstruct: FStructurePart): SolutionState[Unit] = for {
    psol <- get
    newMap = psol.fStructure.map + (id -> fstruct)
    _ <- put(psol.copy(fStructure = psol.fStructure.copy(map = newMap)))
  } yield ()

  def getRepresentativeID(id: AbsoluteIdentifier): SolutionState[AbsoluteIdentifier] = for {
    psol <- get
  } yield psol.nameGroups.find(id).get

  def getFStruct(id: AbsoluteIdentifier): SolutionState[FStructurePart] = for {
    psol <- get
    rep <- getRepresentativeID(id)
  } yield psol.nameMap(rep)

  def equateIDs(
      id1: AbsoluteIdentifier,
      id2: AbsoluteIdentifier): SolutionState[AbsoluteIdentifier] = for {
    psol <- get
    uf = psol.nameGroups
    newUF = uf.union(id1, id2).get
    _ <- put(psol.copy(nameGroups = newUF))
    rep <- getRepresentativeID(id1)
  } yield rep

  // end "pure convenience" methods, begin functionality

  def unifyIDs(
      id1: AbsoluteIdentifier,
      id2: AbsoluteIdentifier): SolutionState[AbsoluteIdentifier] =
    // "if" to mitigate unnecessary corecursion
    if(id1 == id2) state(id1).lift[List]
    else for {
      fstruct1 <- getFStruct(id1)
      fstruct2 <- getFStruct(id2)
      newFStruct <- unifyFStructs(fstruct1, fstruct2)
      newID <- equateIDs(id1, id2)
      _ <- addMapping(newID, newFStruct)
    } yield newID

  def unifyFStructs(one: FStructurePart, two: FStructurePart): SolutionState[FStructurePart] = {
    // "if" to mitigate unnecessary corecursion
    if(one == two) state(one).lift[List]
    else (one, two) match {
      case (Empty, x) => state(x).lift[List]
      case (x, Empty) => state(x).lift[List]
      case (SolutionFMapping(m1), SolutionFMapping(m2)) => {
        val fmapping = m1 ++ m2
        val keys = fmapping.keys
        val unities = keys.map(k => unifyIDs(m1(k), m2(k)))
        for {
          _ <- unities.reduce((x, y) => (for {_ <- x; b <- y} yield b))
        } yield SolutionFMapping(fmapping)
      }
      case (SolutionFSet(s1), SolutionFSet(s2)) =>
        state(SolutionFSet(s1 ++ s2)).lift[List]
      case (SolutionFValue(v1), SolutionFValue(v2)) if(v1 == v2) =>
        state(SolutionFValue(v1)).lift[List]
      case (SolutionFSemanticForm(s1), SolutionFSemanticForm(s2)) if(s1 == s2) =>
        state(SolutionFSemanticForm(s1)).lift[List]
      // THIS CASE is where violations of Uniqueness cause failure!
      case _ => failure
    }
  }

  def makeExpression(exp: Expression[AbsoluteIdentifier]): SolutionState[AbsoluteIdentifier] =
    exp match {
      case IdentifierExpression(id) => state(id).lift[List]
      case Application(e, feat) => for {
        id <- freshID
        subExpressionID <- makeExpression(e)
        // the subexpression maps to the total expression via the feature
        _ <- addMapping(subExpressionID, SolutionFMapping(Map(feat -> id)))
      } yield id
      case ValueExpression(v) => for {
        id <- freshID
        _ <- addMapping(id, SolutionFValue(v))
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
      _ <- addMapping(contID, SolutionFSet(Set(elemID)))
    } yield ()
  }

  def testExpression(
      exp: Expression[AbsoluteIdentifier]): SolutionState[AbsoluteIdentifier] = exp match {
    case IdentifierExpression(id) => for {
      rep <- getRepresentativeID(id)
    } yield rep
    case Application(e, feat) => for {
      subID <- testExpression(e)
      // TODO figure out why filter doesn't work here
      fstruct <- getFStruct(subID)
      mapState: SolutionState[Map[Feature, AbsoluteIdentifier]] = fstruct match {
        case SolutionFMapping(m) => state(m).lift[List]
        case _ => failure
      }
      map <- mapState
      expID <- getRepresentativeID(map(feat)) 
    } yield expID
    case ValueExpression(v) => for {
      // TODO get all rep. IDs that map to this value
      psol <- get
      map = psol.nameMap
      ids = map collect { case (k, SolutionFValue(`v`)) => k }
      id <- ids.toList.liftM[SolutionStateT]
      rep <- getRepresentativeID(id)
    } yield rep
  }

  def verifyConstraint(eq: ConstraintEquation[AbsoluteIdentifier]): SolutionState[Boolean] = eq match {
    case Equals(pos, l, r) => for {
      leftID <- testExpression(l)
      rightID <- testExpression(r)
      leftRep <- getRepresentativeID(leftID)
      rightRep <- getRepresentativeID(rightID)
    } yield (pos == (leftRep == rightRep))
    case Contains(pos, e, c) => for {
      elemID <- testExpression(e)
      contID <- testExpression(c)
      elemRep <- getRepresentativeID(elemID)
      contStruct <- getFStruct(contID)
      set <- contStruct match {
        case SolutionFSet(s) => s.toList.map(getRepresentativeID(_)).sequence.map(_.toSet)
        case _ => failure
      }
    } yield (pos == (set(elemRep)))
    case Exists(pos, e) => ???
    // TODO this case is evidence that I need to revise my approach for this
    // function :(
  }

  def makeFStructure(rootID: AbsoluteIdentifier): SolutionState[FStructure] = for {
    solFStruct <- getFStruct(rootID)
    fStruct <- solFStruct match {
      case Empty => failure
      case SolutionFMapping(map) => {
        ???
      }
      case SolutionFSet(set) => ???
      case SolutionFValue(v) => ???
      case SolutionFSemanticForm(s) => ???
    }
  } yield fStruct

  object PartialSolution {
    // to make sure we don't use names that are already taken in the f-description
    def empty(fdesc: FDescription): PartialSolution = {
      val names = fdesc.flatMap(_.identifiers)
      val unionFind = names.foldLeft(SetUnionFind.empty[AbsoluteIdentifier])(_ add _)
      val nameMap = names.map({ case id => (id -> (Empty: FStructurePart)) }).toMap
      PartialSolution(names, unionFind, nameMap)
    }
  }

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
            satisfied <- verifyConstraint(head)
            sol <- if(satisfied) solvePartial(fdesc - Constraint(head)) else failure
          } yield sol
          // all equations processed: we're done! return the final F-structure(s).
          case None => makeFStructure(???) // TODO should be the root ID
        }
      }
    }
  }

  def solve(fdesc: FDescription): List[FStructure] = {
    solvePartial(fdesc).eval(PartialSolution.empty(fdesc))
  }
}
