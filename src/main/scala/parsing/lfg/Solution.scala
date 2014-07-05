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
 * That is, if there exists a functional union-find data structure...
 */

object Solution {
  case class SolutionPart(
    names: Set[AbsoluteIdentifier],
    struct: Option[SolutionFStructure])
  sealed abstract class SolutionFStructure
  case class SolutionFMapping(map: Map[Feature, SolutionPart]) extends SolutionFStructure
  case class SolutionFSet(set: Set[SolutionPart]) extends SolutionFStructure
  case class SolutionFValue(v: Value) extends SolutionFStructure
  case class SolutionFSemanticForm(s: SemanticForm) extends SolutionFStructure

  type PartialSolution = Set[SolutionPart]
  private[this] val emptySolution = Set[SolutionPart]()

  def addDefine(eq: DefiningEquation[AbsoluteIdentifier])
    (psol: PartialSolution): Set[PartialSolution] = {
    ???
  }

  def verifyConstraint(eq: ConstraintEquation[AbsoluteIdentifier])
    (psol: PartialSolution): Set[PartialSolution] = {
    ???
  }

  def makeFStructure(psol: PartialSolution): FStructure = {
    ???
  }

  def solvePartial(fdesc: FDescription)(psol: PartialSolution): Set[FStructure] = {
    val definingEqs = fdesc collect { case Defining(eq) => eq }
    val compoundEqs = fdesc collect { case Compound(eq) => eq }
    val constraintEqs = fdesc collect { case Constraint(eq) => eq }

    definingEqs.headOption match {
      // first, find the minimal f-structure satisfying the non-compound defining equations
      case Some(head) => {
        addDefine(head)(psol) flatMap solvePartial(fdesc - Defining(head))
      }
      // no more defining equations left, so process compound equations. We
      // delayed the branching until now for efficiency. Recurse because some of
      // the inner equations may be defining equations.
      case None => compoundEqs.headOption match {
        case Some(head@Conjunction(l, r)) =>
          solvePartial(fdesc - Compound(head) + l + r)(psol)
        case Some(head@Disjunction(l, r)) => 
          solvePartial(fdesc - Compound(head) + l)(psol) ++
          solvePartial(fdesc - Compound(head) + r)(psol)
        // no more compound equations left, so verify that the constraints hold
        case None => constraintEqs.headOption match {
          case Some(head) => {
            verifyConstraint(head)(psol) flatMap solvePartial(fdesc - Constraint(head))
          }
          // all equations processed: we're done!
          case None => Set(makeFStructure(psol))
        }
      }
    }
  }

  def solve(fdesc: FDescription): Set[FStructure] = {
    solvePartial(fdesc)(emptySolution)
  }
}
