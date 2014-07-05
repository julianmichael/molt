package parsing.lfg

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
/*
case class PartialSolution(
  map: Map[AbsoluteIdentifier, FStructure],
  remainingFDescription: FDescription,
  assignments: Set[Assignment[AbsoluteIdentifier]]) {

  sealed abstract class GetResult
  case class Item(fstruct: FStructure) extends GetResult
  case object Empty extends GetResult
  case object Invalid extends GetResult

  val definingEqs = remainingFDescription collect { case Defining(eq) => eq }
  val compoundEqs = remainingFDescription collect { case Compound(eq) => eq }
  val constraintEqs = remainingFDescription collect { case Constraint(eq) => eq }

  def get(exp: Expression[AbsoluteIdentifier]): GetResult = exp match {
    case IdentifierExpression(AbsoluteIdentifier(id)) => Item(map(id))
    case Application(appl, feature) => appl match {
      case FMapping(mapping) => mapping.get(feature) match {
        case Some(fstruct) => Item(fstruct)
        case None => Empty
      }
      case _ => Invalid
    }
    case _ => Invalid
  }

  def updateRefsForAssignment(
    exp1: Expression[AbsoluteIdentifier],
    exp2: Expression[AbsoluteIdentifier]): Option[PartialSolution] = {
      def unifyIDs(id1: AbsoluteIdentifier, id2: AbsoluteIdentifier): Option[PartialSolution = {
        (map(id1).deepest, map(id2).deepest) match {
          case (x@FRef(None), y@FRef(None)) => {
            val newRef = FRef(None)
            x.nested = y.nested = Some(newRef)
            Some(this)
          }
          case (
        }
      }
      (exp1, exp2) match {
      case (IdentifierExpression(id1), IdentifierExpression(id2)) =>

      case (IdentifierExpression(id1), 

      case _ => ???
    }
  }
    
}*/
