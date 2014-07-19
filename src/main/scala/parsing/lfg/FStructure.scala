package parsing.lfg

case class FStructure(
    map: Map[AbsoluteIdentifier, FStructurePart],
    root: AbsoluteIdentifier) {
  // Completeness:
  // For all mappings in the F-structure,
  // for all FStructureParts mapped to,
  // if the mapped-to part is a semantic form,
  // all of its arguments must be features in the mapping.
  def isComplete: Boolean = (map collect {
    case (_, FMapping(m)) => m.forall {
      case (_, id) => map(id) match {
        case FSemanticForm(s) => s.semanticArguments.forall(m.keySet)
        case _ => true
      }
    }
  }).forall(identity)
  // Coherence:
  // For all mappings in the F structure,
  // for all features in the mapping that are argument functions,
  // there exists an FStructurePart
  // which is a semantic form with the feature in its argument list.
  def isCoherent(argumentFunction: (Feature => Boolean)): Boolean = (map collect {
    case (_, FMapping(m)) => m.forall {
      case (feat, _) if argumentFunction(feat) => m.exists {
        case (_, id) => map(id) match {
          case FSemanticForm(s) => s.semanticArguments.contains(feat)
          case _ => false
        }
      }
      case _ => true
    }
  }).forall(identity)
}

sealed abstract class FStructurePart
case object Empty extends FStructurePart
case class FMapping(map: Map[Feature, AbsoluteIdentifier]) extends FStructurePart
case class FSet(set: Set[AbsoluteIdentifier]) extends FStructurePart
case class FValue(v: Value) extends FStructurePart
case class FSemanticForm(s: SemanticForm) extends FStructurePart
