package parsing.lfg

case class FStructure(
    map: Map[AbsoluteIdentifier, FStructurePart],
    root: AbsoluteIdentifier) {

  // Completeness:
  // For all mappings in the F-structure,
  // for all FStructureParts mapped to,
  // if the mapped-to part is a semantic form,
  // all of its arguments must be features in the mapping.
  // for each argument that has a semantic role in the semantic form, the
  // FStructure part mapped to by the function must have a semantic feature.
  def isComplete: Boolean = (map collect {
    case (_, FMapping(m)) => m.forall {
      case (_, id) => map(id) match {
        case FSemanticForm(s) => {
          s.allArguments.forall(m.keySet) && s.semanticArguments.forall(arg => map(m(arg)) match {
            case FMapping(innerMap) => innerMap.exists {
              case (_, id2) => map(id2) match {
                case FSemanticForm(_) => true
                case _ => false
              }
              case _ => false
            }
            case _ => false
          })
        }
        case _ => true
      }
    }
  }).forall(identity)
  // Coherence:
  // 
  // 
  // 
  // 
  // which is a semantic form with the feature in its argument list.
  // Furthermore, if the argument/governable function feature has a semantic
  // form, then it must have a semantic role in a designator.
  def isCoherent(argumentFunction: (Feature => Boolean)): Boolean = {
    val mapsWithGovernedFStructs = for {
      (_, FMapping(m)) <- map             // Consider mappings in the F structure
      (f, id) <- m                        // with features f in the mapping
      if argumentFunction(f)              // that are argument functions, and
      semantic = map(id) match {          // whether their fstructs have semantic forms.
        case FMapping(inner) => inner.values.map(map(_)).exists {
          case FSemanticForm(_) => true
          case _ => false
        }
        case _ => false
      }
    } yield (m, f, semantic)
    mapsWithGovernedFStructs.forall {     // For all such (mapping, feature, sem) pairs
      case (m, f, sem) => m.exists {      // there exists an FStructurePart in the mapping
        case (_, id) => map(id) match {   // which is
          case FSemanticForm(s) =>        // a semantic form that,
            if(sem) {                     // if the fstruct has semantic content,
              s.semanticArguments.contains(f) // has f as a semantic argument.
            } else {
              s.allArguments.contains(f)  // Otherwise, f must simply be an argument.
            }
          case _ => false
        }
      }
    }
  }
}

sealed abstract class FStructurePart
case object Empty extends FStructurePart
case class FMapping(map: Map[Feature, AbsoluteIdentifier]) extends FStructurePart
case class FSet(set: Set[AbsoluteIdentifier]) extends FStructurePart
case class FValue(v: Value) extends FStructurePart
case class FSemanticForm(s: SemanticForm) extends FStructurePart
