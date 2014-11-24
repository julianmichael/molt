package molt.syntax.lfg

case class FStructure(
    map: Map[AbsoluteIdentifier, FStructurePart],
    root: AbsoluteIdentifier) {

  // TODO: DO THIS BETTER. THIS WILL INFINITE LOOP FOR RECURSIVE STRUCTURES.
  override def equals(that: Any): Boolean = {
    import molt.syntax.lfg.parsable.LFGParsables.FStructureParser.makeString
    that.isInstanceOf[FStructure] && makeString(this) == makeString(that.asInstanceOf[FStructure])
  }
  override def hashCode = {
    import molt.syntax.lfg.parsable.LFGParsables.FStructureParser.makeString
    makeString(this).hashCode
  }

  // Completeness:
  // For all mappings in the F-structure,
  // for all FStructureParts mapped to,
  // if the mapped-to part is a semantic form,
  // all of its arguments must be features in the mapping.
  // for each argument that has a semantic role in the semantic form, the
  // FStructure part mapped to by the function must have a semantic feature.
  def isComplete: Boolean = {
    val mapsAndSems = for {
      (_, FMapping(m)) <- map
      (f, id) <- m
      FSemanticForm(_, s) <- map.get(id)
    } yield (m, s)

    mapsAndSems.forall {
      case(m, s) => {
        s.semanticArguments.forall(f => {
          val opt = for {
            argID <- m.get(f).toList
            FMapping(innerMap) <- map.get(argID).toList
            (_, possibleSemID) <- innerMap
            FSemanticForm(_, _) <- map.get(possibleSemID)
          } yield argID
          !opt.isEmpty
        }) &&
        s.nonSemanticArguments.forall(f => {
          // this structure may look pointless but I'm trying to mirror above
          val opt = for {
            argID <- m.get(f)
          } yield argID
          !opt.isEmpty
        })
      }
    }
  }
  // Coherence:
  def isCoherent(argumentFunction: (Feature => Boolean)): Boolean = {
    val governedIDs = (for {
      (_, FMapping(m)) <- map
      (f, id) <- m
      if argumentFunction(f)
    } yield id).toSet

    val semanticIDs = (for {
      (_, FMapping(m)) <- map
      (f, id) <- m
      if argumentFunction(f)
      FSemanticForm(_, _) <- map.get(id)
    } yield id).toSet

    (governedIDs).forall(id => {
      id == root || {
        val governors = for {
          (_, FMapping(m)) <- map
          (_, semID) <- m
          FSemanticForm(_, s) <- map.get(semID).toList
          // every argument function must be designated by a semantic form;
          // any function that has a semantic feature must match up with a designator
          // associated with a semantic role by its semantic feature
          f <- if(semanticIDs(id)) s.semanticArguments else s.allArguments
          mappedTo <- m.get(f)
          if mappedTo == id
        } yield semID
        !governors.isEmpty
      }
    })
  }
  // TODO: Extended coherence:
  // The extended coherence condition applies not just to argument functions,
  // but to all syntactic functions, requiring that they be integrated
  // appropriately into the f-structure (Zaenen, 1985; Fassi-Fehri, 1984;
  // Bresnan and Mchombo, 1987). Argument functions are integrated into the
  // f-structure when they are designated by a PRED as above (i.e., if they
  // satisfy normal coherence). Nonargument functions are integrated if they
  // bear an appropriate relation to a PRED. An ADJUNCT is integrated if the
  // f-structure that contains the ADJUNCT has a PRED feature. A TOPIC or FOCUS
  // function is integrated whenever it is identified with, or anaphorically
  // linked to, an integrated function.
}

sealed abstract class FStructurePart
case object Empty extends FStructurePart
case class FMapping(map: Map[Feature, AbsoluteIdentifier]) extends FStructurePart
case class FSet(set: Set[AbsoluteIdentifier]) extends FStructurePart
case class FValue(v: Value) extends FStructurePart
case class FSemanticForm(id: AbsoluteIdentifier, s: SemanticForm) extends FStructurePart
