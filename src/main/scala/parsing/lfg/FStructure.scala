package parsing.lfg

class FStructure(
  map: Map[AbsoluteIdentifier, FStructurePart],
  root: AbsoluteIdentifier)

sealed abstract class FStructurePart
case object Empty extends FStructurePart
case class FMapping(map: Map[Feature, AbsoluteIdentifier]) extends FStructurePart
case class FSet(set: Set[AbsoluteIdentifier]) extends FStructurePart
case class FValue(v: Value) extends FStructurePart
case class FSemanticForm(s: SemanticForm) extends FStructurePart
