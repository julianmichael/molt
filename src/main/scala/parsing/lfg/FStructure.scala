package parsing.lfg

sealed abstract class FStructure {
  def apply(f: Feature): Option[FStructure] = this match {
    case FMapping(map) => map.get(f)
    case _ => None
  }
}
case class FMapping(map: Map[Feature, FStructure]) extends FStructure
case class FSet(set: Set[FStructure]) extends FStructure
case class FValue(v: Value) extends FStructure
case class FSemanticForm(s: SemanticForm) extends FStructure
