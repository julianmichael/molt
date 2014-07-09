package parsing.lfg

case class FStructure(
  map: Map[AbsoluteIdentifier, FStructurePart],
  root: AbsoluteIdentifier) {

  def pretty: String = {
    def prettyForID(id: AbsoluteIdentifier): String = map(id) match {
      case Empty => "[]"
      case FMapping(m) => {
        val inner = m.map{ case (feat, subid) => s"$feat ==> ${prettyForID(subid)}"}.mkString("\n")
        s"[\n$inner\n]"
      }
      case FSet(s) => {
        val inner = s.map(prettyForID(_)).mkString(",")
        s"{\n$inner\n}"
      }
      case FValue(v) => v
      case FSemanticForm(s) => s
    }
    prettyForID(root)
  }
}

sealed abstract class FStructurePart
case object Empty extends FStructurePart
case class FMapping(map: Map[Feature, AbsoluteIdentifier]) extends FStructurePart
case class FSet(set: Set[AbsoluteIdentifier]) extends FStructurePart
case class FValue(v: Value) extends FStructurePart
case class FSemanticForm(s: SemanticForm) extends FStructurePart
