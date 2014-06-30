package parsing.lfg

sealed abstract class FStructure
case class FMapping(map: Map[Feature, FStructure]) extends FStructure
case class FValue(v: Value) extends FStructure
