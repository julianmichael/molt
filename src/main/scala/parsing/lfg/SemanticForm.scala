package parsing.lfg

case class SemanticForm(
  head: String,
  semanticArguments: List[Feature],
  nonSemanticArguments: List[Feature] = List[Feature]()) {
  val allArguments = semanticArguments ++ nonSemanticArguments
}
