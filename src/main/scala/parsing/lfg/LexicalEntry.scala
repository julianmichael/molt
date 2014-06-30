package parsing.lfg

case class LexicalEntry(
  word: String,
  equations: List[Equation[RelativeIdentifier]])