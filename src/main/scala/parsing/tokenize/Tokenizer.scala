package parsing.tokenize

trait Tokenizer {
  @Deprecated
  def tokenize(s: String): List[String]

  // in progress; not implemented at all anywhere
  def tokenizations(s: String): Set[Vector[String]] = ???
}