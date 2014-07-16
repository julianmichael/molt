package parsing.tokenize

trait Tokenizer {
  def tokenizations(s: String): Set[Seq[String]]
}