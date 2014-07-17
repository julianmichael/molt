package parsing.tokenize

trait Tokenizer {
  def tokenizations(s: String): Set[Seq[String]]
}

object IdentityTokenizer {
  override def tokenizations(s: String) = Set(Seq(s))
}