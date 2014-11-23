package molt.syntax

trait Grammar[A] {
  def parseTokens(toks: Seq[String]): Set[A]
}