package parsing

trait Grammar[A] {
  def parseTokens(toks: Seq[String]): Set[A]
}