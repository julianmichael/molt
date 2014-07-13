package parsing.lfg

import parsing.LexicalCategory

trait LFGLexicalCategory[S] extends LexicalCategory[S] {
  val symbol: S
  def specifications(s: String): Set[Specification]
  def member(s: String): Boolean
}

case class BasicLFGLexicalCategory[S](
  val symbol: S,
  lexicalEntries: Set[LexicalEntry]
  ) extends LFGLexicalCategory[S]{

  override def specifications(token: String): Set[Specification] = for {
    (word, spec) <- lexicalEntries
    if word == token
  } yield spec

  override def member(token: String): Boolean =
    lexicalEntries.map(_._1).contains(token)

}