package molt.syntax

/*
 * LexicalCategory objects are for when
 * we don't want to add any productions to the grammar but we still
 * need to parse stuff from ASTs/strings. This is important for not asploding
 * the grammar size just because we have a large vocabulary.
 */
trait LexicalCategory[S] {
  val symbol: S
  def member(str: String): Boolean
}
