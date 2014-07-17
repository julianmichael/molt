package parsing

import scala.collection.mutable
import parsing.tokenize._
import parsing.cfg._

trait Parsable[A] {
  type Intermediate // intermediate representation, i.e., an AST or F-Structure
  def tokenizer: Tokenizer
  def grammar: Grammar[Intermediate]
  def fromAST(ast: Intermediate): Option[A]
}