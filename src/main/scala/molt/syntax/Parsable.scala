package molt.syntax

import molt.tokenize._
import molt.syntax.cfg._

// TODO: create a more highly abstracted parsable outside of this package that doesn't cause
// an inter-package dependency. This would be more appropriate once we have some semantic theories
// implemented.
trait Parsable[+A] {
  type Intermediate // intermediate representation, i.e., an AST or F-Structure
  def tokenizer: Tokenizer
  def grammar: Grammar[Intermediate]
  def fromAST(ast: Intermediate): Option[A]
}