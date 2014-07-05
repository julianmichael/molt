package parsing
package object lfg {
  type Feature = String
  type Value = String
  type Specification = Set[Equation[RelativeIdentifier]]
  type FDescription = Set[Equation[AbsoluteIdentifier]]
  type LexicalEntry = (String, Specification)
  // TODO make this more complicated!
  type SemanticForm = String
}

