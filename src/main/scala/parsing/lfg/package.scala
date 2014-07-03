package parsing
package object lfg {
  type Feature = String
  type Value = String
  type Specification = Set[Equation[RelativeIdentifier]]
  type FDescription = Set[Equation[AbsoluteIdentifier]]
}

