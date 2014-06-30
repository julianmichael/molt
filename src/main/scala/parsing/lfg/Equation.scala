package parsing.lfg

sealed abstract class Equation[ID <: Identifier] {
  def ground(up: AbsoluteIdentifier, down: AbsoluteIdentifier)
    (implicit evidence: ID <:< RelativeIdentifier): Equation[AbsoluteIdentifier] = this match {
    case Assignment(left, right) => Assignment(left.ground(up, down), right.ground(up, down))
    case Existence(exp) => Existence(exp.ground(up, down))
    case Constraint(left, right) => Constraint(left.ground(up, down), right.ground(up, down))
    case Conditional(cond, cons) => Conditional(cond.ground(up, down), cons.ground(up, down))
  }
}
case class Assignment[ID <: Identifier](left: Expression[ID], right: Expression[ID])
  extends Equation[ID]
case class Existence[ID <: Identifier](exp: Expression[ID])
  extends Equation[ID]
case class Constraint[ID <: Identifier](left: Expression[ID], right: Expression[ID])
  extends Equation[ID]
case class Conditional[ID <: Identifier](condition: Equation[ID], consequent: Equation[ID])
  extends Equation[ID]
