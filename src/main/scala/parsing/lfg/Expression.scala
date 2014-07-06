package parsing.lfg

sealed abstract class Expression[ID <: Identifier] {
  def ground(up: AbsoluteIdentifier, down: AbsoluteIdentifier)
    (implicit evidence: ID <:< RelativeIdentifier): Expression[AbsoluteIdentifier] = this match {
    // TODO make this pattern matching nicer :(
    case IdentifierExpression(x) if x == Up => IdentifierExpression(up)
    case IdentifierExpression(x) if x == Down => IdentifierExpression(down)
    case Application(exp, feat) => Application(exp.ground(up, down), feat)
    case ValueExpression(v) => ValueExpression(v)
  }
  def identifiers: Set[ID] = this match {
    case IdentifierExpression(x) => Set[ID](x)
    case Application(exp, feat) => exp.identifiers
    case ValueExpression(v) => Set.empty[ID]
  }
}
case class IdentifierExpression[ID <: Identifier](id: ID)
  extends Expression[ID]
case class Application[ID <: Identifier](exp: Expression[ID], feature: Feature)
  extends Expression[ID]
// TODO perhaps manage to get rid of the type parameter here. ID can't be
// covariant because "identifiers" returns a set, which is invariant in ID.
case class ValueExpression[ID <: Identifier](v: Value)
  extends Expression[ID]
