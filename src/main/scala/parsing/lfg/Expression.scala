package parsing.lfg

sealed abstract class Expression[+ID <: Identifier] {
  def ground(up: AbsoluteIdentifier, down: AbsoluteIdentifier)
    (implicit evidence: ID <:< RelativeIdentifier): Expression[AbsoluteIdentifier] = this match {
    // TODO make this pattern matching nicer :(
    case IdentifierExpression(x) if x == Up => IdentifierExpression(up)
    case IdentifierExpression(x) if x == Down => IdentifierExpression(down)
    case Application(exp, feat) => Application(exp.ground(up, down), feat)
    case ValueExpression(v) => ValueExpression(v)
  }
}
case class IdentifierExpression[ID <: Identifier](id: ID)
  extends Expression[ID]
case class Application[ID <: Identifier](exp: Expression[ID], feature: Feature)
  extends Expression[ID]
case class ValueExpression(v: Value)
  extends Expression[Nothing]
