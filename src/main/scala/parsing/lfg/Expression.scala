package parsing.lfg

import parsing._

sealed abstract class Expression[ID <: Identifier] {
  def ground(up: AbsoluteIdentifier, down: AbsoluteIdentifier)
    (implicit evidence: ID <:< RelativeIdentifier): Expression[AbsoluteIdentifier] = this match {
    case FunctionalExpression(x) => FunctionalExpression(x.ground(up, down))
    case ValueExpression(v) => ValueExpression(v)
    case SemanticFormExpression(s) => SemanticFormExpression(s)
  }
  def identifiers: Set[ID] = this match {
    case FunctionalExpression(x) => x.identifiers
    case ValueExpression(_) => Set.empty[ID]
    case SemanticFormExpression(_) => Set.empty[ID]
  }
}
case class FunctionalExpression[ID <: Identifier](exp: IdentifyingExpression[ID])
  extends Expression[ID]
case class ValueExpression[ID <: Identifier](v: Value)
  extends Expression[ID]
case class SemanticFormExpression[ID <: Identifier](s: SemanticForm)
  extends Expression[ID]

abstract class IdentifyingExpression[ID <: Identifier] {
  def ground(up: AbsoluteIdentifier, down: AbsoluteIdentifier)
    (implicit evidence: ID <:< RelativeIdentifier): IdentifyingExpression[AbsoluteIdentifier] = this match {
    case BareIdentifier(x) if x == Up => BareIdentifier(up)
    case BareIdentifier(x) if x == Down => BareIdentifier(down)
    case Application(exp, feat) => Application(exp.ground(up, down), feat)
  }
  def identifiers: Set[ID] = this match {
    case BareIdentifier(x) => Set(x)
    case Application(exp, _) => exp.identifiers
  }
}

case class BareIdentifier[ID <: Identifier](id: ID)
  extends IdentifyingExpression[ID]
case class Application[ID <: Identifier](exp: IdentifyingExpression[ID], feature: Feature)
  extends IdentifyingExpression[ID]
