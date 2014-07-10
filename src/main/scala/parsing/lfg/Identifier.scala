package parsing.lfg

import parsing._

sealed trait Identifier

sealed abstract class RelativeIdentifier extends Identifier
case object Up extends RelativeIdentifier
case object Down extends RelativeIdentifier
object RelativeIdentifier extends ComplexParsable[RelativeIdentifier] {
  override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[RelativeIdentifier])] = Map(
    List(Terminal("up")) -> (c => Some(Up)),
    List(Terminal("down")) -> (c => Some(Down))
  )
}

case class AbsoluteIdentifier(id: String) extends Identifier {
  override def toString = id
}
object AbsoluteIdentifier {
  implicit val ordering: Ordering[AbsoluteIdentifier] =
    Ordering.by { case AbsoluteIdentifier(id) => id }
  def freshID(prohibited: Set[AbsoluteIdentifier]): AbsoluteIdentifier = {
    Stream.from(0).map(n => AbsoluteIdentifier(s"$n")).dropWhile(prohibited).head
  }
}
