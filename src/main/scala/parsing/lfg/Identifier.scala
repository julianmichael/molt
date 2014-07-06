package parsing.lfg

sealed trait Identifier

sealed abstract class RelativeIdentifier extends Identifier
case object Up extends RelativeIdentifier
case object Down extends RelativeIdentifier

case class AbsoluteIdentifier(id: String) extends Identifier
object AbsoluteIdentifier {
  implicit val ordering: Ordering[AbsoluteIdentifier] =
    Ordering.by { case AbsoluteIdentifier(id) => id }
  def freshID(prohibited: Set[AbsoluteIdentifier]): AbsoluteIdentifier = {
    Stream.from(0).map(n => AbsoluteIdentifier(s"$n")).dropWhile(prohibited).head
  }
}
