package parsing

// encodes a single production in a grammar. Also gives us
// a convenient String representation.
trait Production {
  def label: String
  def children: List[String]
  def toCNF: Set[CNFProduction]
  override def toString = s"$label -> ${children.mkString(" ")}"
}
object Production {
  def fromString(s: String): Option[Production] = {
    val tokens = s.split("\\s+")
    if (tokens.length < 3 || tokens(1) != "->")
      None
    else {
      Some(RawProduction(tokens(0), tokens.drop(2).toList))
    }
  }
}

// for any general (finite) production with a bunch of children
case class RawProduction(
  label: String,
  children: List[String])
  extends Production {
  
  override lazy val toCNF: Set[CNFProduction] = children match {
    case child :: Nil         => Set(Unary(label, child))
    case left :: right :: Nil => Set(Binary(label, left, right))
    case left :: remainder => {
      val nextHead = makeChunkedSymbol(remainder)
      collapseChildren(remainder) + Binary(label, left, nextHead)
    }
  }

  private def makeChunkedSymbol(symbols: List[String]) = symbols match {
    case Nil         => ""
    case head :: Nil => head
    case list        => s"{${list.mkString("+")}}"
  }
  private def collapseChildren(symbols: List[String]): Set[CNFProduction] = symbols match {
    case Nil | _ :: Nil => Set()
    case left :: right :: Nil => {
      val name = makeChunkedSymbol(symbols)
      Set(ChunkedBinary(name, left, right))
    }
    case head :: tail => {
      val name = makeChunkedSymbol(symbols)
      val left = head
      val right = makeChunkedSymbol(tail)
      collapseChildren(tail) + ChunkedBinary(name, left, right)
    }
  }
}

// for all productions guaranteed to be in Chomsky Normal Form
// plus unary productions
sealed abstract class CNFProduction extends Production {
  lazy val toCNF = Set(this)
}
case class ChunkedBinary(label: String, left: String, right: String) extends CNFProduction {
  lazy val children = List(left, right)
}
case class Binary(label: String, left: String, right: String) extends CNFProduction {
  lazy val children = List(left, right)
}
case class Unary(label: String, child: String) extends CNFProduction {
  lazy val children = List(child)
}