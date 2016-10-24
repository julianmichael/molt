package molt
package syntax
package agenda

import ordered._
import shapeless._

object Examples {

  // EXAMPLE USAGE

  import SyncProductionSyntax._

  // example of how to make productions
  case class ExpStr(x: String)
  case class ExpInt(i: Int)

  val StrSymb = new ParseSymbol[ExpStr]("Str")
  val StrSymb2 = new ParseSymbol[ExpStr]("Str")
  val IntSymb = new ParseSymbol[ExpInt]("Int")

  val genTerminals = (s: String) => {
    val term = OrderedStream.unit(Derivation(Terminal(s), s, 0.0))
    val num = scala.util.Try(s.toInt).toOption match {
      case None => OrderedStream.empty[Derivation]
      case Some(i) => OrderedStream.unit(Derivation(IntSymb, ExpInt(i), 1.0))
    }
    val arbStr = OrderedStream.unit(Derivation(StrSymb, ExpStr(s), 2.0))
    // term merge num merge arbStr
    num merge term
  }

  val prod1 = StrSymb to StrSymb usingSingle {
    case ExpStr(str) => Scored(ExpStr(s"($str)"), 1.0)
  }
  val prod2 = (IntSymb, StrSymb) to StrSymb using {
    case ExpInt(i) :: ExpStr(str) :: HNil => OrderedStream.unit(Scored(ExpStr(s"${i * i}:$str"), 1.0))
  }
  val prod3 = (IntSymb, t"+", IntSymb) to IntSymb usingSingle {
    case ExpInt(i) :: _ :: ExpInt(i2) :: HNil => Scored(ExpInt(i + i2), 1.0)
  }

  val grammar1 = SyncCFG(prod1 :: HNil)
  val grammar2 = SyncCFG(prod1 :: prod2 :: HNil)
  val grammar3 = SyncCFG(prod3 :: HNil)

  // more testy grammary
  val cnf1 = SyncCNFGrammar.productionsFromSyncCFG(grammar1)
  val cnf2 = SyncCNFGrammar.productionsFromSyncCFG(grammar2)
  val cnf3 = SyncCNFGrammar.productionsFromSyncCFG(grammar3)

  val parser1 = AgendaBasedSyncCNFParser.buildFromSyncCFG(genTerminals, grammar1)
  val parser2 = AgendaBasedSyncCNFParser.buildFromSyncCFG(genTerminals, grammar2)
  val parser = AgendaBasedSyncCNFParser.buildFromSyncCFG(genTerminals, grammar3)

  // def parseTest(s: String) = parser.parse(s.split(" ").toVector, IntSymb)

  object Calculator {
    val Num = new ParseSymbol[Int]("Num")
    val genlex = (s: String) => {
      val term = OrderedStream.unit(Derivation(Terminal(s), s, 0.0))
      val num = scala.util.Try(s.toInt).toOption match {
        case None => OrderedStream.empty[Derivation]
        case Some(i) => OrderedStream.unit(Derivation(Num, i, 1.0))
      }
      num merge term
    }
    val productions = {
      // val plus = (Num, Terminal("+"), Num) to Num using {
      //   case i :: _ :: j :: HNil => OrderedStream.unit(Scored(i + j, 1.0))
      // }
      // val minus = (Num, Terminal("-"), Num) to Num using {
      //   case i :: _ :: j :: HNil => OrderedStream.unit(Scored(i - j, 1.0))
      // }
      // val times = (Num, Terminal("*"), Num) to Num using {
      //   case i :: _ :: j :: HNil => OrderedStream.unit(Scored(i * j, 1.0))
      // }
      // val div = (Num, Terminal("/"), Num) to Num using {
      //   case i :: _ :: j :: HNil => OrderedStream.unit(Scored(i / j, 1.0))
      // }
      // val parens = (Terminal("("), Num, Terminal(")")) to Num using {
      //   case _ :: i :: _ :: HNil => OrderedStream.unit(Scored(i, 1.0))
      // }
      // plus :: minus :: times :: div :: parens :: HNil
      ???
    }
    val grammar = SyncCFG(productions)
    // val parser = AgendaBasedSyncCNFParser.buildFromSyncCFG(genlex, grammar)
    // def parseTest(s: String) = parser.parse(s.split(" ").toVector, Num)
  }
}
