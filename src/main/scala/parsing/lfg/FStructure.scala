package parsing.lfg

import scalaz._
import Scalaz._

case class FStructure(
  map: Map[AbsoluteIdentifier, FStructurePart],
  root: AbsoluteIdentifier) {

  def pretty: String = {
    type TabState[A] = State[(Int, String), A]
    def getTabs: TabState[Int] = get[(Int, String)] map { case (int, str) => int }
    def getString: TabState[String] = get[(Int, String)] map { case (int, str) => str }
    def putTabs(tabs: Int): TabState[Unit] =
      get[(Int, String)] flatMap (sta => put(sta.copy(_1 = tabs)))
    def putString(s: String): TabState[Unit] =
      get[(Int, String)] flatMap (sta => put(sta.copy(_2 = s)))
    def indent: TabState[Unit] = for {
      tabs <- getTabs
      _ <- putTabs(tabs + 1)
    } yield ()
    def unindent: TabState[Unit] = for {
      tabs <- getTabs
      _ <- putTabs(tabs - 1)
    } yield ()
    def addPrefixedString(prefix: String, added: String): TabState[Unit] = for {
      tabs <- getTabs
      string <- getString
      indentation = Vector.fill(tabs)("  ").mkString("")
      _ <- putString(s"$string\n$indentation$prefix$added")
    } yield ()
    def addString(added: String): TabState[Unit] = addPrefixedString("", added)
    def prettyForID(prefix: String, id: AbsoluteIdentifier): TabState[Unit] = map(id) match {
      case Empty => addPrefixedString(prefix, "[]")
      case FMapping(m) => for {
        _ <- addPrefixedString(prefix, "[")
        _ <- indent
        _ <- m.foldLeft(get[(Int, String)] map (x => ())){
          case (sta, (feat, subid)) => for {
            _ <- sta
            _ <- prettyForID(s"$feat ==> ", subid)
          } yield ()
        }
        _ <- unindent
        _ <- addString("]")
      } yield ()
      case FSet(s) => for {
        _ <- addPrefixedString(prefix, "{")
        _ <- indent
        _ <- s.foldLeft(get[(Int, String)] map (x => ())) {
          case (sta, subid) => for {
            _ <- sta
            _ <- prettyForID("", subid)
          } yield ()
        }
        _ <- unindent
        _ <- addString("}")
      } yield ()
      case FValue(v) => addPrefixedString(prefix, v)
      case FSemanticForm(s) => addPrefixedString(prefix, s)
    }
    (for {
      _ <- prettyForID("", root)
      string <- getString
    } yield string).eval((0, ""))
  }
}

sealed abstract class FStructurePart
case object Empty extends FStructurePart
case class FMapping(map: Map[Feature, AbsoluteIdentifier]) extends FStructurePart
case class FSet(set: Set[AbsoluteIdentifier]) extends FStructurePart
case class FValue(v: Value) extends FStructurePart
case class FSemanticForm(s: SemanticForm) extends FStructurePart
