package parsing.lfg

import parsing.cfg.CFGProduction
import parsing.cfg.ASTTag

case class LFGProduction[+A](
  head: A,
  children: List[(ASTTag[A], Specification)]) {

  val cfgProduction: CFGProduction[A] = CFGProduction(head, children.map(_._1))
}