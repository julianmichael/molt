package parsing.lfg

import parsing.cfg.CFGProduction

case class LFGProduction[+A](
  head: A,
  children: List[(A, Specification)]) {

  val cfgProduction: CFGProduction[A] = CFGProduction(head, children.map(_._1))
}