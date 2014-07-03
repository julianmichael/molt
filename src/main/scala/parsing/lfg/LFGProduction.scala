package parsing.lfg

import parsing.Production

case class LFGProduction[+A](
  head: A,
  children: List[(A, Specification)]) {

  val cfgProduction: Production[A] = Production(head, children.map(_._1))
}