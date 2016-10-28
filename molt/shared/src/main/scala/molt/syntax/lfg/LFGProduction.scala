package molt.syntax.lfg

import molt.syntax.cfg.CFGProduction
import molt.syntax.cfg.ASTTag

case class LFGProduction[+A](
  head: A,
  children: List[(ASTTag[A], Specification)]) {

  val cfgProduction: CFGProduction[A] = CFGProduction(head, children.map(_._1))
}