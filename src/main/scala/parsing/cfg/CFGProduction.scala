package parsing.cfg

import parsing.cnf._

case class CFGProduction[+A](head: A, children: List[ASTTag[A]])
