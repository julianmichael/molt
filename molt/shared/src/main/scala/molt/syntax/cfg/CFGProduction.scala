package molt.syntax.cfg

import molt.syntax.cnf._

case class CFGProduction[+A](head: A, children: List[ASTTag[A]])
