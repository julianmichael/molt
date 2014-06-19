package parsing

trait ParsableTestParameters[A] {
  def children: Set[Parsable[_]]
  def nonterminals: Set[String]
  def terminals: Set[String]
  def productions: Set[Production]
  def cnfProductions: Set[CNFProduction]
  def goodStrings: List[String]
  def goodTokenizations: List[List[String]]
  def goodASTs: List[AST]
  def goodSymbolics: List[A]
}