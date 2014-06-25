package parsing

trait Tokenizer {
  @Deprecated
  def tokenize(s: String): List[String]
  
  // in progress; not implemented at all anywhere
  def tokenizations(s: String): Set[Vector[String]] = ???
}


// We find all of the terminal symbols and make sure we split on
// them, then assume everything in between is contiguous. As in
// the definition of propositional logic, we are restricting atoms
// from containing any of our canonical terminal symbols.
class BasicTokenizer(terminals: Set[String]) extends Tokenizer {
  // TODO warn if terminals can have overlap. leads to ambiguous tokenization
  
  // tokenization as described above
  def tokenize(s: String) = {
	// split on spaces. this is a reversible design decision.
    val unTokenizedStringVector = s.split("\\s+").toList

    // to turn a single string into a list with the specified terminal split out
    def splitSingleString(str: String, terminal: String): List[String] = {
      if (str.isEmpty)
        Nil
      else if (!str.contains(terminal) || str.equals(terminal))
        List(str)
      else {
        val (head, tail) = str.splitAt(str.indexOf(terminal))
        val remainder = terminal :: splitSingleString(tail.substring(terminal.length), terminal)
        if (head.isEmpty)
          remainder
        else
          head :: remainder
      }
    }

    // does the above function over a list of strings to get a new list with all of the tokens
    def splitOutTerminal(strs: List[String], terminal: String): List[String] = {
      strs.flatMap(splitSingleString(_, terminal))
    }

    // we do the splitting for every terminal and get our final token list 
    val tokens = terminals.foldLeft(unTokenizedStringVector)(splitOutTerminal)
    tokens
  }
}