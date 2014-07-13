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
class BasicTokenizer(tokens: Set[String]) extends Tokenizer {
  // TODO warn if tokens can have overlap. leads to ambiguous tokenization
  private def getOverlaps(toks: List[String]): List[(String, String)] = toks match {
    case Nil => Nil
    case head :: tail => {
      val prefixes = head.scanLeft("")(_ + _).filterNot(_.isEmpty)
      val suffixes = head.scanRight("")(_ + _).filterNot(_.isEmpty)
      val culprits = tail.filter(_.contains(head)) ++
        tail.filter(tok => prefixes.exists(prefix => tok.endsWith(prefix))) ++
        tail.filter(tok => suffixes.exists(suffix => tok.startsWith(suffix)))
      culprits.map(x => (head, x)) ++ getOverlaps(tail)
    }
  }
  val overlaps = getOverlaps(tokens.toList).toSet
  val warning =
    if(!overlaps.isEmpty) {
      val str = s"Warning: tokens have overlap: $overlaps"
      Console.err.println(str)
      str
    }
    else "No token overlap detected :)"

  // tokenization as described above
  def tokenize(s: String) = {
	// split on spaces. this is a reversible design decision.
    val unTokenizedStringVector = s.split("\\s+").toList

    // to turn a single string into a list with the specified terminal split out
    def splitSingleString(str: String, tok: String): List[String] = {
      if (str.isEmpty)
        Nil
      else if (!str.contains(tok) || str.equals(tok))
        List(str)
      else {
        val (head, tail) = str.splitAt(str.indexOf(tok))
        val remainder = tok :: splitSingleString(tail.substring(tok.length), tok)
        if (head.isEmpty)
          remainder
        else
          head :: remainder
      }
    }

    // does the above function over a list of strings to get a new list with all of the tokens
    def splitOutToken(strs: List[String], tok: String): List[String] = {
      strs.flatMap(splitSingleString(_, tok))
    }

    // we do the splitting for every terminal and get our final token list 
    tokens.foldLeft(unTokenizedStringVector)(splitOutToken)
  }
}