package parsing.tokenize

/******
 * MaximalMunchTokenizer
 * Tokenizer that employs the "maximal munch" rule for strings that are and are
 * not prefixes of tokens. This is nice because it is deterministic and allows
 * overlapping tokens nicely: if you're bigger and/or you come first, you get to
 * be the token.
*****/
sealed abstract class MunchingState
case class MunchingExtras(nonToken: String) extends MunchingState
case class MunchingPrefix(nonToken: String, prefix: String) extends MunchingState
case class MunchingPrefixWithBackup(
  nonToken: String, prefix: String, backup: String) extends MunchingState

class MaximalMunchTokenizer(allTokens: Set[String]) extends Tokenizer {
  override def tokenizations(s: String): Set[Seq[String]] = {
    val processor = new MunchingStateProcessor
    val finalState: MunchingState = s.replaceAll("\\s+", " ").trim
      .foldLeft(processor.initialState)(processor.processStateForChar _)
    processor.finalizeState(finalState)
    Set(processor.tokens)
  }

  private[this] class MunchingStateProcessor {
    var tokens = Seq.empty[String]
    val initialState: MunchingState = MunchingExtras("")
    def isPrefix(pre: String): Boolean = allTokens.exists(_.startsWith(pre))
    def isToken(tok: String): Boolean = allTokens.contains(tok)
    def processStateForChar(st: MunchingState, chr: Char): MunchingState = {
      val c = chr.toString
      if(c == " ") {
        finalizeState(st)
        initialState
      }
      else st match {
        case MunchingExtras(nonToken) =>
          if(isPrefix(c)) {
            if(isToken(c)) MunchingPrefixWithBackup(nonToken, c, c)
            else MunchingPrefix(nonToken, c)
          } else MunchingExtras(nonToken + c)
        case MunchingPrefix(nonToken, prefix) =>
          if(isPrefix(prefix + c)) {
            if(isToken(prefix + c)) MunchingPrefixWithBackup(nonToken, prefix + c, prefix + c)
            else MunchingPrefix(nonToken, prefix + c)
          } else MunchingExtras(nonToken + prefix + c)
        case MunchingPrefixWithBackup(nonToken, prefix, backup) =>
          if(isPrefix(prefix + c)) {
            if(isToken(prefix + c)) MunchingPrefixWithBackup(nonToken, prefix + c, prefix + c)
            else MunchingPrefixWithBackup(nonToken, prefix + c, backup)
          } else {
            tokens = tokens :+ nonToken :+ backup
            val newPrefix = prefix.substring(backup.length)
            // backtrack and reprocess the uncommitted part
            newPrefix.foldLeft(initialState)(processStateForChar _)
          }
      }
    }

    // commit the final token
    def finalizeState(st: MunchingState): Unit = st match {
      case MunchingExtras("") => ()
      case MunchingExtras(nonToken) => tokens = tokens :+ nonToken
      case MunchingPrefix(nonToken, prefix) => tokens = tokens :+ (nonToken + prefix)
      case MunchingPrefixWithBackup(nonToken, prefix, backup) => {
        if(nonToken.isEmpty) tokens = tokens :+ backup
        else tokens = tokens :+ nonToken :+ backup
        val newPrefix = prefix.substring(backup.length)
        finalizeState(newPrefix.foldLeft(initialState)(processStateForChar _))
      }
    }
  }
}