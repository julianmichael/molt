package parsing

/* THESE FUNCTIONS ARE THE PREFERRED WAY TO PARSE!
  * The other ways are dispreferred!
  * Kind of, not really. Do your own thing. Whatever.
  * */
object ParseCommands {
  def parse[A](str: String)(implicit parsable: Parsable[A]): Set[A] =
    parsable.fromString(str)
  def parseUnique[A](str: String)(implicit parsable: Parsable[A]): Option[A] =
    parsable.fromStringUnique(str)
  def parseForced[A](str: String)(implicit parsable: Parsable[A]): A =
    parseUnique(str).get
}
