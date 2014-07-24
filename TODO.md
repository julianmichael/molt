To Do List
==========

 * A whitespace-sensitive preprocessor that can add in brackets or other
   delimiters where changes in scope are detected. (See the whitespace
   sensitivity in Haskell for inspiration.) It will take in "indentation
   indicator tokens" that will trigger the mechanism. (like Haskell keywords)

 * A way to specify wildcards and do basic functional uncertainty

 * A system for requiring certain coherence and completeness conditions on
   F-Structures: this requires a notion of "governable" grammatical functions.
   This could be specified to the LexicalFunctionalGrammar or could be
   implemented in a layer on top if some users may not be concerned with
   coherence and completeness. (the latter seems like the attractive option.)

 * An example system for producing symbolic objects from F-Structures.

 * Thread failure reporting, somehow, through the parsing calculations.