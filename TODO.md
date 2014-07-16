To Do List
==========

 * A method of doing CFG parsing with productions of the empty string.
 * A whitespace-sensitive preprocessor that can add in brackets or other
   delimiters where changes in scope are detected. (See the whitespace
   sensitivity in Haskell for inspiration.)
 * A system for requiring certain coherence and completeness conditions on
   F-Structures: this requires a notion of "governable" grammatical functions.
   This could be specified to the LexicalFunctionalGrammar or could be
   implemented in a layer on top if some users may not be concerned with
   coherence and completeness. (the latter seems like the attractive option.)
 * An example system for producing symbolic objects from F-Structures.
 * Investigate refactoring some of the "parsing" code into a "parsing.cfg"
   package
 * Implement a check for cycles in the graph of unary productions in the CFG.