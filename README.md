MoLT
=======

MoLT, the Modular Linguistics Toolkit, is a project in its early development.
It has two primary goals:

* Formalize linguistic theories in such a way that they may be freely composed
  and subjected to rigorous and uniform testing to empirically compare their
  theoretical merits.
* Form the basis of a library for application developers to create
  natural-language interfaces for domain-specific uses, which may not require
  broad coverage and are more amenable to full linguistic formalization.

Currently, the architecture of MoLT is in flux, and documentation does not
yet exist (other than this readme).

State of the Project
--------------------

These are the theoretical constructs currently implemented:

* Context-Free Grammar. The package ```molt.syntax.cfg``` contains utilities
  for the creation and parsing of CFGs. There are two parsing methodologies:
  * Traditional CKY parsing. The implementation closely follows the algorithm
    described in [Lange and Leiss 2009][lange-leiss-2009] to produce all of
    the possible parse trees of a sentence that do not have any "cyclic unit
    parses" within them, i.e., where a nonterminal symbol produces nothing
    but a copy of itself (and any number of copies of the empty string).
  * Scheduled chart parsing. This implementation achieves faithful total
    parsing for arbitrary CFGs using scheduled parsing. A user may decide
    the scheduling technique, and it may depend on a richer data structure
    constructed from the parse tree (like a meaning representation), as
    long as the ordering on these structures (which determines the schedule)
    grows monotonically with respect to the inclusion relation (itself an
    ordering) on the original CFG parse trees.
* Lexical Functional Grammar. The package ```molt.syntax.lfg``` contains
  utilities for creating and parsing LFGs. It currently uses the
  traditional CKY CFG parser behind the scenes, but it will eventually
  be parameterized over a choice of CFG parser. Completeness and coherence
  are implemented, as well as inside-out function application, but the
  following features remain to be done:
  * Extended coherence
  * Functional uncertainty
  * Off-path constraints
  * f-precedence and binding

The next theories to be implemented will be some form of Paradigm Functional
Morphology and Lexical Mapping Theory. The LFG and LMT implementations
draw from Lexical-Functional Syntax, 2nd ed., and PFM will be drawn from a
source to be decided.

On the Roster
-------------

Here is a complete list of linguistic (and auxiliary) formalisms that are
currently planned for potential implementation
(this list is subject to revision, and especially growth):
* Logic
  * Propositional logic
  * First-Order logic
  * First-Order Intensional Logic (a la Montague)
  * Lambda Calculus
* Lexicon and Morphology
  * Paradigm Functional Morphology
  * Lexical Mapping Theory
* Syntax
  * Context Free Grammar
  * X-Bar Theory / Endocentricity
  * Government & Binding Theory
  * Lexical Functional Grammar
  * Head-Driven Phrase Structure Grammar
  * Combinatory Categorial Grammar
  * Tree Adjoining Grammar
  * Montague Grammar
* Semantics
  * Montague Grammar
  * Davidsonian FOL Semantics
  * Neo-Davidsonian FOL Semantics
  * Discourse Representation Theory

[lange-leiss-2009]: http://www.informatica-didactica.de/cmsmadesimple/index.php?page=LangeLeiss2009
                  "To CNF or not to CNF? An Efficient Yet Presentable Version of the CYK Algorithm"
