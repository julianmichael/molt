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
yet exist. These are the theoretical constructs currently implemented:

* Context-Free Grammar. This includes a scheduling chart parser that can
  achieve total parsing even on recursive grammars, and sentences that
  have an infinite number of parse trees (see ```parsing/cnf/smart```).
* Lexical Functional Grammar. Completeness and coherence are implemented,
  as well as inside-out function application, but the following features
  remain to be done:
  * Functional uncertainty
  * Off-path constraints
  * f-precedence and binding

The next theories to be implemented will be some form of Paradigm Functional
Morphology and Lexical Mapping Theory. The LFG and LMT implementations will
draw from Lexical-Functional Syntax, 2nd ed., and PFM will be drawn from a
source to be decided.
