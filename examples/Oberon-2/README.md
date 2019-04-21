# Parser for the Oberon-2 programming language

The grammar is based on the grammar from the
[Oberon-2](http://www.ssw.uni-linz.ac.at/Research/Papers/Oberon2.pdf) report, appendix B.
The syntax was changed to LLtool syntax and conflict resolvers were added for
LL(1) conflicts in the grammar.

The grammar is specified in [oberon2.g](source/oberon2.g).

## Usage

The parser requires exactly on source code file as argument:

    oberon2 example.mod

Languages examples can be found in the [Wikipedia entry](https://en.wikipedia.org/wiki/Oberon-2) or
in the book [Object-Oriented Programming in Oberon-2](http://ssw.jku.at/Research/Books/Oberon2.pdf).

For syntax errors, the parser returns an error message. Otherwise it just prints `Done.`.