[![Build status](https://img.shields.io/travis/redstar/LLtool/master.svg?logo=travis&label=Travis%20CI)][1]

# LLtool
A recursive-descent parser generator for D

## Purpose

LLtool generates the body of a parser from a context free grammar written in EBNF.
The generated code fragment can be mixed into D source.

## Interface

The generated code makes some assumptions about the environment:

- It assumes there are functions `expect()`, `consume()` and `advance()`.
- It assumes there exists an enumeration `TokenKind`.
- It assumes there is a member/variable `tok`. The type of `tok` is not important.
  Only a member/property `kind` (of type `TokenKind`) is required.
- Currently the generator assumes that `std.algorithm.among` is imported.

## Syntax

The input for LLtool has the following specification:

    %token identifier, code, argument, string
    %start lltool
    %%
    lltool = ( header )? ( rule )+ .

    header = ( "%start" identifier | "%token" tokenlist | "%eoi" identifier )* "%%" .

    tokenlist = tokendecl ("," tokendecl )* .

    tokendecl = (identifier | string) ( "=" identifier )? .

    rule = nonterminal "=" rhs "." .

    nonterminal = identifier ( argument )? .

    rhs = sequence ( "|" sequence )* .

    sequence = ( group | identifier ( argument)? | string | code | "%if" code )* .

    group = "(" rhs ( ")" | ")?" | ")*" | ")+" ) .

This specification uses the following tokens:

- `identifier`: a sequence of letters and digits. First element must be a letter.
  Only ASCII characters are support.
- `string`: an arbitrary sequence of characters, enclosed by `"` and `"`.
- `code`: an arbitrary sequence of characters, enclosed by `(.` and `.)`.
- `argument`: an arbitrary sequence of characters, enclosed by `<` and `>` or `<.` and `.>`.

Single-line comments start with `//` and run until the end of line.
Multi-line comments use `/*` and `*/` as delimiters. Multi-line comments may not
be nested.

## Open tasks

- Support parser generation at compile time
- Implement error recovery

Feedback of any kind is very much appreciated!

[1]: https://travis-ci.org/redstar/LLtool/branches
