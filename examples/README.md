# Example grammars for LLtool

- [expr.g](expr.g) is a grammar for arithmetic expression. Very simple and good
  for exploring the generated source code.
- [modula2.g](modula2.g) is a grammar for Modula-2, PIM4 variant. All LL(1)
  conflicts in the grammar are resolved.
- [Oberon-2](Oberon-2) is a complete example with parser and lexer.

  The LLtool grammar is [oberon2.g](Oberon-2/source/oberon2.g) and the generated
  D source is [oberon2.mixin](Oberon-2/source/oberon2.mixin).

The directory [wip](wip) contains grammars which are not yet ready.

- ISO Modula-2: [isomodula-2.g](wip/isomodula-2)
- ANSI-C 99: [c.g](wip/c.g)
- SQLite SQL Select: [SQL.g](wip/SQL.g)