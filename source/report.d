// Written in the D programming language.
/**
 * Reporting about grammer as markdown.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module report;

import grammar;
import std.stdio;

void print(Grammar g)
{
    writeln("Analyzing results");
    writeln("=================");
    writeln();
    writeln("Nonterminals");
    writeln("------------");
    writeln();
    foreach (nt; g.nodes)
        writefln(" - %s", nt.name);
    writeln();
    writeln("Terminals");
    writeln("---------");
    writeln();
    //foreach (t; g.terminals)
    //    writefln(" - %s", t.name);
}