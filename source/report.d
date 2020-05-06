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
import std.algorithm : filter;
import std.stdio;

void printReport(Grammar grammar)
{
    writeln("Analyzing results");
    writeln("=================");
    writeln();
    writeln("Nonterminals");
    writeln("------------");
    writeln();
	foreach (node; filter!(n => n.type == NodeType.Nonterminal)(grammar.nodes))
    {
        writefln(" - %s (derives Epsilon: %s)", node.name, yesno(node.derivesEpsilon));
    }
    writeln();
    writeln("Terminals");
    writeln("---------");
    writeln();
 	foreach (node; filter!(n => n.type == NodeType.Terminal)(grammar.nodes))
    {
        writefln(" - %s", node.name);
    }
    writeln();
    writeln("First sets");
    writeln("---------");
    writeln();
	foreach (node; filter!(n => n.type == NodeType.Nonterminal)(grammar.nodes))
    {
        writefln(" - %s:", node.name);
        size_t col = 0;
        foreach (t; node.link.firstSet)
        {
            if (col == 0) {
                writef("    %s", t);
                col += 4 + t.length;
            }
            else
            {
                if (col + t.length < 79)
                {
                    writef(", %s", t);
                    col += 2 + t.length;
                }
                else
                {
                    writef("\n    %s", t);
                    col = 4 + t.length;
                }
            }
        }
        writeln();
    }
    writeln();
    writeln("Follow sets");
    writeln("---------");
    writeln();
	foreach (node; filter!(n => n.type == NodeType.Nonterminal)(grammar.nodes))
    {
        writefln(" - %s:", node.name);
        size_t col = 0;
        foreach (t; node.link.followSet)
        {
            if (col == 0) {
                writef("    %s", t);
                col += 4 + t.length;
            }
            else
            {
                if (col + t.length < 79)
                {
                    writef(", %s", t);
                    col += 2 + t.length;
                }
                else
                {
                    writef("\n    %s", t);
                    col = 4 + t.length;
                }
            }
        }
        writeln();
    }
}

private:
string yesno(bool b)
{
    return b ? "yes" : "no";
}