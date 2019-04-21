// Written in the D programming language.
/**
 * This support module provides functions to output internal data structures
 * in DOT language.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module graphviz;

import std.algorithm : among;
import std.format : format;
import std.string : replace;
import grammar;

string id(Node n)
{
    string s = format("\"%s @ %d", n.type, n.pos);
    if (n.type.among!(NodeType.Nonterminal,NodeType.Terminal,NodeType.Symbol))
        s = format("%s - `%s`", s, replace(n.name, "\"", "'"));
    if (n.type == NodeType.Group)
        s = format("%s %s", s, n.cardinality);
    return s ~ "\"";
}

string edge(alias prop)(Node n)
{
    auto v = __traits(getMember, n, prop);
    if (v !is null)
        return format("%s -> %s [label = \"%s\"]", id(n), id(v), prop);
    return "";
}

string toGraphviz(Grammar grammar)
{
    bool[Node] seen;
    string s = "digraph {\n";

    void line(string str)
    {
        if (str.length)
            s ~= format("  %s;\n", str);
    }

    void dfs(Node n)
    {
        seen[n] = true;
        line(id(n));
        line(edge!"next"(n));
        line(edge!"inner"(n));
        line(edge!"back"(n));
        line(edge!"link"(n));
    }

    foreach (n; grammar.nodes)
        if (n !in seen)
            dfs(n);
    s ~= "}";
    return s;
}

struct GraphvizBuilder
{
    import std.container.array;

    bool[Node] seen;
    Array!string lines;

    void add(Node n)
    {
        if (n !in seen)
        {
            seen[n] = true;
            line(id(n));
        }
    }

    void add(Node p, Node q)
    {
        add(p);
        add(q);
        line(format("%s -> %s", id(p), id(q)));
    }

    private void line(string str)
    {
        if (str.length)
            lines ~= format("  %s;\n", str);
    }

    string toString()
    {
        import std.string : join;

        return "digraph {\n" ~ join(lines[]) ~ "}";
    }
}