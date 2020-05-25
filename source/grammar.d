// Written in the D programming language.
/**
 * This module defines the internal data structur for grammars.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module grammar;
/**
A grammar is a special kind of graph. There are different types of nodes:

- Nonterminals are the left hand side of a production.
-

Node types:
- Nonterminal
  next: only Group/Alternative/Symbol
- Group
  next: only Group/Alternative/Symbol
- Alternative
  next: only Group/Alternative/Symbol
- Symbol
  points to Nonterminal or Terminal


enum NodeType
{
    Terminal, Nonterminal, Group, Alternative, Symbol
}

struct Node
{
    // The type of the node
    NodeType type;

    // All nodes refer to a position in source file
    size_t pos;

    // Name only filled for Terminal and Nonterminal
    string name;
}

*/

import diagnostics;
//import std.bitmanip;
import std.algorithm : among, filter;
import std.container.rbtree;
import std.range;

alias TerminalSet = RedBlackTree!(string, "a < b", false);

struct Grammar
{
    Node[] nodes;
    Node startSymbol;
    Node syntheticStartSymbol;
    Node eoiTerminal;

    string language;

    auto nonterminals()
    {
        return filter!(n => n.type == NodeType.Nonterminal)(nodes);
    }
}

struct GrammarBuilder
{
    immutable(char)[] data;
    Node[] nodes;

    size_t startPos;
    string startName;

    size_t eoiPos;
    string eoiName;

    string lang;

    Node[string] terminals;

    void init(immutable(char)[] buffer)
    {
        data = buffer;
    }

    private Node addSyntheticStart(Node startSymbol, Node eoiTerminal)
    {
        // The following adds a synthetic rule "" = <Symbol> "_eof" .
        // Create start node. This is always the first node in array.
        auto start = nonterminal(0, "");
        auto node = symbol(0, startSymbol.name);
        node.inner = startSymbol;
        start.link = sequence(0);
        start.link.inner = node;
        start.link.back = start;
        node.next = symbol(0, eoiTerminal.name);
        node.next.back = start.link;
        return start;
    }

    Grammar finalize()
    {
        if (hasErrors) // Bail out if there was a syntax error
            return Grammar();
        auto eoiTerminal = terminal(0, "_eoi", eoiName);
        eoiTerminal.isReachable = true;
        auto startSymbol = findStartSymbol();
        auto syntheticStartSymbol = addSyntheticStart(startSymbol, eoiTerminal);
        resolve();
        if (hasErrors) // Bail out if there was a semantic error
            return Grammar();
        foreach (n; nodes)
            n.check;
        Grammar g = { nodes: nodes, startSymbol: startSymbol,
                      syntheticStartSymbol: syntheticStartSymbol,
                      eoiTerminal: eoiTerminal,
                      language: lang.length ? lang : "d" };
        return g;
    }

    private Node findStartSymbol()
    {
        if (startName.length)
        {
            foreach (n; filter!(n => n.type == NodeType.Nonterminal)(nodes))
                if (n.name == startName)
                    return n;
            error(data, startPos, "Start symbol %s not found.", startName);
        }
        else
        {
            foreach (n; filter!(n => n.type == NodeType.Nonterminal)(nodes))
                return n;
            error("No start symbol found.");
        }
        return null;
    }

    private void resolve()
    {
        import std.stdio : writefln;
        Node[string] namesOfNonterminals;

        foreach (n; filter!(n => n.type == NodeType.Nonterminal)(nodes))
        {
            if (auto node = n.name in namesOfNonterminals)
            {
                error(data, n.pos, "Duplicate nontermial %s", n.name);
                auto other = namesOfNonterminals[n.name];
                error(data, other.pos, "First definition of nontermial %s", other.name);
            }
            else
                namesOfNonterminals[n.name] = n;
        }

        foreach (n; filter!(n => n.type == NodeType.Symbol)(nodes))
        {
            if (n.inner is null)
            {
                if (auto v = (n.name in namesOfNonterminals))
                    n.inner = *v;
                else if (auto v = (n.name in terminals))
                    n.inner = *v;
                else
                {
                    error(data, n.pos, "Missing definition of nonterminal %s", n.name);
                    continue;
                }
            }

            // Link node to chain of occurances
            if (n.inner.type == NodeType.Nonterminal)
            {
                n.link = n.inner.back;
                n.inner.back = n;
            }
        }
    }

    private Node add(const NodeType type, const size_t position)
    {
        auto node = new NodeStruct(type, position);
        nodes ~= node;
        return node;
    }

    Node nonterminal(const size_t position, string name)
    {
        auto node = add(NodeType.Nonterminal, position);
        node.name = name;
        return node;
    }

    Node terminal(const size_t position, string name, string externalName = "")
    {
        if (name in terminals)
        {
            error(data, position, "Terminal %s already declared", name);
            return null;
        }
        else
        {
            auto node = add(NodeType.Terminal, position);
            node.name = name;
            node.externalName = externalName;
            terminals[name] = node;
            return node;
        }
    }

    Node symbol(const size_t position, string name, bool isTerminal = false)
    {
        auto node = add(NodeType.Symbol, position);
        node.name = name;
        if (isTerminal)
        {
            if (auto t = name in terminals)
                node.inner = *t;
            else
                node.inner = terminal(position, name);
        }
        return node;
    }

    Node code(const size_t position, string code)
    {
        auto node = add(NodeType.Code, position);
        node.code = code;
        return node;
    }

    Node sequence(const size_t position)
    {
        auto node = add(NodeType.Sequence, position);
        return node;
    }

    Node group(const size_t position, Cardinality cardinality)
    {
        auto node = add(NodeType.Group, position);
        node.cardinality = cardinality;
        return node;
    }

    Node alternative(const size_t position, Node seq)
    {
        assert(seq.type == NodeType.Sequence);
        auto node = add(NodeType.Alternative, position);
        node.link = seq;
        return node;
    }

    void startSymbol(size_t pos, string name)
    {
        if (startName.length)
        {
            warning(data, pos, "Start symbol is already defined. Ignoring new definition.");
        }
        else
        {
            startPos = pos;
            startName = name;
        }
    }

    void eoiSymbol(size_t pos, string name)
    {
        if (eoiName.length)
        {
            warning(data, pos, "End-of-input symbol is already defined. Ignoring new definition.");
        }
        else
        {
            eoiPos = pos;
            eoiName = name;
        }
    }

    void language(size_t pos, string name)
    {
        import std.uni : toLower;

        if (lang.length)
        {
            warning(data, pos, "Language is already defined. Ignoring new definition.");
        }
        else
        {
            name = name[1..$-1].toLower;
            if (name != "d" && name != "c++")
            {
                warning(data, pos, "Unknonw language " ~ name ~ ". Ignoring definition.");
            }
            else
            {
                lang = name;
            }
        }
    }
}

enum NodeType
{
    Terminal, Nonterminal, Group, Alternative, Sequence, Symbol, Code
}

enum Cardinality
{
    One,
    OneOrMore,
    ZeroOrOne,
    ZeroOrMore
}

enum CodeType
{
    Normal,
    Condition,
    Resolver,
    Predicate,
}

alias Node = NodeStruct*;

struct NodeStruct
{
public:
    const size_t pos;

    const NodeType type;

    Cardinality cardinality;

    // Next node in sequence
    Node next;

    // Link to next sequence (nonterminal, group or alternative)
    // or linked list of nonterminal occurances
    Node link;

    // Content of a group or alternative or symbol
    Node inner;

    // Parent node in graph.
    Node back;

    // Name of nonterminal/terminal
    string name;

    // External name of nonterminal/terminal
    string externalName;

    // Formal or actual arguments of nonterminal
    // Source code for code or predicate for resolver
    private string text;

    // Symbol information
    bool derivesEpsilon;
    bool isProductive;
    bool isReachable;

    // True if there is a LL(1) conflict
    bool hasConflict;

    // Kind of condition
    CodeType codeType;

    // FIRST and FOLLOW sets
    TerminalSet firstSet;
    TerminalSet followSet;

public:
    this(NodeType type, size_t pos, Cardinality cardinality)
    {
        this.type = type;
        this.pos = pos;
        this.cardinality = cardinality;
    }

    this(NodeType type, size_t pos)
    {
        this(type, pos, Cardinality.One);
    }

    string toString()
    {
        import std.conv : to;

        string s = "Node[Type=" ~ to!string(type)
        ~ ", Pos=" ~ to!string(pos);
        if (type.among!(NodeType.Nonterminal,NodeType.Terminal,NodeType.Symbol))
            s ~= ", Name=`" ~ name ~ "`";
        if (type == NodeType.Group)
            s ~= ", Cardinality=" ~ to!string(cardinality);
        s ~= ", Next=" ~ to!string(next !is null)
           ~ ", Link=" ~ to!string(link !is null)
           ~ ", Inner=" ~ to!string(inner !is null)
           ~ ", Back=" ~ to!string(back !is null);
        s ~= ", eps=" ~ to!string(derivesEpsilon)
           ~ ", prod=" ~ to!string(isProductive)
           ~ ", reach=" ~ to!string(isReachable);
        return s ~ "]";
    }

    public void check()
    {
        final switch (type)
        {
            case NodeType.Terminal:
                assert(name != "", toString);
                assert(next is null, toString);
                assert(inner is null, toString);
                break;
            case NodeType.Nonterminal:
                assert(name !is null, toString);
                assert(next is null, toString);
                assert(link !is null, toString);
                assert(link.type == NodeType.Sequence || link.type == NodeType.Alternative, toString);
                break;
            case NodeType.Group:
                assert(back !is null || next !is null, toString);
                assert(link !is null, toString);
                assert(link.type.among(NodeType.Sequence, NodeType.Alternative), toString);
                assert(link.back == &this, toString);
                break;
            case NodeType.Alternative:
                assert(back !is null, toString);
                assert(link !is null, toString);
                assert(link.type == NodeType.Sequence, toString);
                foreach (n; NodeLinkRange(link))
                {
                    assert(n.type == NodeType.Sequence, toString);
                    assert(n.back == &this, toString);
                }
                break;
            case NodeType.Sequence:
                assert(next is null, toString);
                assert(back !is null, toString);
                break;
            case NodeType.Symbol:
                assert(inner !is null, toString);
                break;
            case NodeType.Code:
                assert(inner is null, toString);
                assert(link is null, toString);
                break;
             /* case Resolver: */
        }
    }

    @property
    Node parent()
    {
        // The back pointer is only set for the first and last element of
        // a sequence.
        Node node = &this;
        while (node.back is null)
        {
            assert(node.next !is null);
            node = node.next;
        }
        return node.back;
    }

    @property
    Node succ()
    {
        Node node = next;
        while (node !is null && node.type == NodeType.Code)
            node = node.next;
        return node;
    }

    Node succInner()
    {
        Node node = inner;
        while (node !is null && node.type == NodeType.Code)
            node = node.next;
        return node;
    }

    @property
    string formalArgs()
    {
        import std.conv : to;
        assert(type == NodeType.Nonterminal, "Exp: Nonterminal, Actual: " ~ to!string(type));
        return text;
    }

    @property
    string formalArgs(string args)
    {
        assert(type == NodeType.Nonterminal);
        return this.text = args;
    }

    @property
    string actualArgs()
    {
        assert(type == NodeType.Symbol);
        return text;
    }

    @property
    string actualArgs(string args)
    {
        assert(type == NodeType.Symbol);
        return this.text = args;
    }

    @property
    string code()
    {
        assert(type == NodeType.Code);
        return text;
    }

    @property
    string code(string src)
    {
        assert(type == NodeType.Code);
        return this.text = src;
    }

    @property
    string predicate()
    {
        assert(type == NodeType.Group && codeType == CodeType.Predicate);
        return text;
    }

    @property
    string predicate(string src)
    {
        assert(type == NodeType.Group && codeType == CodeType.Predicate);
        return this.text = src;
    }
}

struct NodeRange(alias useNext)
{
private:
    Node node;

    Node next()
    {
        assert(node !is null);
        return useNext ? node.next : node.link;
    }

public:
    this(Node node)
    {
        this.node = node;
    }

    @property
    bool empty()
    {
        return node is null;
    }

    @property
    Node front()
    {
        return node;
    }

    void popFront()
    {
        if (node !is null)
            node = next;
    }

    @property
    NodeRange save()
    {
        return this;
    }
}

alias NodeNextRange = NodeRange!(true);
alias NodeLinkRange = NodeRange!(false);

static assert(isForwardRange!NodeNextRange);
static assert(isForwardRange!NodeLinkRange);

/**
 * Returns the left hand symbol (nonterminal) of the given node.
 *
 * Params:
 *      n = right hand side element (no terminal!)
 *
 * Returns:
 *      left hand side symbol
 */
Node symbolOf(Node n)
in(n !is null && n.type != NodeType.Terminal)
{
    if (n.type == NodeType.Nonterminal)
        return n;
repeat:
    while (n.back is null)
    {
        assert(n.next !is null);
        n = n.next;
    }
    while (n.type != NodeType.Nonterminal)
    {
        if (n.back is null)
            goto repeat;
        assert(n.back !is null);
        n = n.back;
    }
    return n;
}