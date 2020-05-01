// Written in the D programming language.
/**
 * This module provides the code generator for D.
 *
 * Copyright:  (C) 2019, 2020 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module generator;

import grammar;
import std.algorithm : among;
import std.format : formattedWrite;
import std.range : isOutputRange;

void generate(R)(R sink, Grammar grammar, bool wantCPP, string cppClassname) if (isOutputRange!(R, string))
{
    import std.string : toUpper;
    string ppName = cppClassname.length > 0 ? cppClassname.toUpper : "PARSER";
    prefix = wantCPP && cppClassname.length > 0 ? cppClassname ~ "::" : "";
    generateCPP = wantCPP;
    if (wantCPP)
    {
        formattedWrite(sink, "#ifdef %s_DECLARATION\n", ppName);
	    foreach (node; grammar.nonterminals)
        {
            generateCPPRulePrototype(sink, 0, node);
        }
        formattedWrite(sink, "#endif // of %s_DECLARATION\n", ppName);
        formattedWrite(sink, "#ifdef %s_DEFINITION\n", ppName);
    }
    bool first = wantCPP;
	foreach (node; grammar.nonterminals)
    {
        if (first)
            first = false;
        else
            sink.put("\n");
        generateRule(sink, 0, node);
    }
    if (wantCPP)
    {
        formattedWrite(sink, "#endif // of %s_DEFINITION\n", ppName);
    }
}

private:
bool generateCPP; // TODO Remove HACK.
string prefix; // TODO Remove HACK.

void generateRule(R)(R sink, size_t indent, Node node)
in(node.type == NodeType.Nonterminal)
{
    const ws = whitespace(indent);
    formattedWrite(sink, "%svoid %sparse%s(%s) {\n", ws, prefix, firstToUpper(node.name), node.formalArgs);
    generateAlternativeOrSequence(sink, indent+1, node.link);
    formattedWrite(sink, "%s}\n", ws);
}

void generateCPPRulePrototype(R)(R sink, size_t indent, Node node)
in(node.type == NodeType.Nonterminal)
{
    const ws = whitespace(indent);
    formattedWrite(sink, "%svoid parse%s(%s);\n", ws, firstToUpper(node.name), node.formalArgs);
}

void generateGroup(R)(R sink, const size_t indent, Node node)
in(node.type == NodeType.Group)
{
    const ws = whitespace(indent);
    final switch (node.cardinality)
    {
        case Cardinality.One:
            generateAlternativeOrSequence(sink, indent, node.link);
            break;
        case Cardinality.OneOrMore:
            formattedWrite(sink, "%sdo {\n", ws);
            generateAlternativeOrSequence(sink, indent+1, node.link);
            formattedWrite(sink, "%s} while (%s);\n", ws, condition!false(node.link));
            break;
        case Cardinality.ZeroOrOne:
            formattedWrite(sink, "%sif (%s) {\n", ws, condition!false(node.link));
            generateAlternativeOrSequence(sink, indent+1, node.link, true);
            formattedWrite(sink, "%s}\n", ws);
            break;
        case Cardinality.ZeroOrMore:
            formattedWrite(sink, "%swhile (%s) {\n", ws, condition!false(node.link));
            generateAlternativeOrSequence(sink, indent+1, node.link, true);
            formattedWrite(sink, "%s}\n", ws);
            break;
    }
}

void generateAlternativeOrSequence(R)(R sink, size_t indent, Node node, bool startOfCondition = false)
in(node !is null && node.type.among!(NodeType.Sequence, NodeType.Alternative))
{
    if (node.type == NodeType.Alternative)
        generateAlternative(sink, indent, node);
    else if (node.type == NodeType.Sequence)
        generateSequence(sink, indent, node, startOfCondition);
}

void generateAlternative(R)(R sink, const size_t indent, Node node)
in(node.type == NodeType.Alternative)
{
    const ws = whitespace(indent);
    for (auto n = node.link; n !is null; n = n.link)
    {
        formattedWrite(sink, "%s%s (%s) {\n", ws, node.link == n ? "if" : "else if", condition!true(n));
        generateSequence(sink, indent+1, n, true);
        formattedWrite(sink, "%s}\n", ws);
    }
}

void generateSequence(R)(R sink, const size_t indent, Node node, bool startOfCondition = false)
in(node.type == NodeType.Sequence)
{
    const ws = whitespace(indent);
    bool genAdvance = false;
    foreach (n; NodeNextRange(node.inner))
    {
        if (genAdvance && n.type != NodeType.Code)
        {
            formattedWrite(sink, "%sadvance();\n", ws);
            genAdvance = false;
        }
        final switch (n.type)
        {
            case NodeType.Terminal:
                assert(false, "Statement not reachable");
            case NodeType.Nonterminal:
                generateRule(sink, indent, n);
                return ;
            case NodeType.Group:
                generateGroup(sink, indent, n);
                break;
            case NodeType.Alternative:
                generateAlternative(sink, indent, n);
                break;
            case NodeType.Sequence:
                generateSequence(sink, indent, n);
                break;
            case NodeType.Symbol:
                genAdvance = generateSymbol(sink, indent, n, startOfCondition);
                break;
            case NodeType.Code:
                generateCode(sink, indent, n);
                break;
        }
        if (startOfCondition)
            // Set startOfCondition to false if anything other than
            //  code  was emitted.
            startOfCondition = n.type == NodeType.Code;
    }
    if (genAdvance)
    {
        formattedWrite(sink, "%sadvance();\n", ws);
    }
}

bool generateSymbol(R)(R sink, const size_t indent, Node node, bool startOfCondition = false)
in(node.type == NodeType.Symbol)
{
    assert(node.inner !is null);
    const ws = whitespace(indent);
    if (node.inner.type == NodeType.Nonterminal)
    {
        formattedWrite(sink, "%sparse%s(%s);\n", ws, firstToUpper(node.name), node.actualArgs);
        return false;
    }
    else
    {
        const useExpect = node.next !is null && node.next.type == NodeType.Code;
        if (!startOfCondition)
            formattedWrite(sink, "%s%s(%s);\n", ws, useExpect ? "expect" : "consume", tokenName(node.name));
        return useExpect || startOfCondition;
    }
}

void generateCode(R)(R sink, const size_t indent, Node node)  if (isOutputRange!(R, string))
in(node.type == NodeType.Code)
{
    if (!(node.codeType.among(CodeType.Resolver, CodeType.Predicate)))
    {
        const ws = whitespace(indent);
        formattedWrite(sink, "%s%s\n", ws, node.code);
    }
}

string condition(bool isFiFo)(Node n)
{
    if (n.hasConflict && n.inner !is null && n.inner.type == NodeType.Code &&
        n.inner.codeType == CodeType.Resolver)
        return n.inner.code;
    string cond = isFiFo ? fifocondition(n) : condition(n.firstSet);
    if (n.inner !is null && n.inner.type == NodeType.Code && n.inner.codeType == CodeType.Predicate)
      cond ~= " && (" ~ n.inner.code ~ ")";
    return cond;
}

string fifocondition(Node n)
in(n !is null)
{
    if (n.derivesEpsilon)
    {
        TerminalSet set = new TerminalSet();
        set.insert(n.firstSet[]);
        set.insert(n.followSet[]);
        return condition(set);
    }
    else
        return condition(n.firstSet);
}

string condition(TerminalSet set)
in(set !is null)
{
    if (set.length == 0)
        return "false";
    if (set.length == 1)
    {
        if (generateCPP)
            return "Tok.getKind() == " ~ tokenName(set.front);
        else
            return "tok.kind == " ~ tokenName(set.front);
    }
    else
    {
        string c = generateCPP ? "Tok.isOneOf(" : "tok.kind.among(";
        bool first = true;
        foreach (t; set)
        {
            if (first) first = false;
            else
                c ~= ", ";
            c ~= tokenName(t);
        }
        return c ~ ")";
    }
}

string tokenName(string s)
{
    if (generateCPP)
        return "tok::" ~ mapToken(s);
    else
        return "TokenKind." ~ mapToken(s);
}

string mapToken(string s)
{
    import std.string : startsWith, endsWith;
    import std.uni : isAlpha;
    import std.utf;

    dstring[dstring] map = [
        "..": "Ellipsis",
        ".": "Period",
        "|": "Pipe",
        "=": "Equal",
        "(": "LeftParenthesis",
        ")": "RightParenthesis",
        "[": "LeftSquare",
        "]": "RightSquare",
        "{": "LeftBrace",
        "}": "RightBrace",
        ",": "Comma",
        ";": "Semi",
        ":": "Colon",
        "?": "Question",
        "!": "Exclaim",
        "&": "Amp",
        "~": "Tilde",
        "+": "Plus",
        "-": "Minus",
        "*": "Star",
        "/": "Slash",
        "^": "Caret",
        "#": "Hash",
        "<": "Less",
        ">": "Greater",
        "%": "Percent",
        "@": "At",
    ];
    if (s.startsWith("\""))
    {
        assert(s.endsWith("\""));
        s = s[1..$-1];

        bool first = true;
        bool keyword = false;
        dstring src = s.toUTF32;
        if (auto r = src in map)
            return (*r).toUTF8;
        dstring dst = "";
        foreach (ch; src)
        {
            if (first && isAlpha(ch))
            {
                dst = "KW_"d;
                keyword = true;
            }
            first = false;
            if (auto r = [ ch ] in map)
            {
                if (keyword)
                    dst ~= "_";
                dst ~= *r;
            }
            else
                dst ~= ch;
        }
        return dst.toUTF8;
    }
    else
        return firstToUpper(s);
}

string whitespace(size_t width)
{
    import std.string : rightJustify;

    return rightJustify("", 4 * width, ' ');
}

string firstToUpper(string s)
{
    import std.string : toUpper;
    import std.conv : to;

    if (s.length == 0) return s;
    if (s.length == 1) return toUpper(s);
    return  to!string(toUpper(s[0])) ~ s[1..$];
}