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
    dstring[dstring] mapAdditions; // TODO Should come from input file
    const Fragment frag = getFragment(wantCPP, cppClassname, mapAdditions);
    if (wantCPP)
    {
        formattedWrite(sink, "#ifdef %s\n", frag.guardDeclaration);
	    foreach (node; grammar.nonterminals)
        {
            generateCPPRulePrototype(sink, 0, frag, node);
        }
        formattedWrite(sink, "#endif // of %s\n", frag.guardDeclaration);
        formattedWrite(sink, "#ifdef %s\n", frag.guardDefinition);
    }
    bool first = wantCPP;
	foreach (node; grammar.nonterminals)
    {
        if (first)
            first = false;
        else
            sink.put("\n");
        generateRule(sink, 0, frag, node);
    }
    if (wantCPP)
    {
        formattedWrite(sink, "#endif // of %s\n", frag.guardDefinition);
    }
}

private:

void generateRule(R)(R sink, size_t indent, const ref Fragment frag, Node node)
in(node.type == NodeType.Nonterminal)
{
    const ws = frag.whitespace(indent);
    formattedWrite(sink, "%svoid %s(%s) {\n", ws, frag.funcName!true(node.name), node.formalArgs);
    generateAlternativeOrSequence(sink, indent+1, frag, node.link);
    formattedWrite(sink, "%s}\n", ws);
}

void generateCPPRulePrototype(R)(R sink, size_t indent, const ref Fragment frag, Node node)
in(node.type == NodeType.Nonterminal)
{
    const ws = frag.whitespace(indent);
    formattedWrite(sink, "%svoid %s(%s);\n", ws, frag.funcName(node.name), node.formalArgs);
}

void generateGroup(R)(R sink, const size_t indent, const ref Fragment frag, Node node)
in(node.type == NodeType.Group)
{
    const ws = frag.whitespace(indent);
    final switch (node.cardinality)
    {
        case Cardinality.One:
            generateAlternativeOrSequence(sink, indent, frag, node.link);
            break;
        case Cardinality.OneOrMore:
            formattedWrite(sink, "%sdo {\n", ws);
            generateAlternativeOrSequence(sink, indent+1, frag, node.link);
            formattedWrite(sink, "%s} while (%s);\n", ws, condition!false(frag, node.link));
            break;
        case Cardinality.ZeroOrOne:
            formattedWrite(sink, "%sif (%s) {\n", ws, condition!false(frag, node.link));
            generateAlternativeOrSequence(sink, indent+1, frag, node.link, true);
            formattedWrite(sink, "%s}\n", ws);
            break;
        case Cardinality.ZeroOrMore:
            formattedWrite(sink, "%swhile (%s) {\n", ws, condition!false(frag, node.link));
            generateAlternativeOrSequence(sink, indent+1, frag, node.link, true);
            formattedWrite(sink, "%s}\n", ws);
            break;
    }
}

void generateAlternativeOrSequence(R)(R sink, size_t indent, const ref Fragment frag, Node node, bool startOfCondition = false)
in(node !is null && node.type.among!(NodeType.Sequence, NodeType.Alternative))
{
    if (node.type == NodeType.Alternative)
        generateAlternative(sink, indent, frag, node);
    else if (node.type == NodeType.Sequence)
        generateSequence(sink, indent, frag, node, startOfCondition);
}

void generateAlternative(R)(R sink, const size_t indent, const ref Fragment frag, Node node)
in(node.type == NodeType.Alternative)
{
    const ws = frag.whitespace(indent);
    for (auto n = node.link; n !is null; n = n.link)
    {
        formattedWrite(sink, "%s%s (%s) {\n", ws, node.link == n ? "if" : "else if", condition!true(frag, n));
        generateSequence(sink, indent+1, frag, n, true);
        formattedWrite(sink, "%s}\n", ws);
    }
}

void generateSequence(R)(R sink, const size_t indent, const ref Fragment frag, Node node, bool startOfCondition = false)
in(node.type == NodeType.Sequence)
{
    const ws = frag.whitespace(indent);
    bool genAdvance = false;
    foreach (n; NodeNextRange(node.inner))
    {
        if (genAdvance && n.type != NodeType.Code)
        {
            formattedWrite(sink, "%s%s();\n", ws, frag.advanceFunc);
            genAdvance = false;
        }
        final switch (n.type)
        {
            case NodeType.Terminal:
                assert(false, "Statement not reachable");
            case NodeType.Nonterminal:
                assert(false, "Statement not reachable");
                //generateRule(sink, indent, n);
                //return ;
            case NodeType.Group:
                generateGroup(sink, indent, frag, n);
                break;
            case NodeType.Alternative:
                generateAlternative(sink, indent, frag, n);
                break;
            case NodeType.Sequence:
                generateSequence(sink, indent, frag, n);
                break;
            case NodeType.Symbol:
                genAdvance = generateSymbol(sink, indent, frag, n, startOfCondition);
                break;
            case NodeType.Code:
                generateCode(sink, indent, frag, n);
                break;
        }
        if (startOfCondition)
            // Set startOfCondition to false if anything other than
            //  code  was emitted.
            startOfCondition = n.type == NodeType.Code;
    }
    if (genAdvance)
    {
        formattedWrite(sink, "%s%s();\n", ws, frag.advanceFunc);
    }
}

bool generateSymbol(R)(R sink, const size_t indent, const ref Fragment frag, Node node, bool startOfCondition = false)
in(node.type == NodeType.Symbol)
{
    assert(node.inner !is null);
    const ws = frag.whitespace(indent);
    if (node.inner.type == NodeType.Nonterminal)
    {
        formattedWrite(sink, "%s%s(%s);\n", ws, frag.funcName(node.name), node.actualArgs);
        return false;
    }
    else
    {
        const useExpect = node.next !is null && node.next.type == NodeType.Code;
        if (!startOfCondition)
            formattedWrite(sink, "%s%s(%s);\n", ws, useExpect ? frag.expectFunc : frag.consumeFunc, frag.tokenName(node.name));
        return useExpect || startOfCondition;
    }
}

void generateCode(R)(R sink, const size_t indent, const ref Fragment frag, Node node)  if (isOutputRange!(R, string))
in(node.type == NodeType.Code)
{
    // Only emit "normal" code. Resolvers and predicates are emitted elsewhere.
    if (node.codeType == CodeType.Normal)
    {
        const ws = frag.whitespace(indent);
        formattedWrite(sink, "%s%s\n", ws, node.code);
    }
}

string condition(bool isFiFo)(const ref Fragment frag, Node n)
{
    string cond = isFiFo ? fifocondition(frag, n) : condition(frag, n.firstSet);
    // If a there is a resolver or predicate attached, then add it.
    if (n.inner !is null && n.inner.type == NodeType.Code &&
        n.inner.codeType.among(CodeType.Predicate, CodeType.Resolver))
      cond ~= " && (" ~ n.inner.code ~ ")";
    return cond;
}

string fifocondition(const ref Fragment frag, Node n)
in(n !is null)
{
    if (n.derivesEpsilon)
    {
        TerminalSet set = new TerminalSet();
        set.insert(n.firstSet[]);
        set.insert(n.followSet[]);
        return condition(frag, set);
    }
    else
        return condition(frag, n.firstSet);
}

string condition(const ref Fragment frag, TerminalSet set)
in(set !is null)
{
    if (set.length == 0)
        return "false";
    if (set.length == 1)
    {
        return frag.tokenSingleCompare ~ frag.tokenName(set.front);
    }
    else
    {
        string c = frag.tokenSetMembership;
        bool first = true;
        foreach (t; set)
        {
            if (first) first = false;
            else
                c ~= ", ";
            c ~= frag.tokenName(t);
        }
        return c ~ ")";
    }
}

// A fragement holds part of the output strings.
struct Fragment
{
    enum KWMode { AsIs, AllUpper, AllLower };
    immutable int indentWidth;
    immutable string tokenNamePrefix;
    immutable string tokenSingleCompare;
    immutable string tokenSetMembership;
    immutable string advanceFunc;
    immutable string consumeFunc;
    immutable string expectFunc;
    immutable string classNamePrefix;
    immutable string funcNamePrefix;
    immutable string guardDeclaration;
    immutable string guardDefinition;
    immutable dstring[dstring] tokenMappings;
    immutable dstring keywordJoin;
    immutable KWMode kwMode;

    string whitespace(size_t width) const
    {
        import std.string : rightJustify;

        return rightJustify("", indentWidth * width, ' ');
    }

    string funcName(bool withPrefix = false)(string name) const
    {
        return (withPrefix ? classNamePrefix : "")
               ~ funcNamePrefix
               ~ firstToUpper(name);
    }

    string tokenName(string s) const
    {
        return tokenNamePrefix ~ mapToken(s);
    }

private:
    string mapToken(string s) const
    {
        import std.string : startsWith, endsWith;
        import std.uni : isAlpha, toUpper, toLower;
        import std.utf : toUTF8, toUTF32;

        if (s.startsWith("\""))
        {
            assert(s.endsWith("\""));
            s = s[1..$-1];

            bool first = true;
            bool keyword = false;
            dstring src = s.toUTF32;
            if (auto r = src in tokenMappings)
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
                if (auto r = [ ch ] in tokenMappings)
                {
                    if (keyword)
                        dst ~= keywordJoin;
                    dst ~= *r;
                }
                else
                    final switch (kwMode)
                    {
                        case KWMode.AsIs: dst ~= ch; break;
                        case KWMode.AllUpper: dst ~= ch.toUpper; break;
                        case KWMode.AllLower: dst ~= ch.toLower; break;
                    }
            }
            return dst.toUTF8;
        }
        else
            return firstToUpper(s);
    }

    static string firstToUpper(string s)
    {
        import std.string : toUpper;
        import std.conv : to;

        if (s.length == 0) return s;
        if (s.length == 1) return toUpper(s);
        return  to!string(toUpper(s[0])) ~ s[1..$];
    }
}

Fragment getFragment(bool cpp, string cppclass, dstring[dstring] add)
{
    import std.array: byPair, assocArray;
    import std.range: chain;

    if (cpp)
    {
        import std.string : toUpper;
        immutable string guard = cppclass.length > 0 ? cppclass.toUpper : "PARSER";
        immutable dstring[dstring] basemap = [  "..": "ellipsis",
                                                ".": "period",
                                                "|": "pipe",
                                                "=": "equal",
                                                "(": "l_paren",
                                                ")": "r_paren",
                                                "[": "l_square",
                                                "]": "r_square",
                                                "{": "l_brace",
                                                "}": "r_brace",
                                                ",": "comma",
                                                ";": "semi",
                                                ":": "colon",
                                                "?": "question",
                                                "!": "exclaim",
                                                "&": "amp",
                                                "~": "tilde",
                                                "+": "plus",
                                                "-": "minus",
                                                "*": "star",
                                                "/": "slash",
                                                "^": "caret",
                                                "#": "hash",
                                                "<": "less",
                                                ">": "greater",
                                                "%": "percent",
                                                "@": "at",
                                                "->": "arrow",
                                            ];
        immutable auto map = basemap.byPair.chain(add.byPair).assocArray;
        Fragment res = { indentWidth: 2,
                         tokenNamePrefix: "tok::",
                         tokenSingleCompare: "Tok.getKind() == ",
                         tokenSetMembership: "Tok.isOneOf(",
                         advanceFunc: "consumeToken",
                         consumeFunc: "expectAndConsumeToken",
                         expectFunc: "expectToken",
                         classNamePrefix: cppclass.length ?  cppclass ~ "::" : "",
                         funcNamePrefix: "parse",
                         guardDeclaration: guard ~ "_DECLARATION",
                         guardDefinition: guard ~ "_DEFINITION",
                         tokenMappings: map,
                         keywordJoin: "_",
                         kwMode: Fragment.KWMode.AsIs,
        };
        return res;
    }
    else
    {
        immutable dstring[dstring] basemap = [  "..": "Ellipsis",
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
        immutable auto map = basemap.byPair.chain(add.byPair).assocArray;
        Fragment res = { indentWidth: 4,
                         tokenNamePrefix: "TokenKind.",
                         tokenSingleCompare: "tok.kind == ",
                         tokenSetMembership: "tok.kind.among(",
                         advanceFunc: "advance",
                         consumeFunc: "consume",
                         expectFunc: "expect",
                         classNamePrefix: "",
                         funcNamePrefix: "parse",
                         guardDeclaration: "",
                         guardDefinition: "",
                         tokenMappings: map,
                         keywordJoin: "_",
                         kwMode: Fragment.KWMode.AsIs,
      };
       return res;
    }
}