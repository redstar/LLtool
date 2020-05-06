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
    const Fragment frag = getFragment(grammar, wantCPP, cppClassname);
    auto gen = CodeGen!(R)(sink, frag);
    if (wantCPP)
    {
        formattedWrite(sink, "#ifdef %s\n", frag.guardDeclaration);
	    foreach (node; grammar.nonterminals)
        {
            gen.reset();
            gen.rulePrototype(0, node);
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
        gen.reset();
        gen.rule(0, node);
    }
    if (wantCPP)
    {
        formattedWrite(sink, "#endif // of %s\n", frag.guardDefinition);
    }
}

private:

struct CodeGen(R) if (isOutputRange!(R, string))
{
private:
    R sink;
    const Fragment frag;

    bool needErrorHandling;

public:
    this(R sink, const Fragment frag) {
        this.sink = sink;
        this.frag = frag;
        for (size_t i = 0; i < wscache.length; ++i)
            wscache[i] = frag.whitespace(i);
    }

    void reset()
    {
        needErrorHandling = false;
    }

    void rulePrototype(size_t indent, Node node)
    in(node.type == NodeType.Nonterminal)
    {
        const ws = ws(indent);
        formattedWrite(sink, "%svoid %s(%s);\n", ws, frag.funcName(node.name), node.formalArgs);
    }

    void rule(size_t indent, Node node)
    {
        formattedWrite(sink, "%svoid %s(%s) {\n", ws(indent), frag.funcName!true(node.name), node.formalArgs);
        alternativeOrSequence(indent+1, node.link);
        formattedWrite(sink, "%sreturn;\n", ws(indent+1));

        if (needErrorHandling)
        {
            formattedWrite(sink, "%s%s:\n", ws(indent), frag.errorLabel);

            // Make sure the set contains eoi
            TerminalSet set;
            if (node.link.followSet.equalRange(frag.eoiToken).empty) {
                set = new TerminalSet();
                set.insert(node.link.followSet[]);
                set.insert(frag.eoiToken);
            }
            else
                set = node.link.followSet;

            formattedWrite(sink, "%swhile (%s)\n", ws(indent+1), condition!true(frag, set));
            formattedWrite(sink, "%s%s();\n", ws(indent+2), frag.advanceFunc);
        }
        formattedWrite(sink, "%s}\n", ws(indent));
    }

    void alternativeOrSequence(size_t indent, Node node, bool startOfCondition = false)
    in(node !is null && node.type.among!(NodeType.Sequence, NodeType.Alternative))
    {
        if (node.type == NodeType.Alternative)
            alternative(indent, node);
        else if (node.type == NodeType.Sequence)
            sequence(indent, node, startOfCondition);
    }

    void group(const size_t indent, Node node)
    in(node.type == NodeType.Group)
    {
        final switch (node.cardinality)
        {
            case Cardinality.One:
                alternativeOrSequence(indent, node.link);
                break;
            case Cardinality.OneOrMore:
                formattedWrite(sink, "%sdo {\n", ws(indent));
                alternativeOrSequence(indent+1, node.link);
                formattedWrite(sink, "%s} while (%s);\n", ws(indent), condition!false(frag, node.link));
                break;
            case Cardinality.ZeroOrOne:
                formattedWrite(sink, "%sif (%s) {\n", ws(indent), condition!false(frag, node.link));
                alternativeOrSequence(indent+1, node.link, true);
                formattedWrite(sink, "%s}\n", ws(indent));
                break;
            case Cardinality.ZeroOrMore:
                formattedWrite(sink, "%swhile (%s) {\n", ws(indent), condition!false(frag, node.link));
                alternativeOrSequence(indent+1, node.link, true);
                formattedWrite(sink, "%s}\n", ws(indent));
                break;
        }
    }

    void alternative(const size_t indent, Node node)
    in(node.type == NodeType.Alternative)
    {
        bool isFirstChildOfOptGroup(Node node)
        {
            Node n = node;
            Node p = node.parent;
            while (p !is null)
            {
                if (p.type == NodeType.Group &&
                    p.cardinality.among(Cardinality.ZeroOrOne, Cardinality.ZeroOrMore))
                    return true;
                if ((p.type == NodeType.Group &&
                    p.cardinality == Cardinality.One) ||
                    (p.type == NodeType.Sequence && p.inner == n))
                {
                    n = p;
                    p = p.parent;
                    continue;
                }
                break;
            }
            return false;
        }

        const ws2 = ws(indent+2);
        const ws1 = ws(indent+1);
        const ws = ws(indent);
        bool useSwitch = true; // useSwitch == true <=> max. 2 tokens, no predicate
        // If the alternative is inside an optional group, e.g. ( A | B )?, then
        // the condition of the group covers all tokens used in the alternative.
        // Therefore an error check is not required.
        bool needError = !isFirstChildOfOptGroup(node);
        for (auto n = node.link; n !is null; n = n.link)
        {
            useSwitch &= singleCondition(n);
            needError &= !n.derivesEpsilon;
        }
        if (useSwitch)
        {
            formattedWrite(sink, "%sswitch (%s) {\n", ws, frag.tokenSingleCompare);
            for (auto n = node.link; n !is null; n = n.link)
            {
                string token = n.firstSet.empty ? n.followSet.front : n.firstSet.front;
                formattedWrite(sink, "%scase %s:\n", ws1, frag.tokenName(token));
                sequence(indent+2, n, true);
                formattedWrite(sink, "%sbreak;\n", ws2);
            }
            if (needError)
            {
                formattedWrite(sink, "%sdefault:\n", ws1);
                formattedWrite(sink, "%s/*ERROR*/\n", ws2);
                formattedWrite(sink, "%sgoto %s;\n", ws2, frag.errorLabel);
                needErrorHandling = true;
                formattedWrite(sink, "%sbreak;\n", ws2);
            }
            formattedWrite(sink, "%s}\n", ws);
        }
        else
        {
            for (auto n = node.link; n !is null; n = n.link)
            {
                formattedWrite(sink, "%s%s (%s) {\n", ws, node.link == n ? "if" : "else if", condition!true(frag, n));
                sequence(indent+1, n, true);
                formattedWrite(sink, "%s}\n", ws);
            }
            if (needError)
            {
                formattedWrite(sink, "%selse {\n", ws);
                formattedWrite(sink, "%s/*ERROR*/\n", ws1);
                formattedWrite(sink, "%sgoto %s;\n", ws1, frag.errorLabel);
                needErrorHandling = true;
                formattedWrite(sink, "%s}\n", ws);
            }
        }
    }

    void sequence(const size_t indent, Node node, bool startOfCondition = false)
    in(node.type == NodeType.Sequence)
    {
        const ws = ws(indent);
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
                case NodeType.Group:
                    group(indent, n);
                    break;
                case NodeType.Alternative:
                    alternative(indent, n);
                    break;
                case NodeType.Sequence:
                    sequence(indent, n);
                    break;
                case NodeType.Symbol:
                    genAdvance = symbol(indent, n, startOfCondition);
                    break;
                case NodeType.Code:
                    code(indent, n);
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

    bool symbol(const size_t indent, Node node, bool startOfCondition = false)
    in(node.type == NodeType.Symbol)
    {
        assert(node.inner !is null);
        const ws = ws(indent);
        if (node.inner.type == NodeType.Nonterminal)
        {
            formattedWrite(sink, "%s%s(%s);\n", ws, frag.funcName(node.name), node.actualArgs);
            return false;
        }
        else
        {
            const useExpect = node.next !is null && node.next.type == NodeType.Code;
            if (!startOfCondition) {
                string func = useExpect ? frag.expectFunc : frag.consumeFunc;
                formattedWrite(sink, "%sif (%s(%s))\n", ws, func, frag.tokenName(node.name));
                formattedWrite(sink, "%s%sgoto %s;\n", ws, frag.whitespace(1), frag.errorLabel);
                needErrorHandling = true;
            }
            return useExpect || startOfCondition;
        }
    }

    void code(const size_t indent, Node node)
    in(node.type == NodeType.Code)
    {
        // Only emit "normal" code. Resolvers and predicates are emitted elsewhere.
        if (node.codeType == CodeType.Normal)
        {
            formattedWrite(sink, "%s%s\n", ws(indent), node.code);
        }
    }

private:
    string[5] wscache;

    string ws(size_t indent){
        if (indent < wscache.length)
            return wscache[indent];
        return frag.whitespace(indent);
    }
}

bool singleCondition(Node n)
{
    if (n.inner !is null && n.inner.type == NodeType.Code &&
        n.inner.codeType.among(CodeType.Predicate, CodeType.Resolver))
        return false;
    if (n.firstSet.length == 1)
        return true;
    if (n.firstSet.length == 0 && n.followSet.length == 1)
        return true;
    return false;
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
    return condition(frag, n.firstSet.empty ? n.followSet : n.firstSet);
}

string condition(bool negate = false)(const ref Fragment frag, TerminalSet set)
in(set !is null)
{
    if (set.length == 0)
        return "false";
    if (set.length == 1)
    {
        return frag.tokenSingleCompare
               ~ (negate ? " != " : " == " )
               ~ frag.tokenName(set.front);
    }
    else
    {
        string c = (negate ? "!" : "") ~ frag.tokenSetMembership;
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
    immutable string eoiToken;
    immutable string mappedEoiToken;
    immutable string advanceFunc;
    immutable string consumeFunc;
    immutable string expectFunc;
    immutable string classNamePrefix;
    immutable string funcNamePrefix;
    immutable string errorLabel;
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
        else if (s == eoiToken)
            return mappedEoiToken;
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

Fragment getFragment(Grammar grammar, bool cpp, string cppclass)
{
    import std.array: byPair, assocArray;
    import std.range: chain;

    immutable string eoi = grammar.eoiTerminal.name;
    immutable string mappedEoi = grammar.eoiTerminal.externalName;
    dstring[dstring] externMap = findExternalNames(grammar);
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
        immutable auto map = basemap.byPair.chain(externMap.byPair).assocArray;
        Fragment res = { indentWidth: 2,
                         tokenNamePrefix: "tok::",
                         tokenSingleCompare: "Tok.getKind()",
                         tokenSetMembership: "Tok.isOneOf(",
                         eoiToken: eoi,
                         mappedEoiToken: mappedEoi.length ? mappedEoi : "eoi",
                         advanceFunc: "advance",
                         consumeFunc: "consume",
                         expectFunc: "expect",
                         classNamePrefix: cppclass.length ?  cppclass ~ "::" : "",
                         funcNamePrefix: "parse",
                         errorLabel: "_error",
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
        immutable auto map = basemap.byPair.chain(externMap.byPair).assocArray;
        Fragment res = { indentWidth: 4,
                         tokenNamePrefix: "TokenKind.",
                         tokenSingleCompare: "tok.kind",
                         tokenSetMembership: "tok.kind.among(",
                         eoiToken: eoi,
                         mappedEoiToken: mappedEoi.length ? mappedEoi : "Eoi",
                         advanceFunc: "advance",
                         consumeFunc: "consume",
                         expectFunc: "expect",
                         classNamePrefix: "",
                         funcNamePrefix: "parse",
                         errorLabel: "_error",
                         guardDeclaration: "",
                         guardDefinition: "",
                         tokenMappings: map,
                         keywordJoin: "_",
                         kwMode: Fragment.KWMode.AsIs,
      };
       return res;
    }
}

dstring[dstring] findExternalNames(Grammar grammar)
{
    import std.algorithm : filter;
    import std.utf : toUTF32;

    const Node eoi = grammar.eoiTerminal;
    dstring[dstring] map;
	foreach (node; filter!(n => n.type == NodeType.Terminal)(grammar.nodes))
    {
        if (node.externalName.length && node != eoi)
        {
            // Remove " from string.
            string name = node.name[1..$-1];
            map[name.toUTF32] = node.externalName.toUTF32;
        }
    }
    return map;
}