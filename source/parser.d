// Written in the D programming language.
/**
 * Parser for the LLtool language.
 *
 * The parser is implemented as a recursive descent parser.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module parser;

import diagnostics;
import grammar;
import lexer; // TODO remove
import std.algorithm.comparison : castSwitch;
import std.range : isInputRange, ElementType;
import std.traits;

enum bool isTokenType(T) =
    is(ReturnType!((T t) => t.kind) == enum);

struct Parser(TLexer) if (isInputRange!TLexer && isTokenType!(ElementType!TLexer))
{
private:
    alias Token = ElementType!TLexer;
    alias TokenKind = typeof(Token.kind);

    TLexer lexer;
    Token tok;
    GrammarBuilder builder;

    void advance()
    {
        tok = lexer.moveFront;
    }

    bool expect(TokenKind kind)
    {
        if (tok.kind != kind)
        {
            error(lexer.buffer, tok.pos, tok.len,
                    "Expected %s but found %s", kind.displayName, tok.kind.displayName);
            return false;
        }
        return true;
    }

    bool consume(TokenKind kind)
    {
        if (expect(kind))
        {
            advance();
            return true;
        }
        else
        {
            return false;
        }
    }

public:
    this(TLexer lexer)
    {
        this.lexer = lexer;
        this.builder.init(lexer.buffer);
    }

    Grammar parse()
    {
        tok = lexer.front;
        parseLltool();
        Grammar grammar;
        if (!hasErrors)
            grammar = builder.finalize();
        return grammar;
    }

private:
    mixin(import("lltool.mixin"));
}

unittest
{
    import lexer;

    immutable str = r"
      A = B x .
      B = C .
      C = y .
    ";

	auto parser = Parser!Lexer(Lexer(str));

    //enum grammar = Parser!Lexer(Lexer(str)).parse;
}