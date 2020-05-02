// Written in the D programming language.
/**
 * Lexical analysis of the input.
 *
 * The lexer is implemented as an $(D InputRange).
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module lexer;

import diagnostics;
import std.ascii : isAlpha, isAlphaNum, isWhite;
import std.range;
import std.typecons : Tuple;

// A mapping from char to TokenKind for single-char tokens
private alias CASE = Tuple!(char, "key", TokenKind, "value");

enum TokenKind : uint
{
    Identifier,
    String,
    Argument,
    Code,
    Equal,
    Colon,
    Semi,
    Comma,
    Pipe,
    LeftParenthesis,
    RightParenthesis,
    RightParenthesisQuestion,
    RightParenthesisStar,
    RightParenthesisPlus,
    PercentPercent,
    KW_eoi,
    KW_if,
    KW_start,
    KW_token,
    _eoi,
    Unknown,
}

string displayName(TokenKind kind)
{
    final switch (kind)
    {
        case TokenKind.Identifier: return "identifier";
        case TokenKind.String: return "string";
        case TokenKind.Argument: return "argument";
        case TokenKind.Code: return "code";
        case TokenKind.Equal: return "=";
        case TokenKind.Colon: return ":";
        case TokenKind.Semi: return ";";
        case TokenKind.Comma: return ",";
        case TokenKind.Pipe: return "|";
        case TokenKind.LeftParenthesis: return "(";
        case TokenKind.RightParenthesis: return ")";
        case TokenKind.RightParenthesisQuestion: return ")?";
        case TokenKind.RightParenthesisStar: return ")*";
        case TokenKind.RightParenthesisPlus: return ")+";
        case TokenKind.PercentPercent: return "%%";
        case TokenKind.KW_eoi: return "%eoi";
        case TokenKind.KW_if: return "%sif";
        case TokenKind.KW_start: return "%start";
        case TokenKind.KW_token: return "%token";
        case TokenKind._eoi: return "end-of-input";
        case TokenKind.Unknown: return "unknown";
    }
}

struct Token
{
    TokenKind kind;
    size_t pos;
    size_t len;
    string val;
}

struct Lexer
{
private:
    immutable(char)[] data;
    size_t cur;
    Token tok;

public:
    this(immutable(char)[] str)
    {
        data = str;
        popFront();
    }

    @property
    bool empty()
    {
        return tok.kind == TokenKind._eoi;
    }

    @property
    Token front()
    {
        return tok;
    }

    void popFront()
    {
        tok = next();
    }

    Token moveFront()
    {
        return tok = next();
    }

    @property
    immutable(char)[] buffer()
    {
        return data;
    }

private:
    Token next()
    {
repeat:
        while (cur < data.length && isWhite(data[cur]))
        {
            ++cur;
        }
        if (cur >= data.length)
        {
            return Token(TokenKind._eoi, data.length, 0, "");
        }

        if (isAlpha(data[cur]))
            return identifier();
        else switch(data[cur])
        {
            case '"':
            case '\'':
                return string();
            case '<':
                return argument();
            case '{':
                if (cur+1 < data.length && data[cur+1] == '.')
                    return code();
                goto default;
            static foreach (c; [CASE('=', TokenKind.Equal), CASE(',', TokenKind.Comma),
                                CASE(':', TokenKind.Colon), CASE(';', TokenKind.Semi),
                                CASE('|', TokenKind.Pipe), CASE('(', TokenKind.LeftParenthesis)])
            {
                case c.key:
                    return Token(c.value, cur++, 1, [ c.key ]);
            }
            case ')':
                if (cur+1 < data.length)
                    switch (data[cur+1])
                    {
                        case '?':
                            cur += 2;
                            return Token(TokenKind.RightParenthesisQuestion, cur-2, 2, ")?");
                        case '*':
                            cur += 2;
                            return Token(TokenKind.RightParenthesisStar, cur-2, 2, ")*");
                        case '+':
                            cur += 2;
                            return Token(TokenKind.RightParenthesisPlus, cur-2, 2, ")+");
                        default:
                            break;
                    }
                return Token(TokenKind.RightParenthesis, cur++, 1, ")");
            case '%':
                if (cur+1 < data.length && data[cur+1] == '%')
                {
                    cur += 2;
                    return Token(TokenKind.PercentPercent, cur-2, 2, "%%");
                }
                else
                    return keyword();
            case '/':
                if (cur+1 < data.length)
                {
                    if (data[cur+1] == '*')
                        multilinecomment();
                    else if (data[cur+1] == '/')
                        singlelinecomment();
                    else
                        goto default;
                    goto repeat;
                }
                goto default;
            default:
                return Token(TokenKind.Unknown, cur++, 1, data[cur-1..cur]);
        }
    }

    Token identifier()
    {
        auto pos = cur++;
        while (cur < data.length && (isAlphaNum(data[cur]) || data[cur] == '_'))
            ++cur;
        auto val = data[pos..cur];
        return Token(TokenKind.Identifier, pos, cur-pos, val);
    }

    Token keyword()
    {
        auto pos = cur++;
        while (cur < data.length && isAlpha(data[cur]))
            ++cur;
        auto val = data[pos..cur];
        if (val == "%eoi")
            return Token(TokenKind.KW_eoi, pos, cur-pos, val);
        if (val == "%if")
            return Token(TokenKind.KW_if, pos, cur-pos, val);
        if (val == "%start")
            return Token(TokenKind.KW_start, pos, cur-pos, val);
        if (val == "%token")
            return Token(TokenKind.KW_token, pos, cur-pos, val);
        return Token(TokenKind.Unknown, pos, cur-pos, val);
    }

    Token argument()
    {
        auto pos = cur;
        auto dot = cur+1 < data.length && data[cur+1] == '.';
        if (dot)
            ++cur;
        do
        {
            ++cur;
            while (cur < data.length && data[cur] != '>')
                ++cur;
        }
        while (dot && cur < data.length && pos+1 < cur && data[cur-1] != '.');
        if (cur == data.length || data[cur] != '>')
            error(data, pos, cur-pos, "Unterminated argument");
        else
            ++cur;
        auto ignore = dot ? 2 : 1;
        return Token(TokenKind.Argument, pos, cur-pos, data[pos+ignore..cur-ignore]);
    }

    Token code()
    {
        import std.uni : isWhite;

        auto pos = cur;
        cur += 2;
        do
        {
            while (cur < data.length && data[cur] != '.')
                ++cur;
            ++cur;
        }
        while (cur < data.length && data[cur] != '}');
        if (cur >= data.length || data[cur] != '}')
            error(data, pos, cur-pos, "Unterminated code");
        else
            ++cur;
        auto begin = pos+2;
        auto end = cur-2;
        while (begin < end && isWhite(data[begin]))
            ++begin;
        while (begin < end && isWhite(data[end-1]))
            --end;
        return Token(TokenKind.Code, pos, cur-pos, data[begin..end]);
    }

    Token string()
    {
        immutable end = data[cur];
        auto pos = cur++;
        while (cur < data.length && data[cur] != end)
        {
            if (data[cur] == 0x0D || data[cur] == 0x0A)
            {
                // Run away line.
                error(data, pos, cur-pos, "Run away string");
                return Token(TokenKind.String, pos, cur-pos, data[pos..cur]);
            }
            ++cur;
        }
        cur++;
        return Token(TokenKind.String, pos, cur-pos, data[pos..cur]);
    }

    void multilinecomment()
    {
        auto pos = cur;
        cur += 2;
        do
        {
            while (cur < data.length && data[cur] != '*')
                ++cur;
            ++cur;
        }
        while (cur < data.length && data[cur] != '/');
        if (cur == data.length || data[cur] != '/')
            error(data, pos, cur-pos, "Unterminated comment");
        ++cur;
    }

    void singlelinecomment()
    {
        // Line endings: Unix \n, Mac \r, Dos/Windows \r\n
        while (cur < data.length && data[cur] != '\n' && data[cur] != '\r')
            ++cur;
        if (cur+1 < data.length && data[cur] == '\r' && data[cur+1] == '\n')
            ++cur;
        ++cur;
    }
}

static assert(isInputRange!Lexer);

unittest
{
    void checkToken(string str, TokenKind k, string v)
    {
        auto tok = Lexer(str).front;
        assert(tok.kind == k, "Expected: " ~ k.displayName ~ " Actual: " ~ tok.kind.displayName);
        assert(tok.val == v, "Expected: \"" ~ v ~ "\" Actual: \"" ~ tok.val ~ "\"");
    }

    checkToken("A", TokenKind.Identifier, "A");
    checkToken("\"str\"", TokenKind.String, "\"str\"");
    checkToken("'str'", TokenKind.String, "'str'");
    checkToken("<arg>", TokenKind.Argument, "arg");
    checkToken("<.arg.>", TokenKind.Argument, "arg");
    checkToken("{. code .}", TokenKind.Code, "code");
    checkToken("=", TokenKind.Equal, "=");
    checkToken(":", TokenKind.Colon, ":");
    checkToken(";", TokenKind.Semi, ";");
    checkToken(",", TokenKind.Comma, ",");
    checkToken("|", TokenKind.Pipe, "|");
    checkToken("(", TokenKind.LeftParenthesis, "(");
    checkToken(")", TokenKind.RightParenthesis, ")");
    checkToken(")?", TokenKind.RightParenthesisQuestion, ")?");
    checkToken(")*", TokenKind.RightParenthesisStar, ")*");
    checkToken(")+", TokenKind.RightParenthesisPlus, ")+");
    checkToken("%%", TokenKind.PercentPercent, "%%");
    checkToken("%eoi", TokenKind.KW_eoi, "%eoi");
    checkToken("%if", TokenKind.KW_if, "%if");
    checkToken("%start", TokenKind.KW_start, "%start");
    checkToken("%token", TokenKind.KW_token, "%token");
    checkToken("$", TokenKind.Unknown, "$");
    checkToken("", TokenKind._eoi, "");
}

unittest
{
    immutable str = r"
    A = B c d ;
    B = a | b ;
";

    auto lexer = Lexer(str);
    assert(!lexer.empty);
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "A");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Equal);
    assert(lexer.front.val == "=");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "B");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "c");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "d");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Semi);
    assert(lexer.front.val == ";");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "B");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Equal);
    assert(lexer.front.val == "=");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "a");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Pipe);
    assert(lexer.front.val == "|");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "b");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Semi);
    assert(lexer.front.val == ";");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind._eoi);
    assert(lexer.empty);

    // Do the same at compile time
    import std.array;
    enum toks = array(Lexer(str));
    assert(toks.length == 12);
    assert(toks[0].kind == TokenKind.Identifier);
    assert(toks[0].val == "A");
    assert(toks[1].kind == TokenKind.Equal);
    assert(toks[1].val == "=");
    assert(toks[2].kind == TokenKind.Identifier);
    assert(toks[2].val == "B");
    assert(toks[3].kind == TokenKind.Identifier);
    assert(toks[3].val == "c");
    assert(toks[4].kind == TokenKind.Identifier);
    assert(toks[4].val == "d");
    assert(toks[5].kind == TokenKind.Semi);
    assert(toks[5].val == ";");
    assert(toks[6].kind == TokenKind.Identifier);
    assert(toks[6].val == "B");
    assert(toks[7].kind == TokenKind.Equal);
    assert(toks[7].val == "=");
    assert(toks[8].kind == TokenKind.Identifier);
    assert(toks[8].val == "a");
    assert(toks[9].kind == TokenKind.Pipe);
    assert(toks[9].val == "|");
    assert(toks[10].kind == TokenKind.Identifier);
    assert(toks[10].val == "b");
    assert(toks[11].kind == TokenKind.Semi);
    assert(toks[11].val == ";");
}

unittest
{
    auto lexer = Lexer("A<bcd>");
    assert(!lexer.empty);
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "A");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Argument);
    assert(lexer.front.val == "bcd");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind._eoi);
    assert(lexer.empty);
}

unittest
{
    auto lexer = Lexer("A<.bcd.>");
    assert(!lexer.empty);
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "A");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Argument);
    assert(lexer.front.val == "bcd");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind._eoi);
    assert(lexer.empty);
}

unittest
{
    auto lexer = Lexer("A<..>");
    assert(!lexer.empty);
    assert(lexer.front.kind == TokenKind.Identifier);
    assert(lexer.front.val == "A");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind.Argument);
    assert(lexer.front.val == "");
    lexer.popFront;
    assert(lexer.front.kind == TokenKind._eoi);
    assert(lexer.empty);
}