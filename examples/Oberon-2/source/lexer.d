// Written in the D programming language.
/**
 * Example lexical analysis for Oberon-2.
 *
 * The lexer is implemented as an $(D ForwardRange).
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module lexer;

import diag;
import std.ascii : isAlpha, isAlphaNum, isDigit, isHexDigit, isWhite;
import std.range;
import std.typecons : Tuple;

private alias CASE = Tuple!(char, "key", TokenKind, "value");

enum TokenKind : uint
{
    Ident,
    String,
    Character,
    Real,
    Integer,
    Plus,
    Minus,
    Star,
    Slash,
    Tilde,
    Amp,
    Period,
    Comma,
    Semi,
    Pipe,
    LeftParenthesis,
    LeftSquare,
    LeftBrace,
    ColonEqual,
    Caret,
    Equal,
    Hash,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Ellipsis,
    Colon,
    RightParenthesis,
    RightSquare,
    RightBrace,
    KW_ARRAY,
    KW_BEGIN,
    KW_BY,
    KW_CASE,
    KW_CONST,
    KW_DIV,
    KW_DO,
    KW_ELSE,
    KW_ELSIF,
    KW_END,
    KW_EXIT,
    KW_FOR,
    KW_IF,
    KW_IMPORT,
    KW_IN,
    KW_IS,
    KW_LOOP,
    KW_MOD,
    KW_MODULE,
    KW_NIL,
    KW_OF,
    KW_OR,
    KW_POINTER,
    KW_PROCEDURE,
    KW_RECORD,
    KW_REPEAT,
    KW_RETURN,
    KW_THEN,
    KW_TO,
    KW_TYPE,
    KW_UNTIL,
    KW_VAR,
    KW_WHILE,
    KW_WITH,
    Eoi,
    Unknown,
}

/*
 +         :=         ARRAY    IMPORT     RETURN
  -         ^          BEGIN    IN         THEN
  *         =          BY       IS         TO
  /         #          CASE     LOOP       TYPE
  ~         <          CONST    MOD        UNTIL
  &         >          DIV      MODULE     VAR
  .         <=         DO       NIL        WHILE
  ,         >=         ELSE     OF         WITH
  ;         ..         ELSIF    OR
  |         :          END      POINTER
  (         )          EXIT     PROCEDURE
  [         ]          FOR      RECORD
  {         }          IF       REPEAT*/

string displayName(TokenKind kind)
{
    final switch (kind)
    {
        case TokenKind.Ident: return "ident";
        case TokenKind.String: return "string";
		case TokenKind.Character: return "character";
		case TokenKind.Real: return "real";
		case TokenKind.Integer: return "integer";
		case TokenKind.Plus: return "+";
		case TokenKind.Minus: return "-";
		case TokenKind.Star: return "*";
		case TokenKind.Slash: return "/";
		case TokenKind.Tilde: return "~";
		case TokenKind.Amp: return "&";
		case TokenKind.Period: return ".";
		case TokenKind.Comma: return ",";
		case TokenKind.Semi: return ";";
		case TokenKind.Pipe: return "|";
		case TokenKind.LeftParenthesis: return "(";
		case TokenKind.LeftSquare: return "[";
		case TokenKind.LeftBrace: return "{";
		case TokenKind.ColonEqual: return ":=";
		case TokenKind.Caret: return "^";
		case TokenKind.Equal: return "=";
		case TokenKind.Hash: return "#";
		case TokenKind.Less: return "<";
		case TokenKind.Greater: return ">";
		case TokenKind.LessEqual: return "<=";
		case TokenKind.GreaterEqual: return ">=";
		case TokenKind.Ellipsis: return "..";
		case TokenKind.Colon: return ":";
		case TokenKind.RightParenthesis: return ")";
		case TokenKind.RightSquare: return "]";
		case TokenKind.RightBrace: return "}";
		case TokenKind.KW_ARRAY: return "ARRAY";
		case TokenKind.KW_BEGIN: return "BEGIN";
		case TokenKind.KW_BY: return "BY";
		case TokenKind.KW_CASE: return "CASE";
		case TokenKind.KW_CONST: return "CONST";
		case TokenKind.KW_DIV: return "DIV";
		case TokenKind.KW_DO: return "DO";
		case TokenKind.KW_ELSE: return "ELSE";
		case TokenKind.KW_ELSIF: return "ELSIF";
		case TokenKind.KW_END: return "END";
		case TokenKind.KW_EXIT: return "EXIT";
		case TokenKind.KW_FOR: return "FOR";
		case TokenKind.KW_IF: return "IF";
		case TokenKind.KW_IMPORT: return "IMPORT";
		case TokenKind.KW_IN: return "IN";
		case TokenKind.KW_IS: return "IS";
		case TokenKind.KW_LOOP: return "";
		case TokenKind.KW_MOD: return "MOD";
		case TokenKind.KW_MODULE: return "MODULE";
		case TokenKind.KW_NIL: return "NIL";
		case TokenKind.KW_OF: return "OF";
		case TokenKind.KW_OR: return "OR";
		case TokenKind.KW_POINTER: return "POINTER";
		case TokenKind.KW_PROCEDURE: return "PROCEDURE";
		case TokenKind.KW_RECORD: return "RECORD";
		case TokenKind.KW_REPEAT: return "REPEAT";
		case TokenKind.KW_RETURN: return "RETURN";
    	case TokenKind.KW_THEN: return "THEN";
    	case TokenKind.KW_TO: return "TO";
    	case TokenKind.KW_TYPE: return "TYPE";
    	case TokenKind.KW_UNTIL: return "UNTIL";
    	case TokenKind.KW_VAR: return "VAR";
    	case TokenKind.KW_WHILE: return "WHILE";
    	case TokenKind.KW_WITH: return "WITH";
        case TokenKind.Eoi: return "end-of-input";
        case TokenKind.Unknown: return "unknown";
    }
}

private string keywordMap(E)() if (is(E == enum))
{
    import std.string : startsWith;

    string s;
    static foreach (m; __traits(allMembers, E))
    {
        static if (m.startsWith("KW_"))
        {
            s ~= "\"" ~ m[3..$] ~ "\": " ~ __traits(identifier, E) ~ "." ~ m ~ ",";
        }
    }
    return "[ " ~ s ~ " ]";
}

immutable TokenKind[string] keywords;
static this()
{
    import std.exception : assumeUnique;
    TokenKind[string] tmp = mixin(keywordMap!TokenKind);
    keywords = assumeUnique(tmp);
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
        return tok.kind == TokenKind.Eoi;
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
    Lexer save() const
    {
        return this;
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
            return Token(TokenKind.Eoi, data.length, 0, "");
        }

        if (isAlpha(data[cur]))
            return ident();
        else if (isDigit(data[cur]))
            return number();
        else switch(data[cur])
        {
            case '"':
            case '\'':
                return string();
            static foreach (c; [CASE('+', TokenKind.Plus), CASE('-', TokenKind.Minus),
                                CASE('*', TokenKind.Star), CASE('/', TokenKind.Slash),
                                CASE('~', TokenKind.Tilde), CASE('&', TokenKind.Amp),
                                CASE(',', TokenKind.Comma), CASE('|', TokenKind.Pipe),
                                CASE('^', TokenKind.Caret), CASE('=', TokenKind.Equal),
                                CASE(';', TokenKind.Semi),
                                CASE('#', TokenKind.Hash), CASE(')', TokenKind.RightParenthesis),
                                CASE('[', TokenKind.LeftSquare), CASE(']', TokenKind.RightSquare),
                                CASE('{', TokenKind.LeftBrace), CASE('}', TokenKind.RightBrace)])
            {
                case c.key:
                    return Token(c.value, cur++, 1, [ c.key ]);
            }
            case '.':
                if (cur+1 < data.length && data[cur+1] == '.')
                {
                    cur += 2;
                    return Token(TokenKind.Ellipsis, cur-2, 2, "..");
                }
                else
                    return Token(TokenKind.Period, cur++, 1, ".");
            case ':':
                if (cur+1 < data.length && data[cur+1] == '=')
                {
                    cur += 2;
                    return Token(TokenKind.ColonEqual, cur-2, 2, ":=");
                }
                else
                    return Token(TokenKind.Colon, cur++, 1, ":");
            case '(':
                if (cur+1 < data.length && data[cur+1] == '*')
                {
                    comment();
                    goto repeat;
                }
                else
                    return Token(TokenKind.LeftParenthesis, cur++, 1, "(");
            default:
                return Token(TokenKind.Unknown, cur++, 1, data[cur-1..cur]);
        }
    }

    Token ident()
    {
        auto pos = cur++;
        while (cur < data.length && isAlphaNum(data[cur]))
            ++cur;
        auto val = data[pos..cur];
        auto kind = val in keywords;
        return Token(kind !is null ? *kind : TokenKind.Ident, pos, cur-pos, val);
    }

    Token string()
    {
        immutable del = data[cur];
        auto pos = cur++;
        while (cur < data.length && data[cur] != del)
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

    Token number()
    {
        auto pos = cur++;
        bool hex = false;

        bool checkAndSetHex(char c)
        {
            auto b = c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F';
            hex |= b;
            return b | isDigit(c);
        }

        while (cur < data.length && checkAndSetHex(data[cur]))
            ++cur;

        if (cur < data.length && (data[cur] == 'H' || data[cur] == 'X'))
        {
            auto kind = data[cur] == 'H' ? TokenKind.Integer : TokenKind.Character;
            ++cur;
            return Token(kind, pos, cur-pos, data[pos..cur]);
        }
        else if (cur < data.length && data[cur] == '.')
        {
            ++cur;
            while (cur < data.length && isDigit(data[cur]))
                ++cur;
            // ScaleFactor
            if (cur < data.length && (data[cur] == 'E' || data[cur] == 'D'))
            {
                ++cur;
                if (cur < data.length && (data[cur] == '+' || data[cur] == '-'))
                    ++cur;
                while (cur < data.length && isDigit(data[cur]))
                    ++cur;
            }
            if (hex)
                error(data, pos, cur-pos, "Found real constant with hex digits");
            auto val = data[pos..cur];
            return Token(TokenKind.Real, pos, cur-pos, val);
        }
        else
        {
            if (hex)
                error(data, pos, cur-pos, "Found hex constant without trailing H");
            return Token(TokenKind.Integer, pos, cur-pos, data[pos..cur]);
        }
    }

    void comment()
    {
        auto pos = cur;
        cur += 2;
        do
        {
            while (cur < data.length && data[cur] != '*')
                ++cur;
            if (data[cur-1] == '(')
            {
                --cur;
                comment();
                continue;
            }
            else
                ++cur;
        }
        while (cur < data.length && data[cur] != ')');
        if (cur >= data.length || (cur < data.length && data[cur] != ')'))
            error(data, pos, 2, "Unterminated comment");
        ++cur;
    }
}

static assert(isForwardRange!Lexer);

unittest
{
    void checkToken(string str, TokenKind k, string v)
    {
        auto tok = Lexer(str).front;
        assert(tok.kind == k, "Expected: " ~ k.displayName ~ " Actual: " ~ tok.kind.displayName);
        assert(tok.val == v, "Expected: \"" ~ v ~ "\" Actual: \"" ~ tok.val ~ "\"");
    }

    checkToken("A", TokenKind.Ident, "A");
    checkToken("\"str\"", TokenKind.String, "\"str\"");
    checkToken("=", TokenKind.Equal, "=");
    checkToken(":", TokenKind.Colon, ":");
    checkToken(":=", TokenKind.ColonEqual, ":=");
    checkToken(".", TokenKind.Period, ".");
    checkToken(",", TokenKind.Comma, ",");
    checkToken("|", TokenKind.Pipe, "|");
    checkToken("(", TokenKind.LeftParenthesis, "(");
    checkToken(")", TokenKind.RightParenthesis, ")");
    checkToken("123", TokenKind.Integer, "123");
    checkToken("0DEADBEEFH", TokenKind.Integer, "0DEADBEEFH");
    checkToken("0CAFEBABEX", TokenKind.Character, "0CAFEBABEX");
    checkToken("3.14", TokenKind.Real, "3.14");
    checkToken("0.314E+01", TokenKind.Real, "0.314E+01");
    checkToken("MODULE", TokenKind.KW_MODULE, "MODULE");
    checkToken("WITH", TokenKind.KW_WITH, "WITH");
    checkToken("", TokenKind.Eoi, "");
}