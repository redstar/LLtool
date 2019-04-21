// Written in the D programming language.
/**
 * This module provides error handling functions.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module diag;

import std.stdio;
import std.string;
import std.traits : isSomeChar;

void error(Char, A...)(const(char)[] buffer, size_t pos, in Char[] msg, A args) if (isSomeChar!Char)
{
    error(buffer, pos, 1, format(msg, args));
}

void error(Char, A...)(const(char)[] buffer, size_t pos, size_t len, in Char[] msg, A args) if (isSomeChar!Char)
{
    error(buffer, pos, len, format(msg, args));
}

void error(const(char)[] buffer, size_t pos, size_t len, string msg)
{
    auto info = computeLine(buffer, pos);
    stderr.writefln("%s", buffer[info.start..info.start+info.len]);
    stderr.writefln("%*s%s", info.ofs, " ", leftJustify("^", len, '^'));
    stderr.writefln("Error: %d,%d: %s", info.line, info.ofs, msg);
}


private:
struct LineInfo
{
    size_t start;  // Index to start of line containing 'pos'
    size_t len;    // Length of line
    size_t ofs;    // Offset in line
    size_t line;   // Line numer
}

LineInfo computeLine(const(char)[] buffer, size_t pos)
{
    LineInfo li = { line: 1 };
    for (size_t i = 0; i < pos; i++)
    {
        // Line endings: Unix \n, Mac \r, Dos/Windows \r\n
        if (buffer[i] == '\r' || buffer[i] == '\n')
        {
            if (buffer[i] == '\r' && buffer[i+1] == '\n')
                ++i;
            li.start = i+1;
            li.line++;
        }
    }
    li.ofs = pos - li.start;
    li.len = buffer.length - li.start;
    for (size_t i = pos; i < buffer.length; i++)
    {
        if (buffer[i] == '\r' || buffer[i] == '\n')
        {
            li.len = i - li.start;
            break;
        }
    }
    return li;
}

unittest
{
    import std.conv : to;

    string input1 = "S = A 'a' 'b' .\nA = [ 'a' ] .";
    auto li = computeLine(input1, 21);
    assert(li.start == 16, "Exp: 16 Act: " ~to!string(li.start));
    assert(li.len == 13, "Exp: 13 Act: " ~to!string(li.len));
    assert(li.ofs == 5, "Exp: 5 Act: " ~to!string(li.ofs));
    assert(li.line == 2, "Exp: 2 Act: " ~to!string(li.line));

    string input2 = " S = E | E 'a' .\n E = [ 'b' ] .";
    li = computeLine(input2, 5);
    assert(li.start == 0, "Exp: 0 Act: " ~to!string(li.start));
    assert(li.len == 16, "Exp: 16 Act: " ~to!string(li.len));
    assert(li.ofs == 5, "Exp: 5 Act: " ~to!string(li.ofs));
    assert(li.line == 1, "Exp: 1 Act: " ~to!string(li.line));
}