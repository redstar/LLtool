// Written in the D programming language.
/**
 * Example parser for Oberon-2.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module parser;

import diag;
import lexer;
import std.algorithm : among;
import std.container.rbtree;

struct Parser
{
private:
    Lexer lexer;
    Token tok;

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

    /* Simple symbol table */
    RedBlackTree!(string, "a < b", false) modules;

    void addImport(size_t modulePos, string moduleName, size_t aliasPos, string aliasName)
    {
        if (aliasName.length)
        {
            if (aliasName in modules)
                error(lexer.buffer, aliasPos, aliasName.length, "Module %s (alias of %s) already imported", aliasName, moduleName);
            else
                modules.insert(aliasName);
        }
        else
        {
            if (moduleName in modules)
                error(lexer.buffer, modulePos, moduleName.length, "Module %s already imported", moduleName);
            else
                modules.insert(moduleName);
        }
    }

    /* The next functions are the resolvers. */
    bool isModule()
    {
        // Resolve conflict with symbol table lookup
        return tok.val in modules;
    }

    bool isProcCall()
    {
        // If current look ahead symbol is ".", "[" or "^" then it is not a proc call
        if (tok.kind.among!(TokenKind.Period, TokenKind.LeftSquare, TokenKind.Caret))
            return false;
        // Otherwise a "(" follows. This can be a guard or a proc call.
        // Here is more semantic analysis required. Just assume it is a proc
        // call for now.
        return true;
    }

    bool isAlias()
    {
        // LL(1) conflict can be resolved with a look ahead of two
        return lexer.save.moveFront.kind == TokenKind.ColonEqual;
    }

    bool isProcDecl()
    {
        // LL(1) conflict can be resolved with a look ahead of two
        return lexer.save.moveFront.kind != TokenKind.Caret;
    }

public:
    this(Lexer lexer)
    {
        this.lexer = lexer;
        tok = lexer.front;
        modules = new RedBlackTree!(string, "a < b", false);
    }

    void parse()
    {
        parseModule();
    }

private:
    mixin(import("oberon2.mixin"));
}