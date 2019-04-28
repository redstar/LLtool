// Written in the D programming language.
/**
 * This module provides checks on certain aspects of the grammar.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module verify;

import diagnostics;
import grammar;
import std.algorithm : among, count, filter, setIntersection;

/**
 * Check if grammar contains only useful symbols.
 *
 * Params:
 *      buffer = content of grammar file (for error messages)
 *      grammar = grammar to check
 */
void checkGrammar(const(char)[] buffer, Grammar grammar)
{
	foreach (node; grammar.nonterminals)
    {
        if (!node.isReachable)
            error(buffer, node.pos, "Nonterminal %s is not reachable", node.name);
        if (!node.isProductive)
            error(buffer, node.pos, "Nonterminal %s is not productive", node.name);
    }
}

/**
 * Check if LL(1) conflicts exist in the grammar.
 *
 * Params:
 *      buffer = content of grammar file (for error messages)
 *      grammar = grammar to check
 */
void checkLL(const(char)[] buffer, Grammar grammar)
{
    static bool isResolver(Node n)
    {
        return n !is null && n.type == NodeType.Code && n.isResolver;
    }

    void checkGroup(Node node)
    {
        if (node.cardinality.among!(Cardinality.ZeroOrMore, Cardinality.ZeroOrOne))
        {
            if (node.link.derivesEpsilon || setIntersection(node.link.firstSet[], node.followSet[]).count > 0)
            {
                node.link.hasConflict = true;
                if (isResolver(node.link.inner))
                    node.link.inner.isValidResolver = true;
                else
                {
                    if (node.link.derivesEpsilon)
                        warning(buffer, node.pos, "LL conflict in %s: contents of [...] or {...} must not be deletable", symbolOf(node).name);
                    else
                        warning(buffer, node.pos, "LL conflict in %s: same start and sucessor of deletable element", symbolOf(node).name);
                }
            }
        }
    }

    void checkAlternative(Node node)
    {
        // Assume the best: no conflict
        TerminalSet set = new TerminalSet;
        size_t count = 0;
        foreach (n; NodeLinkRange(node.link))
        {
            set.insert(n.firstSet[]);
            count += n.firstSet.length;
            if (n.derivesEpsilon)
            {
                set.insert(node.followSet[]);
                count += n.followSet.length;
            }
            if (count != set.length)
                goto conflict;
        }
        return ;
conflict:
        TerminalSet a = new TerminalSet;
        TerminalSet b = new TerminalSet;
        foreach (ni; NodeLinkRange(node.link))
        {
            a.clear;
            a.insert(ni.firstSet[]);
            if (ni.derivesEpsilon)
                a.insert(ni.followSet[]);
            foreach (nj; NodeLinkRange(ni.link))
            {
                b.clear;
                b.insert(nj.firstSet[]);
                if (ni.derivesEpsilon)
                    b.insert(nj.followSet[]);
                if (setIntersection(a[], b[]).count > 0)
                {
                    ni.hasConflict = true;
                    if (isResolver(ni.inner))
                        ni.inner.isValidResolver = true;
                    else
                    {
                        warning(buffer, ni.pos, "LL conflict in %s: same start of several alternatives", symbolOf(ni).name);
                        warning(buffer, nj.pos, "LL conflict in %s: conflicting alternative", symbolOf(nj).name);
                    }
                }
            }
        }
    }

	foreach (node; filter!(n => n.type.among!(NodeType.Alternative, NodeType.Group))(grammar.nodes))
    {
        if (node.type == NodeType.Group)
            checkGroup(node);
        else if (node.type == NodeType.Alternative)
            checkAlternative(node);
    }

	foreach (node; filter!(n => n.type == NodeType.Code)(grammar.nodes))
    {
        if (node.isResolver && !node.isValidResolver)
            error(buffer, node.pos, "No LL conflict in %s: misplaced resolver", symbolOf(node).name);
    }
}