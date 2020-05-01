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
    static bool isCondition(Node n)
    {
        return n !is null && n.type == NodeType.Code && n.codeType == CodeType.Condition;
    }

    static void makePredicate(Node n)
    {
        assert(isCondition(n), "Node must be of type code with condition");
        n.codeType = CodeType.Predicate;
    }

    static void makeResolver(Node n)
    {
        assert(isCondition(n), "Node must be of type code with condition");
        n.codeType =CodeType.Resolver;
    }

    void checkGroup(Node node)
    {
        if (node.cardinality.among!(Cardinality.ZeroOrMore, Cardinality.ZeroOrOne))
        {
            if (node.link.derivesEpsilon || setIntersection(node.link.firstSet[], node.followSet[]).count > 0)
            {
                node.link.hasConflict = true;
                if (isCondition(node.link.inner))
                    makeResolver(node.link.inner);
                else
                {
                    if (node.link.derivesEpsilon)
                        warning(buffer, node.pos, "LL conflict in %s: contents of [...] or {...} must not be deletable", symbolOf(node).name);
                    else
                        warning(buffer, node.pos, "LL conflict in %s: same start and sucessor of deletable element", symbolOf(node).name);
                }
            }
            else if (isCondition(node.link.inner))
            {
                // The group is optional but there is no conflict.
                // Turn resolver into predicate.
                makePredicate(node.link.inner);
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
                if (nj.derivesEpsilon)
                    b.insert(nj.followSet[]);
                if (setIntersection(a[], b[]).count > 0)
                {
                    ni.hasConflict = true;
                    if (isCondition(ni.inner))
                        makeResolver(ni.inner);
                    else
                    {
                        warning(buffer, ni.pos, "LL conflict in %s: same start of several alternatives", symbolOf(ni).name);
                        warning(buffer, nj.pos, "LL conflict in %s: conflicting alternative", symbolOf(nj).name);
                    }
                }
            }
        }
    }

    void checkAlternativeForPredicate(Node node)
    {
        bool parentEps = (node.back !is null && node.back.derivesEpsilon);
        foreach (n; NodeLinkRange(node.link))
        {
            if ((parentEps || n.derivesEpsilon) && !n.hasConflict &&
                isCondition(n.inner)) {
                makePredicate(n.inner);
            }
        }
    }

	foreach (node; filter!(n => n.type.among!(NodeType.Alternative, NodeType.Group))(grammar.nodes))
    {
        if (node.type == NodeType.Group)
            checkGroup(node);
        else if (node.type == NodeType.Alternative)
        {
            checkAlternative(node);
            checkAlternativeForPredicate(node);
        }
    }

	foreach (node; filter!(n => n.type == NodeType.Code)(grammar.nodes))
    {
        if (isCondition(node))
            warning(buffer, node.pos, "No LL conflict in %s: misplaced resolver", symbolOf(node).name);
    }
}