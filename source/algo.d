// Written in the D programming language.
/**
 * This module provides algorithms for grammar flow analysis.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module algo;

import grammar;
import std.algorithm : among, filter, min;
import std.bitmanip;
import std.container.array;
import std.range;
version(unittest) import lexer, parser;
version(unittest) import std.conv : to;

/**
 * Marks all symbols reachable from the start symbol.
 *
 * Params:
 * 		grammar = grammar for which the reachability of the symbols is
 *                computed
 */
void calculateReachable(Grammar grammar)
{
	static void mark(Node node)
	{
		final switch (node.type)
		{
			case NodeType.Terminal:
				node.isReachable = true;
				break;
			case NodeType.Nonterminal:
				node.isReachable = true;
				mark(node.link);
				break;
			case NodeType.Group:
			case NodeType.Alternative:
				node.isReachable = true;
				foreach (n; NodeLinkRange(node.link))
					mark(n);
				break;
			case NodeType.Sequence:
				node.isReachable = true;
	    		for (auto n = node.succInner; n !is null; n = n.succ)
					mark(n);
				break;
			case NodeType.Symbol:
				node.isReachable = true;
				if (!node.inner.isReachable)
					mark(node.inner);
				break;
			case NodeType.Code:
				assert(false, "Statement not reachable");
		}
	}

	mark(grammar.syntheticStartSymbol);
}

unittest
{
    // Wilhelm, Maurer; p. 298
	// Nonterminals U, V are not reachable.
    immutable content = `
	%token a, b, c, d
	%%
	S : Y ;
	Y : Y Z | Y a | b ;
	U : V ;
	X : c ;
	V : V d | d ;
    Z : Z X ;
`;
	auto grammar = Parser!Lexer(Lexer(content)).parse;
	calculateReachable(grammar);
	foreach (nt; grammar.nonterminals)
		assert(nt.isReachable == (nt.name != "U" && nt.name != "V"),
		    "Nonterminal: `" ~ nt.name ~ "`" ~
			" Exp: " ~ to!string(nt.name != "U" && nt.name != "V") ~
			" Act: " ~ to!string(nt.isReachable));
}

/**
 * Performs a fixed point computation on boolean values.
 *
 * Params:
 *		prop = name of property
 *		terminalVal = value for terminals
 *		groupval = function / delegate which returns additional value for group
 *                 symbols
 */
private void fixedPointComputation(string prop, bool terminalVal, alias groupval)(Grammar grammar)
{
	bool changes;

    auto ref member(string prop)(inout Node a)
	{
		return __traits(getMember, a, prop);
	}

	void mark(Node node, bool val)
	{
		if (!member!prop(node) && val)
		{
			member!prop(node) = changes = true;
		}
	}

	void traverse(Node node)
	{
	    for (; node !is null; node = node.succ)
		{
			final switch (node.type)
			{
				case NodeType.Terminal:
					mark(node, terminalVal);
					break;
				case NodeType.Nonterminal:
					traverse(node.link);
					mark(node, member!prop(node.link));
					break;
				case NodeType.Group:
					traverse(node.link);
					mark(node, groupval(node) || member!prop(node.link));
					break;
				case NodeType.Alternative:
				    bool val = false;
					foreach (n; NodeLinkRange(node.link))
					{
						traverse(n);
						val |= member!prop(n);
					}
					mark(node, val);
					break;
				case NodeType.Sequence:
				    bool val = true;
					traverse(node.succInner);
					for (auto n = node.succInner; n !is null && val; n = n.succ)
					{
						val &= member!prop(n);
					}
					mark(node, val);
					break;
				case NodeType.Symbol:
					if (node.inner.type == NodeType.Terminal)
						traverse(node.inner);
					mark(node, member!prop(node.inner));
					break;
				case NodeType.Code:
					assert(false, "Statement not reachable");
			}
		}
	}

	do
	{
		changes = false;
		foreach (nt; grammar.nonterminals)
		{
			traverse(nt);
		}
	} while (changes);
}

/**
 * Calculates the epsilon productivity for each element of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the epsilon productivity of the symbols is
 *                computed
 */
void calculateDerivesEpsilon(Grammar grammar)
{
	fixedPointComputation!("derivesEpsilon", false,
	                       (Node n) => n.cardinality.among!(Cardinality.ZeroOrMore, Cardinality.ZeroOrOne))(grammar);
}

unittest
{
    // Wilhelm, Maurer; p. 311
	// Nonterminals Eq, Tq derives epsilon.
    immutable content = `
	%token id
	%%
	S : E ;
	E : T Eq ;
	Eq : ( "+" E )? ;
    T : F Tq ;
	Tq : ( "*" T )? ;
	F : id | "(" E ")" ;
`;
	auto grammar = Parser!Lexer(Lexer(content)).parse;
	calculateDerivesEpsilon(grammar);
	foreach (nt; grammar.nonterminals)
		assert(nt.derivesEpsilon == !(nt.name != "Eq" && nt.name != "Tq"),
		    "Nonterminal: `" ~ nt.name ~ "`" ~
			" Exp: " ~ to!string(!(nt.name != "Eq" && nt.name != "Tq")) ~
			" Act: " ~ to!string(nt.derivesEpsilon));
}

unittest
{
    // See https://www.codewars.com/kata/compute-nullable-non-terminals
    immutable content = `
    A : "a" B | "c" C ;
    B : ( A B )? ;
    C : "b" "c" ;
    D : ( "a" "b" )? ;
    E : B D ;
`;
	auto grammar = Parser!Lexer(Lexer(content)).parse;
	calculateDerivesEpsilon(grammar);
	foreach (nt; grammar.nonterminals)
		assert(nt.derivesEpsilon == (nt.name == "B" || nt.name == "D" || nt.name == "E"),
		    "Nonterminal: `" ~ nt.name ~ "`" ~
			" Exp: " ~ to!string(nt.name == "B" || nt.name == "D" || nt.name == "E") ~
			" Act: " ~ to!string(nt.derivesEpsilon));
}

unittest
{
    // See https://mkaul.wordpress.com/2009/12/11/computing-nullable-first-and-follow-sets/
    immutable content = `
    Z : "d" | X Y Z ;
    Y : ( "c" )? ;
    X : Y | "a" ;
`;
	auto grammar = Parser!Lexer(Lexer(content)).parse;
	calculateDerivesEpsilon(grammar);
	foreach (nt; grammar.nonterminals)
		assert(nt.derivesEpsilon == (nt.name == "X" || nt.name == "Y"),
		    "Nonterminal: `" ~ nt.name ~ "`" ~
			" Exp: " ~ to!string(nt.name == "X" || nt.name == "Y") ~
			" Act: " ~ to!string(nt.derivesEpsilon));
}

/**
 * Calculates the productivity of each symbol of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the productivity of the symbols is
 *                computed
 */
void calculateProductive(Grammar grammar)
{
	fixedPointComputation!("isProductive", true,
	                       (Node n) => false)(grammar);
}


unittest
{
    // Wilhelm, Maurer; p. 297
	// Nonterminal Z is not productive.
    immutable content = `
	%token a, b
    %%
	S : a X ;
	X : b S | a Y b Y ;
	Y : b a | a Z ;
	Z : a Z X ;
`;
	auto grammar = Parser!Lexer(Lexer(content)).parse;
	calculateProductive(grammar);
	foreach (nt; grammar.nonterminals)
		assert(nt.isProductive == (nt.name != "Z"),
		    "Nonterminal: `" ~ nt.name ~ "`" ~
			" Exp: " ~ to!string(nt.name != "Z") ~
			" Act: " ~ to!string(nt.isProductive));
}

/// Grammar Flow Analysis algorithm.
///
/// See Wilhelm, Maurer. p. 309
/// This is Tarjan's strongly connected components algorithm
public void computeSetValuedFunc(alias startValue, alias relation, string prop, R)(R range) if (isInputRange!R)
{
	immutable int INFINITY = int.max;

	alias NodeType = ElementType!R;
	Array!NodeType stack;
	size_t[NodeType] numbers;

	void dfs(NodeType a)
	{
		stack.insertBack(a);
		const d = stack.length;
		numbers[a] = d;

		__traits(getMember, a, prop) = startValue(a);
		foreach (b; relation(a)) {
			assert(b !is null);
			if (b !in numbers) { dfs(b); }
			numbers[a] = min(numbers[a], numbers[b]);
			assert(__traits(getMember, b, prop) !is null);
			__traits(getMember, a, prop).insert(__traits(getMember, b, prop)[]);
		}

		if (numbers[a] == d) {
			while (true) {
				auto t = stack.back;
				numbers[t] = INFINITY;
				__traits(getMember, t, prop) = __traits(getMember, a, prop);
				stack.removeBack;
				if (t == a)
					break;
			}
		}
	}

	foreach (v; range) {
		if (v !in numbers) {
			dfs(v);
		}
	}
}

unittest
{
	import std.algorithm.comparison : equal;
	import std.container.rbtree;

	alias StringSet = RedBlackTree!(string, "a < b", false);

	struct SimpleNode
	{
		string n;
		StringSet result;
		SimpleNode*[] next;
	}

	auto start(SimpleNode *a)
	{
		return new StringSet(a.n);
	}

	auto relation(SimpleNode *b)
	{
		return b.next;
	}

	auto a = new SimpleNode("A");
	auto b = new SimpleNode("B");
	auto c = new SimpleNode("C");

    /*
	 *   A ----> B
	 *   ^       |
	 *   +---C---+
	 */
    a.next ~= b;
    b.next ~= c;
    c.next ~= a;

	auto all = [ a, b, c ];

	computeSetValuedFunc!(start, relation, "result")(all);

	assert(equal(a.result[], [ "A", "B", "C" ]));
	assert(equal(b.result[], [ "A", "B", "C" ]));
	assert(equal(c.result[], [ "A", "B", "C" ]));

    /*
	 *   A ----> B ----> C
	 */
	c.next.length = 0;

	computeSetValuedFunc!(start, relation, "result")(all);

	assert(equal(a.result[], [ "A", "B", "C" ]));
	assert(equal(b.result[], [ "B", "C" ]));
	assert(equal(c.result[], [ "C" ]));

	a.next.length = 0;
	b.next.length = 0;

	computeSetValuedFunc!(start, relation, "result")(all);

	assert(equal(a.result[], [ "A" ]));
	assert(equal(b.result[], [ "B" ]));
	assert(equal(c.result[], [ "C" ]));

	auto d = new SimpleNode("D");
	auto e = new SimpleNode("E");

    /*
	 *   D ----> A ----> B ----> E
	 *           ^       |
	 *           +---C---+
	 */
    a.next ~= b;
    b.next ~= c;
    c.next ~= a;
	d.next ~= a;
	b.next ~= e;

	all = [ e, d, c, b, a ];
	computeSetValuedFunc!(start, relation, "result")(all);

	assert(equal(a.result[], [ "A", "B", "C", "E" ]));
	assert(equal(b.result[], [ "A", "B", "C", "E" ]));
	assert(equal(c.result[], [ "A", "B", "C", "E" ]));
	assert(equal(d.result[], [ "A", "B", "C", "D", "E" ]));
	assert(equal(e.result[], [ "E" ]));

	all = [ e, a, c, b, d ];
	computeSetValuedFunc!(start, relation, "result")(all);

	assert(equal(a.result[], [ "A", "B", "C", "E" ]));
	assert(equal(b.result[], [ "A", "B", "C", "E" ]));
	assert(equal(c.result[], [ "A", "B", "C", "E" ]));
	assert(equal(d.result[], [ "A", "B", "C", "D", "E" ]));
	assert(equal(e.result[], [ "E" ]));
}

/**
 * Computes the epsilon-free first sets of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the epsilon-free first sets is computed
 */
void calculateFirstSets(Grammar grammar)
{
	// Start value is nonempty only for a terminal
	static TerminalSet startValue(Node a)
	in (a !is null)
	out (r; r !is null && r.length <= 1)
	{
		auto set = new TerminalSet;
		if (a.type == NodeType.Terminal)
		{
			set.insert(a.name);
		}
		return set;
	}

	// Definition of the relation:
	// a R b <=> 1. a is a nonterminal and b its right hand side or
	//           2. b is a direct subexpression of a and contributes to
	//              the first set of a
	static auto relation(Node a)
	in (a !is null)
	{
		Node[] rel;

		void add(Node n)
		{
			for (; n !is null; n = n.succ)
			{
				rel ~= n;
				if (!n.derivesEpsilon)
					break;
			}
		}

		final switch (a.type)
		{
			case NodeType.Nonterminal:
				    assert(a.link !is null);
					rel ~= a.link;
					break;
			case NodeType.Group:
			case NodeType.Alternative:
				assert(a.link !is null);
				foreach (n; NodeLinkRange(a.link))
				{
					rel ~= n;
				}
				break;
			case NodeType.Sequence:
				add(a.succInner);
				break;
			case NodeType.Symbol:
				assert(a.inner !is null);
				rel ~= a.inner;
				break;
			case NodeType.Terminal:
				// A terminal has no relation
				break;
			case NodeType.Code:
				assert(false, "Statement not reachable");
		}
		return rel;
	}

	auto noderange = filter!(n => n.type != NodeType.Code)(grammar.nodes);
	computeSetValuedFunc!(startValue, relation, "firstSet")(noderange);
	version(unittest) checkFirstSets(grammar);
	debug checkFirstSets(grammar);
}

private void checkFirstSets(Grammar grammar)
{
	void checkExpr(Node n)
	{
		import std.algorithm.comparison : equal;
		import std.conv : to;

		assert(n !is null, "Node is null");
		assert(n.type == NodeType.Code || n.firstSet !is null, "FIRST set is null " ~ n.toString);
		if (n.type == NodeType.Sequence)
		{
			TerminalSet exp = new TerminalSet;
			for (auto v = n.succInner; v !is null; v = v.succ)
			{
				exp.insert(v.firstSet[]);
				if (!v.derivesEpsilon)
					break;
			}
			assert(equal(n.firstSet[], exp[]), "FIRST sets not equal (sequence)");
		}
		else if (n.type == NodeType.Group)
		{
			assert(n.link !is null);
			assert(n.link.firstSet !is null);
			if (n.cardinality == Cardinality.One || n.cardinality == Cardinality.OneOrMore)
				assert(equal(n.firstSet[], n.link.firstSet[]), "FIRST sets not equal");
			else
				assert(equal(n.firstSet[], n.link.firstSet[]), "FIRST sets not equal");
		}
		else if (n.type == NodeType.Alternative)
		{
			TerminalSet exp = new TerminalSet;
			for (auto v = n.link; v !is null; v = v.link)
			{
				exp.insert(v.firstSet[]);
			}
			assert(equal(n.firstSet[], exp[]), "FIRST sets not equal (alternative)");
		}
		else if (n.type == NodeType.Nonterminal)
		{
			assert(n.link !is null);
			assert(n.link.firstSet !is null);
			assert(equal(n.firstSet[], n.link.firstSet[]), "FIRST sets not equal (nonterminal)");
		}
		else if (n.type == NodeType.Symbol)
		{
			assert(n.inner !is null);
			assert(n.inner.firstSet !is null);
			assert(equal(n.firstSet[], n.inner.firstSet[]), "FIRST sets not equal (symbol " ~ n.name ~ ")");
		}
		else if (n.type == NodeType.Terminal)
		{
			assert(n.firstSet.length == 1, "FIRST set of terminal has size different from 1");
			assert(equal(n.firstSet[], [n.name]), "FIRST set of terminal does not contain terminal name");
		}
	}

	foreach (v; grammar.nodes)
		checkExpr(v);
}

version(unittest)
private void checkFirstSet(Node n, string[] values ...)
{
	assert(n !is null);
	foreach (v; values)
		assert(v in n.firstSet,
			"Terminal `" ~ v ~ "` not in first set of `" ~n.name ~ "`");
	assert(n.firstSet.length == values.length,
		"Nonterminal `" ~ n.name ~ "`" ~
		" Exp: " ~ to!string(values.length) ~
		" Act: " ~ to!string(n.firstSet.length));
}

unittest
{
    // Wilhelm, Maurer; p. 311
	// Nonterminals Eq, Tq derives epsilon.
    immutable content = `
	%token id
	%%
	S : E ;
	E : T Eq ;
	Eq : ( "+" E )? ;
    T : F Tq ;
	Tq : ( "*" T )? ;
	F : id | "(" E ")" ;
`;
	auto grammar = Parser!Lexer(Lexer(content)).parse;
	calculateReachable(grammar);
	calculateDerivesEpsilon(grammar);
	calculateFirstSets(grammar);
	Node[string] map;
	foreach (nt; grammar.nonterminals)
	{
		map[nt.name] = nt;
		assert(nt.isReachable);
	}

	checkFirstSet(map["S"], "id", "\"(\"");
	assert(!map["S"].derivesEpsilon);
	checkFirstSet(map["E"], "id", "\"(\"");
	assert(!map["E"].derivesEpsilon);
	checkFirstSet(map["Eq"], "\"+\"");
	assert(map["Eq"].derivesEpsilon);
	checkFirstSet(map["T"], "id", "\"(\"");
	assert(!map["T"].derivesEpsilon);
	checkFirstSet(map["Tq"], "\"*\"");
	assert(map["Tq"].derivesEpsilon);
	checkFirstSet(map["F"], "id", "\"(\"");
	assert(!map["F"].derivesEpsilon);
}

/**
 * Computes the follow sets of the grammar.
 *
 * Params:
 * 		grammar = grammar for which the follow sets is computed
 */
void calculateFollowSets(Grammar grammar)
{
	// Start values are the epsilon-free first sets
	TerminalSet startValue(Node a)
	in (a !is null)
	{
		auto set = new TerminalSet;
		// Equation 5
		if (a.type == NodeType.Sequence && a.back.type == NodeType.Group &&
			a.back.cardinality.among!(Cardinality.ZeroOrMore, Cardinality.OneOrMore))
		{
			set.insert(a.firstSet[]);
		}
		// Equation 1
		else if (a.type == NodeType.Sequence && a.back.type == NodeType.Nonterminal &&
			a.back.back is null)
		{
			set.insert(grammar.eoiTerminal.name);
		}
		// Equation 3
		else
		{
			auto v = a.succ;
			if (v !is null)
				set.insert(v.firstSet[]);
		}
		return set;
	}

	// Definition of the relation:
	// a R b <=> 1. a and b are extended context-free items
	//           2. b contributes to the follow set of a
	//
	// Let Y -> a b c be a production. If b is a regular subexpression
	// then the extended context-free item is written as
	// [ Y -> a .b c ]
	static auto relation(Node a)
	in (a !is null)
	{
		Node[] rel;

		void add(Node n)
		{
			if (n.type == NodeType.Nonterminal)
			{
				foreach (v; NodeLinkRange(n.back))
				{
					assert(v.type == NodeType.Symbol);
					rel ~= v;
				}
			}
			else
			{
				rel ~= n;
			}
		}

		final switch (a.type)
		{
			case NodeType.Symbol:
			case NodeType.Group:
			case NodeType.Alternative:
				auto n = a.succ;
				if (n is null)
				{
					// Equation (4)
					// Leaking abstraction: succ is null but there may be
					// traling code nodes
					for (n = a; n.next !is null; n = n.next)
						{}
					if (n.back !is null)
						add(n.back);
				}
				else
				{
					// Equation (3)
					if (n.derivesEpsilon)
					{
						rel ~= n;
					}
				}
				break;
			case NodeType.Sequence:
				// Equation: (2), (5), (6)
				assert(a.back !is null);
				assert(a.next is null);
				add(a.back);
				break;
			case NodeType.Terminal:
				assert(false, "Statement not reachable");
			case NodeType.Nonterminal:
				assert(false, "Statement not reachable");
			case NodeType.Code:
				assert(false, "Statement not reachable");
		}
		return rel;
	}

	auto noderange = filter!(n => n.type.among!(NodeType.Alternative, NodeType.Group, NodeType.Sequence, NodeType.Symbol))(grammar.nodes);
	computeSetValuedFunc!(startValue, relation, "followSet")(noderange);
	version(unittest) checkFollowSets(grammar);
	debug checkFollowSets(grammar);
}

private void checkFollowSets(Grammar grammar)
{
	void checkExpr(Node n)
	{
		import std.algorithm.comparison : equal;
		import std.conv : to;

		assert(n !is null, "Node is null");
		assert(!n.type.among!(NodeType.Alternative, NodeType.Group, NodeType.Sequence, NodeType.Symbol) || n.followSet !is null, "FOLLOW set is null " ~ n.toString);
		if (n.type == NodeType.Alternative)
		{
			for (auto v = n.link; v !is null; v = v.link)
			{
				assert(equal(n.followSet[], v.followSet[]), "FOLLOW sets not equal (alternative)");
			}
		}
		else if (n.type == NodeType.Group)
		{
		}
		else if (n.type == NodeType.Sequence)
		{
		}
	}

	foreach (v; grammar.nodes)
		checkExpr(v);
}

version(unittest)
private void checkFollowSet(Node nt, string[] values ...)
{
	assert(nt !is null);
	assert(nt.type == NodeType.Nonterminal);
	assert(nt.link !is null);
	auto n = nt.link;
	assert(n.followSet !is null);
	foreach (v; values)
		assert(v in n.followSet,
			"Terminal `" ~ v ~ "` not in follow set of `" ~nt.name ~ "`");
	assert(n.followSet.length == values.length,
		"Nonterminal `" ~ nt.name ~ "`" ~
		" Exp: " ~ to!string(values.length) ~
		" Act: " ~ to!string(n.followSet.length));
}

unittest
{
    // Holub; p. 214
	// This is basically the same example as from Wilhelm, Maurer.
	// Please note the ";" at the end of production stmt.
    immutable content = `
	%token number
	%%
	stmt : expr ";" ;
	expr : ( term exprq )? ;
	exprq : ( "+" term exprq )? ;
    term : factor termq ;
	termq : ( "*" factor termq )? ;
	factor : "(" expr ")" | number ;
`;
	auto grammar = Parser!Lexer(Lexer(content)).parse;
	calculateReachable(grammar);
	calculateDerivesEpsilon(grammar);
	calculateFirstSets(grammar);
	calculateFollowSets(grammar);
	Node[string] map;
	foreach (nt; grammar.nonterminals)
	{
		map[nt.name] = nt;
		assert(nt.isReachable);
	}
	checkFirstSet(map["stmt"], "number", "\"(\"", "\";\"");
	checkFollowSet(map["stmt"], "_eoi");
	assert(!map["stmt"].derivesEpsilon);
	checkFirstSet(map["expr"], "number", "\"(\"");
	checkFollowSet(map["expr"], "\";\"", "\")\"");
	assert(map["expr"].derivesEpsilon);
	checkFirstSet(map["exprq"], "\"+\"");
	checkFollowSet(map["exprq"], "\";\"", "\")\"");
	assert(map["exprq"].derivesEpsilon);
	checkFirstSet(map["term"], "number", "\"(\"");
	checkFollowSet(map["term"], "\"+\"", "\";\"", "\")\"");
	assert(!map["term"].derivesEpsilon);
	checkFirstSet(map["termq"], "\"*\"");
	checkFollowSet(map["termq"], "\"+\"", "\";\"", "\")\"");
	assert(map["termq"].derivesEpsilon);
	checkFirstSet(map["factor"], "number", "\"(\"");
	checkFollowSet(map["factor"], "\"*\"", "\"+\"", "\";\"", "\")\"");
	assert(!map["factor"].derivesEpsilon);
}
