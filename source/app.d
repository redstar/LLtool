// Written in the D programming language.
/**
 * Main module of the application.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */

import lexer;
import parser;
import algo;
import generator;
import cmdline;
import diagnostics;
import graphviz;
import verify;
import std.file : write, readText, FileException;
import std.path : setExtension;
import std.range : isOutputRange;
import std.stdio : File, stdout;
import std.traits : isSomeString;

struct Sink
{
	private File file;

	this(string name)
	{
		if (name == "-")
			file = stdout;
		else
			file = File(name, "w+");
	}

	void close()
	{
		file.close;
	}

	void put(T)(T value) if (isSomeString!T)
	{
		file.write(value);
	}
}

static assert(isOutputRange!(Sink, string));

void main(string[] args)
{
	if (!parseCmdLine(args))
		return ;

	try
	{
		string inputFilename = args[1];
		string outputFilename = output.length > 0 ? output : inputFilename.setExtension("mixin");

		string content = readText(inputFilename);
		auto lexer = Lexer(content);
		auto parser = Parser!Lexer(lexer);
		auto grammar = parser.parse();
		if (debugging)
		{
			write(inputFilename.setExtension("dot"), grammar.toGraphviz);
		}
		if (hasErrors)
			return ;

		calculateReachable(grammar);
		calculateDerivesEpsilon(grammar);
		calculateProductive(grammar);
		checkGrammar(content, grammar);
		if (hasErrors)
			return ;

		calculateFirstSets(grammar);
		calculateFollowSets(grammar);
		checkLL(content, grammar);
		if (hasErrors)
			return ;

		auto sink = Sink(outputFilename);
		scope(exit) sink.close();
		generate(sink, grammar);
	}
	catch (FileException e)
	{
		error(e.msg);
	}
	catch (Throwable e)
	{
		version (AFL)
		{
			import core.stdc.stdlib: abort;
			abort();
		}
		else
		{
			throw e;
		}
	}
}
