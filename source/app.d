// Written in the D programming language.
/**
 * Main module of the application.
 *
 * Copyright:  (C) 2019, 2020 by Kai Nacke
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
import report;
import variables;
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
		return;

	try
	{
		string inputFilename = args[1];

		string content = readText(inputFilename);
		auto lexer = Lexer(content);
		auto parser = Parser!Lexer(lexer);
		auto grammar = parser.parse();
		if (debugging)
		{
			write(inputFilename.setExtension("dot"), grammar.toGraphviz);
		}
		if (hasErrors)
			return;

		calculateReachable(grammar);
		calculateDerivesEpsilon(grammar);
		calculateProductive(grammar);
		checkGrammar(content, grammar);
		if (hasErrors)
			return;

		calculateFirstSets(grammar);
		calculateFollowSets(grammar);
		checkLL(content, grammar);
		if (hasErrors)
			return;

		if (generateCPP)
			grammar.variables.set(VarName.Language, "c++");
		if (cppClassname.length)
			grammar.variables.set(VarName.ApiParserClass, cppClassname);
		if (generateSwitch)
			grammar.variables.set(VarName.CodePreferSwitch, "true");

		string outputFilename = output.length > 0 ? output
			: inputFilename.setExtension(grammar.variables.get(VarName.Language) == "c++"
					? "inc" : "mixin");

		auto sink = Sink(outputFilename);
		scope (exit)
			sink.close();
		generate(sink, grammar);

		if (xref)
			printReport(grammar);
	}
	catch (FileException e)
	{
		error(e.msg);
	}
}
