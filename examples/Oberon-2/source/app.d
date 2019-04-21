/**
 * Main module of the application.
 *
 * Copyright:  (C) 2019 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */

import std.stdio;

import lexer;
import parser;
import std.file : readText, FileException;
import std.stdio : stderr, writeln, writefln;

int main(string[] args)
{

	try
	{
		if (args.length == 2)
		{
			string content = readText(args[1]);
			auto lexer = Lexer(content);
			auto parser = Parser(lexer);
			parser.parse();
			writeln("Done.");
			return 0;
		}
		else
			writefln("%s: requires one file argument", args[0]);
	}
	catch (FileException e)
	{
		writeln(e.msg);
	}
	return 1;
}
