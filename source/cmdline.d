// Written in the D programming language.
/**
 * This module handles command line parsing and holds all parsed values.
 *
 * Copyright:  (C) 2019, 2020 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module cmdline;

import std.getopt;

/// Name of output file
string output;

/// Debugging requested?
bool debugging;

/// Treat warnings as errors?
bool warningsAreErrors;

/// Should we generate C++ source? (Iiiiihhh!)
bool generateCPP;

/// Optional name of C++ class.
string cppClassname;

/**
 * Parses the commandline.
 *
 * Params:
 *      args = command line argumens
 *
 * Returns:
 *      true if all options were consumed.
 */
bool parseCmdLine(ref string[] args)
{
	immutable string head = "LLtool - a LL(1) parser generator for D";
	try {
		auto result = getopt(args,
			std.getopt.config.caseSensitive,
			"cpp", "Generate C++ source", &generateCPP,
			"cppclass", "Name of generated C++ class", &cppClassname,
			"debug|d", "Enable debug output", &debugging,
			"o", "The name of output file", &output,
			"w", "Treat warnings are errors", &warningsAreErrors,
		);
		if (result.helpWanted || args.length != 2)
		{
			defaultGetoptPrinter(head ~ "\nUsage: LLtool [options] grammar\n", result.options);
			return false;
		}
	}
	catch (GetOptException ex)
	{
		import std.stdio : writefln;
		writefln("%s\n%s. See 'LLtool --help'.", head, ex.message);
		return false;
	}
    return true;
}