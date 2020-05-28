// Written in the D programming language.
/**
 * Handling of variables.
 *
 * Copyright:  (C) 2020 by Kai Nacke
 *
 * License: See LICENSE file
 *
 * Authors: Kai Nacke
 */
module variables;

enum VarName
{
    Language,
    ApiNamespace,
    ApiParserClass,
    ApiPrefix,
    ApiSymbolPrefix,
    ApiTokenPrefix,
    CodePreferSwitch,
}

enum VarKind
{
    Identifier,
    Code,
    String,
    Flag
}

struct VariableStore
{
    string get(VarName name)
    {
        string* str = (name in store);
        if (str !is null)
            return *str;
        return "";
    }

    bool set(string name, string value, VarKind kind, out string msg)
    {
        int idx = 0;
        while (idx < definedVars.length && name != definedVars[idx].extName)
            ++idx;
        if (idx == definedVars.length)
        {
            msg = "Unknown variable name";
            return true;
        }
        if (kind != definedVars[idx].kind)
        {
            writefln("Found Kind: %s", definedVars[idx].kind);
            msg = "Wrong variable type";
            return true;
        }
        if (kind == VarKind.Flag && value != "" && value != "true")
        {
            msg = "Wrong value for boolean type";
            return true;
        }

        store[definedVars[idx].name] = value;
        return false;
    }

    void set(VarName name, string value)
    {
        store[name] = value;
    }

private:
    string[VarName] store;

    struct Def
    {
        string extName;
        VarName name;
        VarKind kind;
    }

    static Def[] definedVars = [
        {"language", VarName.Language, VarKind.String},
        {"api.namespace", VarName.ApiNamespace, VarKind.Code},
        {"api.parser.class", VarName.ApiParserClass, VarKind.Code},
        {"api.prefix", VarName.ApiPrefix, VarKind.Code},
        {"api.symbol.prefix", VarName.ApiSymbolPrefix, VarKind.Code},
        {"api.token.prefix", VarName.ApiTokenPrefix, VarKind.Code},
        {"code.prefer.switch", VarName.CodePreferSwitch, VarKind.Flag},
    ];
}
