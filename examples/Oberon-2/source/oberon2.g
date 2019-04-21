/*
Source: https://www.ssw.uni-linz.ac.at/Research/Papers/Oberon2.pdf

The Programming Language Oberon-2

H. Mössenböck, N. Wirth, Institut für Computersysteme, ETH Zürich, 1992-1996

Changes:
 - Enclosed all keywords with double quotes
 - Replaced meta symbols [ ] with ( )? and { } with ( )*
 - Added a new production for non terminal number
 - Added four conflict resolvers
*/
%token ident, string, character, integer, real
%start Module
%%
Module
  = "MODULE" ident ";" (ImportList)? DeclSeq ("BEGIN" StatementSeq)? "END" ident ".".

ImportList
  = "IMPORT" Import ("," Import)* ";".

Import
  =                                     (. string aliasName; size_t aliasPos; .)
    (%if (. isAlias() .) ident          (. aliasName = tok.val; aliasPos = tok.pos; .)
    ":=")? ident                        (. addImport(tok.pos, tok.val, aliasPos, aliasName); .)
  .

DeclSeq
  = ( "CONST" (ConstDecl ";" )* | "TYPE" (TypeDecl ";")* | "VAR" (VarDecl ";")* )*
    (%if (. isProcDecl() .) ProcDecl ";" | ForwardDecl ";")*.

ConstDecl
  = IdentDef "=" ConstExpr.

TypeDecl
  = IdentDef "=" Type.

VarDecl
  = IdentList ":" Type.

ProcDecl
  = "PROCEDURE" (Receiver)? IdentDef (FormalPars)? ";" DeclSeq ("BEGIN" StatementSeq)? "END" ident.

ForwardDecl
  = "PROCEDURE" "^" (Receiver)? IdentDef (FormalPars)?.

FormalPars
  = "(" (FPSection (";" FPSection)*)? ")" (":" Qualident)?.

FPSection
  = ("VAR")? ident ("," ident)* ":" Type.

Receiver
  = "(" ("VAR")? ident ":" ident ")".

Type
  = Qualident
  | "ARRAY" (ConstExpr ("," ConstExpr)*)? "OF" Type
  | "RECORD" ("("Qualident")")? FieldList (";" FieldList)* "END"
  | "POINTER" "TO" Type
  | "PROCEDURE" (FormalPars)?
  .

FieldList
  = (IdentList ":" Type)?.

StatementSeq
  = Statement (";" Statement)*.

Statement
  = ( Designator (":=" Expr | ( "(" (ExprList)? ")")?)
      | "IF" Expr "THEN" StatementSeq
        ("ELSIF" Expr "THEN" StatementSeq)?
        ("ELSE" StatementSeq)?
        "END"
      | "CASE" Expr "OF" Case
        ("|" Case)*
        ("ELSE" StatementSeq)?
        "END"
      | "WHILE" Expr "DO" StatementSeq "END"
      | "REPEAT" StatementSeq "UNTIL" Expr
      | "FOR" ident ":=" Expr "TO" Expr ("BY" ConstExpr)? "DO" StatementSeq "END"
      | "LOOP" StatementSeq "END"
      | "WITH" Guard "DO" StatementSeq
        ("|" Guard "DO" StatementSeq)*
        ("ELSE" StatementSeq)?
        "END"
      | "EXIT"
      | "RETURN" (Expr)?
    )?
  .

Case
  = (CaseLabels ("," CaseLabels)* ":" StatementSeq)?.

CaseLabels
  = ConstExpr (".." ConstExpr)?.

Guard
  = Qualident ":" Qualident.

ConstExpr
  = Expr.

Expr
  = SimpleExpr (Relation SimpleExpr)?.

SimpleExpr
  = ("+" | "-")? Term (AddOp Term)*.

Term
  = Factor (MulOp Factor)*.

Factor
  = Designator ("(" (ExprList)? ")")?
  | number
  | character
  | string
  | "NIL"
  | Set
  | "(" Expr ")"
  | "~" Factor
  .

Set
  = "{" (Element ("," Element)* )? "}".

Element
  = Expr (".." Expr)?.

Relation
  = "=" | "#" | "<" | "<=" | ">" | ">=" | "IN" | "IS".

AddOp
  = "+" | "-" | "OR".

MulOp
  = "*" | "/" | "DIV" | "MOD" | "&".

Designator
  = Qualident ( %if (. !isProcCall() .) ("." ident | "[" ExprList "]" | "^" | "(" Qualident ")") )*.

ExprList
  = Expr ("," Expr)*.

IdentList
  = IdentDef ("," IdentDef)*.

Qualident
  = ( %if (. isModule() .) ident ".")? ident.

IdentDef
  = ident ("*" | "-")?.

number
  = integer | real.