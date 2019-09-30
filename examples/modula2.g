/*
 * Modula-2 grammar from "Programming in Modula-2", fourth edition.
 * See http://freepages.modula2.org/report4/modula-2.html
 *
 * The following changes were made:
 * - ident renamed to identifier
 * - integer and real renamed to integer_literal and real_literal and
 *   introduced char_literal. This is inspired by ISO.
 *
 * This grammar has four LL(A)1) conflicts:
 * - between qualident and designator: "." is start and successor of deletable element
 * - in SimpleType between qualident and SubrangeType: both alternatives start with identifier
 * - in factor between qualident and designator: both alternatives start with identifier
 * - in statement between assignment and assignment: both alternatives start with identifier
 *
 * A resolver can be used to resolve these conflicts.
 */
%token identifier, integer_literal, char_literal, real_literal, string_literal
%start CompilationUnit
%%
number : integer_literal | real_literal ;
string : string_literal | char_literal;
qualident : identifier ("." identifier )* ;
ConstantDeclaration : identifier "=" ConstExpression ;
ConstExpression : expression ;
TypeDeclaration : identifier "=" type ;
type : SimpleType | ArrayType | RecordType | SetType| PointerType | ProcedureType ;
SimpleType : qualident | enumeration | SubrangeType ;
enumeration : "(" IdentList ")" ;
IdentList : identifier ( "," identifier )* ;
SubrangeType : ( identifier )? "[" ConstExpression ".." ConstExpression "]" ;
ArrayType : "ARRAY" SimpleType ( "," SimpleType )* "OF" type ;
RecordType : "RECORD" FieldListSequence "END" ;
FieldListSequence : FieldList ( ";" FieldList )* ;
FieldList : (IdentList ":" type |
            "CASE" ( identifier)? ":" qualident "OF" variant
            ( "|" variant )* ( "ELSE" FieldListSequence )? "END" )? ;
variant : ( CaseLabelList ":" FieldListSequence )? ;
CaseLabelList : CaseLabels ( "," CaseLabels )* ;
CaseLabels : ConstExpression ( ".." ConstExpression )? ;
SetType : "SET" "OF" SimpleType ;
PointerType : "POINTER" "TO" type ;
ProcedureType : "PROCEDURE" ( FormalTypeList )? ;
FormalTypeList : "(" ( ("VAR")? FormalType
                 ("," ( "VAR")? FormalType )* )? ")" ( ":" qualident)? ;
VariableDeclaration : IdentList ":" type ;
designator : qualident ( "." identifier | "[" ExpList "]" | "^" )* ;
ExpList : expression ( "," expression )* ;
expression : SimpleExpression ( relation SimpleExpression )? ;
relation : "=" | "#" | "<" | "<=" | ">" | ">=" | "IN" ;
SimpleExpression : ( "+" | "-" )? term ( AddOperator term )* ;
AddOperator : "+" | "-" | "OR" ;
term : factor ( MulOperator factor )* ;
MulOperator : "*" | "/" | "DIV" | "MOD" | "AND" ;
factor : number | string | set |
         designator ( ActualParameters )? |
         "(" expression ")" | "NOT" factor ;
set : ( qualident )? "{" ( element ( "," element )* )? "}" ;
element : expression ( ".." expression )? ;
ActualParameters : "(" ( ExpList )? ")" ;
statement : ( assignment | ProcedureCall |
            IfStatement | CaseStatement |
            WhileStatement | RepeatStatement |
            LoopStatement | ForStatement |
            WithStatement | "EXIT" |
            "RETURN" ( expression )? )? ;
assignment : designator ":=" expression ;
ProcedureCall : designator ( ActualParameters )? ;
StatementSequence : statement ( ";" statement )* ;
IfStatement : "IF" expression "THEN" StatementSequence
              ( "ELSIF" expression "THEN" StatementSequence )*
              ( "ELSE" StatementSequence )? "END" ;
CaseStatement : "CASE" expression "OF" case
                ( "|" case )* ( "ELSE" StatementSequence )? "END" ;
case : ( CaseLabelList ":" StatementSequence )? ;
WhileStatement : "WHILE" expression "DO"
                 StatementSequence "END" ;
RepeatStatement : "REPEAT" StatementSequence
                  "UNTIL" expression ;
ForStatement : "FOR" identifier ":=" expression
               "TO" expression ( "BY" ConstExpression )?
               "DO" StatementSequence "END" ;
LoopStatement : "LOOP" StatementSequence "END" ;
WithStatement : "WITH" designator "DO"
                StatementSequence "END" ;
ProcedureDeclaration : ProcedureHeading ";" block identifier ;
ProcedureHeading : "PROCEDURE" identifier ( FormalParameters )? ;
block : ( declaration )*  ( "BEGIN" StatementSequence )? "END" ;
declaration : "CONST" ( ConstantDeclaration ";" )* |
              "TYPE" ( TypeDeclaration ";" )* |
              "VAR" ( VariableDeclaration ";" )* |
              ProcedureDeclaration ";" |
              ModuleDeclaration ";" ;
FormalParameters : "(" ( FPSection ( ";" FPSection )* )? ")" ( ":" qualident )? ;
FPSection : ( "VAR" )? IdentList ":" FormalType ;
FormalType : ( "ARRAY" "OF" )? qualident ;
ModuleDeclaration : "MODULE" identifier ( priority )?
                    ";" ( import )* ( export )? block identifier ;
priority : "[" ConstExpression "]" ;
export : "EXPORT" ( "QUALIFIED" )? IdentList ";" ;
import : ("FROM" identifier)? "IMPORT" IdentList ";" ;
DefinitionModule : "DEFINITION" "MODULE" identifier ";"
                   ( import )* ( definition )* "END" identifier "." ;
definition : "CONST" ( ConstantDeclaration ";" )* |
             "TYPE" ( identifier ( "="type )? ";" )* |
             "VAR" ( VariableDeclaration ";" )* |
             ProcedureHeading ";" ;
ProgramModule : "MODULE" identifier ( priority )? ";"
                ( import )* block identifier "." ;
CompilationUnit : DefinitionModule |
                  ("IMPLEMENTATION")? ProgramModule ;