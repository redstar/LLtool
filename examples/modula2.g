/*
 * Modula-2 grammar from "Programming in Modula-2", fourth edition.
 * See http://freepages.modula2.org/report4/modula-2.html
 *
 * This grammar has four LL(1) conflicts:
 * - between qualident and designator: "." is start and successor of deletable element
 * - in SimpleType between qualident and SubrangeType: both alternatives start with ident
 * - in factor between qualident and designator: both alternatices start with ident
 * - in statement between assignment and assignment: both alternatices start with ident
 *
 * A resolver can be used to resolve these conflicts.
 */
%token ident, integer, real, string
%start CompilationUnit
%%
number = integer | real .
qualident = ident ( "." ident )* .
ConstantDeclaration = ident "=" ConstExpression .
ConstExpression = expression .
TypeDeclaration = ident "=" type .
type = SimpleType | ArrayType | RecordType | SetType| PointerType | ProcedureType .
SimpleType = qualident | enumeration | SubrangeType .
enumeration = "(" IdentList ")" .
IdentList = ident ( "," ident )* .
SubrangeType = ( ident )? "[" ConstExpression ".." ConstExpression "]" .
ArrayType = "ARRAY" SimpleType ( "," SimpleType )* "OF" type .
RecordType = "RECORD" FieldListSequence "END" .
FieldListSequence = FieldList ( ";" FieldList )* .
FieldList = (IdentList ":" type |
            "CASE" ( ident)? ":" qualident "OF" variant
            ( "|" variant )* ( "ELSE" FieldListSequence )? "END" )? .
variant = ( CaseLabelList ":" FieldListSequence )? .
CaseLabelList = CaseLabels ( "," CaseLabels )* .
CaseLabels = ConstExpression ( ".." ConstExpression )? .
SetType = "SET" "OF" SimpleType .
PointerType = "POINTER" "TO" type .
ProcedureType = "PROCEDURE" ( FormalTypeList )? .
FormalTypeList = "(" ( ("VAR")? FormalType
                 ("," ( "VAR")? FormalType )* )? ")" ( ":" qualident)? .
VariableDeclaration = IdentList ":" type .
designator = qualident ( "." ident | "[" ExpList "]" | "^" )* .
ExpList = expression ( "," expression )* .
expression = SimpleExpression ( relation SimpleExpression )? .
relation = "=" | "#" | "<" | "<=" | ">" | ">=" | "IN" .
SimpleExpression = ( "+" | "-" )? term ( AddOperator term )* .
AddOperator = "+" | "-" | "OR" .
term = factor ( MulOperator factor )* .
MulOperator = "*" | "/" | "DIV" | "MOD" | "AND" .
factor = number | string | set |
         designator ( ActualParameters )? |
         "(" expression ")" | "NOT" factor .
set = ( qualident )? "{" ( element ( "," element )* )? "}" .
element = expression ( ".." expression )? .
ActualParameters = "(" ( ExpList )? ")" .
statement = ( assignment | ProcedureCall |
            IfStatement | CaseStatement |
            WhileStatement | RepeatStatement |
            LoopStatement | ForStatement |
            WithStatement | "EXIT" |
            "RETURN" ( expression )? )? .
assignment = designator ":=" expression .
ProcedureCall = designator ( ActualParameters )? .
StatementSequence = statement ( ";" statement )* .
IfStatement = "IF" expression "THEN" StatementSequence
              ( "ELSIF" expression "THEN" StatementSequence )*
              ( "ELSE" StatementSequence )? "END" .
CaseStatement = "CASE" expression "OF" case
                ( "|" case )* ( "ELSE" StatementSequence )? "END" .
case = ( CaseLabelList ":" StatementSequence )? .
WhileStatement = "WHILE" expression "DO"
                 StatementSequence "END" .
RepeatStatement = "REPEAT" StatementSequence
                  "UNTIL" expression .
ForStatement = "FOR" ident ":=" expression
               "TO" expression ( "BY" ConstExpression )?
               "DO" StatementSequence "END" .
LoopStatement = "LOOP" StatementSequence "END" .
WithStatement = "WITH" designator "DO"
                StatementSequence "END" .
ProcedureDeclaration = ProcedureHeading ";" block ident .
ProcedureHeading = "PROCEDURE" ident ( FormalParameters )? .
block = ( declaration )*  ( "BEGIN" StatementSequence )? "END" .
declaration = "CONST" ( ConstantDeclaration ";" )* |
              "TYPE" ( TypeDeclaration ";" )* |
              "VAR" ( VariableDeclaration ";" )* |
              ProcedureDeclaration ";" |
              ModuleDeclaration ";" .
FormalParameters = "(" ( FPSection ( ";" FPSection )* )? ")" ( ":" qualident )? .
FPSection = ( "VAR" )? IdentList ":" FormalType .
FormalType = ( "ARRAY" "OF" )? qualident .
ModuleDeclaration = "MODULE" ident ( priority )?
                    ";" ( import )* ( export )? block ident .
priority = "[" ConstExpression "]" .
export = "EXPORT" ( "QUALIFIED" )? IdentList ";" .
import = ("FROM" ident)? "IMPORT" IdentList ";" .
DefinitionModule = "DEFINITION" "MODULE" ident ";"
                   ( import )* ( definition )* "END" ident ".".
definition = "CONST" ( ConstantDeclaration ";" )* |
             "TYPE" ( ident ( "="type )? ";" )* |
             "VAR" ( VariableDeclaration ";" )* |
             ProcedureHeading ";" .
ProgramModule = "MODULE" ident ( priority )? ";"
                ( import )* block ident "." .
CompilationUnit = DefinitionModule |
                  ("IMPLEMENTATION")? ProgramModule.