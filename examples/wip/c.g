/* Source: http://www.quut.com/c/ANSI-C-grammar-y-1999.html */
%token IDENTIFIER, CONSTANT, STRING_LITERAL, TYPE_NAME
%start translation_unit
%%

primary_expression
    : IDENTIFIER
    | CONSTANT
    | STRING_LITERAL
    | '(' expression ')'
    ;

postfix_expression
    : ( %if {. isTypeExpr() .} '(' type_name ')' '{' initializer_list ( ',' )? '}'
				| primary_expression
			)
      ( '++'
			  | '--'
			  | '->' IDENTIFIER
			  | '.' IDENTIFIER
        | '(' ( argument_expression_list )? ')'
        | '[' expression ']'
      )*
    ;

argument_expression_list
    : assignment_expression ( ',' assignment_expression )*
    ;

unary_expression
    : postfix_expression
    | ( '++' | '--' ) unary_expression
    | unary_operator cast_expression
    | 'sizeof' ( %if {. isTypeExpr() .} '(' type_name ')' | unary_expression )
    ;

unary_operator
    : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | '!'
    ;

cast_expression
    : %if {. isTypeExpr() .} '(' type_name ')' cast_expression
		| unary_expression
    ;

multiplicative_expression
    : cast_expression ( ( '*' | '/' | '%' )  cast_expression )*
    ;

additive_expression
    : multiplicative_expression ( ('+' | '-' ) multiplicative_expression )*
    ;

shift_expression
    : additive_expression ( ( '<<' | '>>' ) additive_expression )*
    ;

relational_expression
    : shift_expression ( ( '<' | '>' | '<=' | '>=' ) shift_expression )*
    ;

equality_expression
    : relational_expression ( ( '==' | '!=' ) relational_expression )*
    ;

and_expression
    : equality_expression ( '&' equality_expression )*
    ;

exclusive_or_expression
    : and_expression ( '^' and_expression )*
    ;

inclusive_or_expression
    : exclusive_or_expression ( '|' exclusive_or_expression )*
    ;

logical_and_expression
    : inclusive_or_expression ( '&&' inclusive_or_expression )*
    ;

logical_or_expression
    : logical_and_expression ( '||' logical_and_expression )*
    ;

conditional_expression
    : logical_or_expression
    | logical_or_expression ( '?' expression ':' conditional_expression )?
    ;

assignment_expression
    : conditional_expression
    | unary_expression assignment_operator assignment_expression
    ;

assignment_operator
    : '='
    | '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '&='
    | '^='
    | '|='
    ;

expression
    : assignment_expression ( ',' assignment_expression )*
    ;

constant_expression
    : conditional_expression
    ;

declaration
    : declaration_specifiers ( init_declarator_list )? ';'
    ;

declaration_specifiers
    : ( storage_class_specifier
        | type_specifier
        | type_qualifier
        | function_specifier
		  )+
    ;

init_declarator_list
    : init_declarator ( ',' init_declarator )*
    ;

init_declarator
    : declarator ( '=' initializer )?
    ;

storage_class_specifier
    : 'typedef'
    | 'extern'
    | 'static'
    | 'auto'
    | 'register'
    ;

type_specifier
    : 'void'
    | 'char'
    | 'short'
    | 'int'
    | 'long'
    | 'float'
    | 'double'
    | 'signed'
    | 'unsigned'
    | '_Bool'
    | '_Complex'
    | '_Imaginary'
    | struct_or_union_specifier
    | enum_specifier
    | TYPE_NAME
    ;

struct_or_union_specifier
    : struct_or_union
		  ( IDENTIFIER ( '{' struct_declaration_list '}' )?
        | '{' struct_declaration_list '}'
			)
    ;

struct_or_union
    : 'struct'
    | 'union'
    ;

struct_declaration_list
    : ( struct_declaration )+
    ;

struct_declaration
    : specifier_qualifier_list struct_declarator_list ';'
    ;

specifier_qualifier_list
    : ( type_specifier | type_qualifier )+
    ;

struct_declarator_list
    : struct_declarator ( ',' struct_declarator )*
    ;

struct_declarator
    : declarator ( ':' constant_expression )?
    | ':' constant_expression
    ;

enum_specifier
    : 'enum' ( '{' enumerator_list ( ',' )? '}'
             | IDENTIFIER ( '{' enumerator_list ( ',' )? '}' )?
           )
    ;

enumerator_list
    : enumerator ( ',' enumerator )*
    ;

enumerator
    : IDENTIFIER ( '=' constant_expression )?
    ;

type_qualifier
    : 'const'
    | 'restrict'
    | 'volatile'
    ;

function_specifier
    : 'inline'
    ;

declarator
    : ( pointer )? direct_declarator
    ;


direct_declarator
    : ( IDENTIFIER
      | '(' declarator ')' )
      ( '[' ( '*'
		          | 'static' type_qualifier_list assignment_expression
		          | type_qualifier_list ( '*'
															        | ( 'static' )? assignment_expression )?
							| assignment_expression )? ']'
        | '(' ( parameter_type_list | identifier_list )?')' )*
    ;

pointer
    : ( ( '*' )+ ( type_qualifier_list )? )+
    ;

type_qualifier_list
    : ( type_qualifier )+
    ;


parameter_type_list
    : parameter_list ( ',' '...' )?
    ;

parameter_list
    : parameter_declaration ( ',' parameter_declaration )*
    ;

parameter_declaration
    : declaration_specifiers ( declarator | abstract_declarator )?
    ;

identifier_list
    : IDENTIFIER ( ',' IDENTIFIER )*
    ;

type_name
    : specifier_qualifier_list ( abstract_declarator )?
    ;

abstract_declarator
    : pointer ( direct_abstract_declarator )?
    | direct_abstract_declarator
    ;

direct_abstract_declarator
    : ( '(' ( abstract_declarator | parameter_type_list )? ')'
        | '[' ( assignment_expression | '*' )? ']' )
      ( '[' ( assignment_expression | '*' )? ']'
        | '(' ( parameter_type_list )? ')' )*
    ;

initializer
    : assignment_expression
    | '{' initializer_list ( ',' )? '}'
    ;

initializer_list
    : ( designation )? initializer ( ( designation )? initializer )*
    ;

designation
    : designator_list '='
    ;

designator_list
    : ( designator )+
    ;

designator
    : '[' constant_expression ']'
    | '.' IDENTIFIER
    ;

statement
    : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    ;

labeled_statement
    : IDENTIFIER ':' statement
    | 'case' constant_expression ':' statement
    | 'default' ':' statement
    ;

compound_statement
    : '{' ( block_item_list )? '}'
    ;

block_item_list
    : ( block_item )+
    ;

block_item
    : declaration
    | statement
    ;

expression_statement
    : ( expression )? ';'
    ;

selection_statement
    : 'if' '(' expression ')' statement ( 'else' statement )?
    | 'switch' '(' expression ')' statement
    ;

iteration_statement
    : 'while' '(' expression ')' statement
    | 'do' statement 'while' '(' expression ')' ';'
    | 'for' '(' ( expression_statement expression_statement ( expression )?
                | declaration expression_statement ( expression )?
               ) ')' statement
    ;

jump_statement
    : 'goto' IDENTIFIER ';'
    | 'continue' ';'
    | 'break' ';'
    | 'return' ( expression )? ';'
    ;

translation_unit
    : ( external_declaration )+
    ;

/* Conflict in this rule because both alternatives start with declaration_specifiers */
external_declaration
    : function_definition
    | declaration
    ;

function_definition
    : declaration_specifiers declarator ( declaration_list )? compound_statement
    ;

declaration_list
    : ( declaration )+
    ;