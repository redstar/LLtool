/*
 * SQL SELECT statement as understood by SQLLite.
 * See syntax diagram at https://www.sqlite.org/lang_select.html
 */
%token ident, string, number
%start select_stmt
%%
// https://www.sqlite.org/syntax/factored-select-stmt.html
select_stmt
  : ( "WITH" ( "RECURSIVE" )? common_table_expression ("," common_table_expression)* )?
    select_core (compound_operator select_core)*
    ("ORDER" "BY" ordering_term ("," ordering_term)*)?
    ("LIMIT" expr  (("OFFSET" | ",") expr)?)?
  ;

// https://www.sqlite.org/syntax/select-core.html
select_core
  : "SELECT" ("DISTINCT" | "ALL") result_column ("," result_column)*
     ("FROM" (table_or_subquery ("," table_or_subquery)* | join_clause))?
     ("WHERE" expr)?
     ("GROUP" "BY" expr ("," expr)* ("HAVING" expr)?)?
     ("WINDOW" window_name "AS" window_defn ("," window_name "AS" window_defn)* )?
    | "VALUES" "(" expr ("," expr)* ")" ("," "(" expr ("," expr)* ")")*
  ;

// https://www.sqlite.org/syntax/common-table-expression.html
common_table_expression
  : table_name ("(" column_name ("," column_name)* ")" )?
    "AS" "(" select_stmt ")"
  ;

// https://www.sqlite.org/syntax/compound-operator.html
compound_operator
  : "UNION"
  | "UNION" "ALL"
  | "INTERSECT"
  | "EXCEPT"
  ;

// https://www.sqlite.org/syntax/table-or-subquery.html
table_or_subquery
  : (schema_name ".")? table_name (("AS")? table_alias)?
    ("INDEXED" "BY" index_name | "NOT" "INDEXED")?
  | (schema_name ".")? table_function_name "(" (expr (";" expr)*)? ")"
    (("AS")? table_alias)?
  | "(" join_clause | table_or_subquery ("," table_or_subquery)* ")"
  | "(" select_stmt ")" (("AS")? table_alias)?
  ;

// https://www.sqlite.org/syntax/window-defn.html
window_defn
  : "(" base_window_name ("PARTITION" "BY" expr ("," expr)* )?
    ("ORDER" "BY" ordering_term ("," ordering_term)*)?
    (frame_spec)? ")"
  ;

// https://www.sqlite.org/syntax/frame-spec.html
frame_spec
  : ("RANGE" | "ROWS" | "GROUPS")
    ("BETWEEN" ("UNBOUNDED" "PRECEDING" | expr ("FOLLOWING" | "PRECEDING") | "CURRENT" "ROW" )
     "AND" ("UNBOUNDED" "FOLLOWING" | expr ("FOLLOWING" | "PRECEDING") | "CURRENT" "ROW" )
    | "UNBOUNDED" "PRECEDING"
    | expr "PRECEDING"
    | "CURRENT" "ROW"
    )
    ("EXCLUDE" ("NO OTHERS" | "CURRENT" "ROW" | "GROUP" | "TIES"))?
  ;

// https://www.sqlite.org/syntax/ordering-term.html
ordering_term
  : expr ("COLLATE" collation_name)? ("ASC" | "DESC")?
    ("NULLS" ("FIRST" | "LAST"))?
  ;

// https://www.sqlite.org/syntax/join-clause.html
join_clause
  : table_or_subquery (join_operator table_or_subquery join_constraint)*
  ;

// https://www.sqlite.org/syntax/join-operator.html
join_operator
  : ","
  | ("NATURAL")? ("LEFT" ("OUTER")? | "INNER" | "CROSS")? "JOIN"
  ;

// https://www.sqlite.org/syntax/join-constraint.html
join_constraint
  : "ON" expr
  | "USING" "(" column_name ("," column_name)* ")"
  ;

// https://www.sqlite.org/syntax/result-column.html
result_column
  : expr (("AS")? column_alias)?
  | "*"
  | table_name "." "*"
  ;

// https://www.sqlite.org/syntax/expr.html
expr
  : literal_value
  | bind_parameter
  | ((schema_name ".")? table_name ".")? column_name
  | unary_operator expr
  | expr binary_operator expr
// NOT FINISHED HERE
  ;

// Did not find the definition for these:
unary_operator : "+" | "-" ;
binary_operator : "+" | "-" | "*" | "/" ;
table_name : ident ;
table_alias : ident ;
table_function_name : ident ;
index_name : ident ;
schema_name : ident ;
column_name : ident ;
column_alias : ident ;
collation_name : ident ;
window_name : ident ;
base_window_name : ident ;
bind_parameter : ident ;
literal_value : string | number;