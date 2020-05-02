/*
 * SQL SELECT statement as understood by SQLLite.
 * See syntax diagram at https://www.sqlite.org/lang_select.html
 */
%%
select_stmt
  = ( "WITH" ( "RECURSIVE" )? common_table_expression ("," common_table_expression)* )?
    fullselect ( compound_operator fullselect )*


fullselect
  = "SELECT" ( "DISTINCT" | "ALL" )? result_column ("," result_column)
    ( "FROM" ( table_or_subquery ( "," table_or_subquery )* | join_clause ) )?
    ( "WHERE" expr )?
    ( "GROUP" "BY" expr ( "," expr )* ("HAVING" expr)? )?
    ( "WINDOW" window_name "AS" window_defn ( "," window_name "AS" window_defn )* )?