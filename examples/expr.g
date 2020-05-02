%token number
%start Expr
%%
Expr
  : Term ( ( "+" | "-" ) Term )*
  ;

Term
  : Factor ( ( "*" | "/" ) Factor )*
  ;

Factor
  : number
  | "(" Expr ")"
  ;