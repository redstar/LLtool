/* Grammar for LLtool */
%token identifier, code, argument, string
%token "%token" = KW_token, "%start" = KW_start
%start lltool
%%
lltool
  = ( header )? ( rule )+ .

header
  = ("%start" identifier                (. builder.startSymbol(tok.pos, tok.val); .)
     | "%token" tokenlist
     | "%eoi" identifier                (. builder.eoiSymbol(tok.pos, tok.val); .)
    )*
    "%%"
  .

tokenlist
  = tokendecl ("," tokendecl )*
  .

tokendecl
  =                                     (. size_t pos; string val, ext; .)
    (identifier                         (. pos = tok.pos; val = tok.val; .)
     | string                           (. pos = tok.pos; val = tok.val; .)
    )
    ( "=" identifier                    (. ext = tok.val; .)
    )?
                                        (. builder.terminal(pos, val, ext); .)
  .

rule
  =                                     (. Node node; .)
    nonterminal<node> "="
    rhs<node.link>                      (. node.link.back = node; .)
    "."
  .

nonterminal<out Node node>
  = identifier                          (. node = builder.nonterminal(tok.pos, tok.val); .)
    ( argument                          (. node.formalArgs = tok.val; .)
    )?
    ( code
    )?
  .

rhs<out Node node>
  = sequence<node>
    (                                   (. node = builder.alternative(node.pos, node);
                                           auto alt = node.link; alt.back = node; .)
      ( "|" sequence<alt.link>          (. alt = alt.link; alt.back = node; .)
      )+
    )?
  .

sequence<out Node node>
  =                                     (. Node last; node = builder.sequence(tok.pos); .)
    (                                   (. Node n; .)
      ( group<n>
      | identifier                      (. n = builder.symbol(tok.pos, tok.val); .)
        ( argument                      (. n.actualArgs = tok.val; .)
        )?
      | string                          (. n = builder.symbol(tok.pos, tok.val, true); .)
      | code                            (. n = builder.code(tok.pos, tok.val); .)
      | "%if" code                      (. n = builder.code(tok.pos, tok.val); n.isResolver = true; .)
      )
                                        (. if (last is null) node.inner = last = n;
                                           else last.next = n, last = n; .)
    )*
                                        (. if (last !is null) last.back = node; .)
  .

group<out Node node>
  = "("                                 (. node = builder.group(tok.pos, Cardinality.One); .)
    rhs<node.link>                      (. node.link.back = node; .)
    ( ")"
      | ")?"                            (. node.cardinality = Cardinality.ZeroOrOne; .)
      | ")*"                            (. node.cardinality = Cardinality.ZeroOrMore; .)
      | ")+"                            (. node.cardinality = Cardinality.OneOrMore; .)
    )
  .