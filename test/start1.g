// RUN: LLtool -w %s 2>&1 | FileCheck -dump-input-on-failure %s
%start A
%start S
%%
A : "a" ;
// CHECK: Start symbol is already defined. Ignoring new definition.
