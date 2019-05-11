// RUN: LLtool -w %s 2>&1 | FileCheck %s
%start A
%start S
%%
A : "a" ;
// CHECK: Start symbol is already defined. Ignoring new definition.
