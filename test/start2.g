// RUN: LLtool -w %s 2>&1 | FileCheck %s
%start S
%%
A : "a" ;
// CHECK: Start symbol S not found.
