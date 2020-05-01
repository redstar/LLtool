// RUN: LLtool -w %s 2>&1 | FileCheck -dump-input-on-failure %s
%start S
%%
A : "a" ;
// CHECK: Start symbol S not found.
