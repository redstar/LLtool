// RUN: LLtool -w %s 2>&1 | FileCheck -dump-input-on-failure %s
%token a, b, c, d
%%
A : (a | B C d) ;
// CHECK: LL conflict in A: same start of several alternatives
// CHECK: LL conflict in A: conflicting alternative
B : (b)? a ;
C : c (d)* ;
// CHECK: LL conflict in C: same start and sucessor of deletable element
