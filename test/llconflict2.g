// RUN: LLtool -w %s 2>&1 | FileCheck -dump-input-on-failure %s

// See https://en.wikipedia.org/wiki/LL_parser#Conflicts

S : A "a" "b" ;
A : ( "a" )? ;
// CHECK: LL conflict in A: same start and sucessor of deletable element
