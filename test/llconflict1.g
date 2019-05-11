// RUN: LLtool -w %s 2>&1 | FileCheck %s

// See https://en.wikipedia.org/wiki/LL_parser#Conflicts

S : E | E "a" ;
E : ( "b" )? ;
// CHECK: LL conflict in S: same start of several alternatives
// CHECK: LL conflict in S: conflicting alternative
