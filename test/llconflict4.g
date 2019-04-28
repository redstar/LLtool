// RUN: LLtool -w %s 2>&1 | FileCheck %s
%token id
%%
S = QualId ";" IdList .
// See http://www.ssw.uni-linz.ac.at/Coco/Doc/UserManual.pdf, page 29.
QualId = (id ".")? id.
// CHECK: LL conflict in QualId: same start and sucessor of deletable element
IdList = id ("," id)* (",")?.
// CHECK: LL conflict in IdList: same start and sucessor of deletable element
