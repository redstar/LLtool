// RUN: LLtool -w %s 2>&1 | FileCheck %s

// Check for LL(1) conflict due to left derivation

StatementList = StatementList Statement
// CHECK: LL conflict in StatementList: same start of several alternatives
                |
// CHECK: LL conflict in StatementList: conflicting alternative
                .
Statement = ("IF" | "WHILE") ";" .
