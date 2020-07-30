//===- KnossosDialect.td - Knossos dialect -----------*- tablegen -*-===//
//
// This file is licensed under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef KNOSSOS_DIALECT
#define KNOSSOS_DIALECT

include "mlir/IR/OpBase.td"

//===----------------------------------------------------------------------===//
// Knossos dialect definition.
//===----------------------------------------------------------------------===//

def KnossosDialect : Dialect {
    let name = "knossos";
    let summary = "MSRC knossos dialect.";
    let description = [{
        This is the MSRC Knossos dialect, for describing algorithms in a
        functional way, allowing rewrite rules to optimise them through
        engines implementing reinforcement learning, heuristic searches,
        constrain/linear programming, monte carlo, greedy and brute force.
    }];
    let cppNamespace = "knossos";
}

//===----------------------------------------------------------------------===//
// Base knossos operation definition.
//===----------------------------------------------------------------------===//

class Knossos_Op<string mnemonic, list<OpTrait> traits = []> :
        Op<KnossosDialect, mnemonic, traits>;

#endif // KNOSSOS_DIALECT
