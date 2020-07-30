//===- KnossosDialect.cpp - Knossos dialect ---------------*- C++ -*-===//
//
// This file is licensed under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Knossos/KnossosDialect.h"
#include "Knossos/KnossosOps.h"

using namespace mlir;
using namespace mlir::knossos;

//===----------------------------------------------------------------------===//
// Knossos dialect.
//===----------------------------------------------------------------------===//

KnossosDialect::KnossosDialect(mlir::MLIRContext *context)
    : Dialect(getDialectNamespace(), context) {
  addOperations<
#define GET_OP_LIST
#include "Knossos/KnossosOps.cpp.inc"
      >();
}
