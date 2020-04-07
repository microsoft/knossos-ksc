//===- KnossosOps.cpp - Knossos dialect ops ---------------*- C++ -*-===//
//
// This file is licensed under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Knossos/KnossosOps.h"
#include "Knossos/KnossosDialect.h"
#include "mlir/IR/OpImplementation.h"
#include "mlir/IR/Builders.h"

namespace mlir {
namespace knossos {
#define GET_OP_CLASSES
#include "Knossos/KnossosOps.cpp.inc"
} // namespace knossos
} // namespace mlir
