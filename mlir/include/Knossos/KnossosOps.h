//===- KnossosOps.h - Knossos dialect ops -----------------*- C++ -*-===//
//
// This file is licensed under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef KNOSSOS_KNOSSOSOPS_H
#define KNOSSOS_KNOSSOSOPS_H

#include "mlir/IR/Dialect.h"
#include "mlir/IR/OpDefinition.h"
#include "mlir/Interfaces/SideEffects.h"

namespace mlir {
namespace knossos {

#define GET_OP_CLASSES
#include "Knossos/KnossosOps.h.inc"

} // namespace knossos
} // namespace mlir

#endif // KNOSSOS_KNOSSOSOPS_H
