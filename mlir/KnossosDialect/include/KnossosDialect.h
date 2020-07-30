//===- KnossosDialect.h - Knossos dialect -----------------*- C++ -*-===//
//
// This file is licensed under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef KNOSSOS_KNOSSOSDIALECT_H
#define KNOSSOS_KNOSSOSDIALECT_H

#include "mlir/IR/Dialect.h"
#include "mlir/IR/StandardTypes.h"

namespace mlir {
namespace knossos {

#include "Knossos/KnossosOpsDialect.h.inc"

} // namespace knossos
} // namespace mlir

#endif // KNOSSOS_KNOSSOSDIALECT_H
