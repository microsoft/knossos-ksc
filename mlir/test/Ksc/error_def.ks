; RUN: ( ksc-mlir MLIR %s 2>&1 || true) | FileCheck %s --check-prefix=MLIR

(def f f)
; MLIR: ASSERT FAIL
