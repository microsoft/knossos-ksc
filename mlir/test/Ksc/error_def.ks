; RUN: ( ksc-mlir MLIR %s 2>&1 || true) | FileCheck %s --check-prefix=MLIR

(def f f)
; MLIR: error_def.ks:[[# @LINE - 1]]:2: error: Expect def to have 4 parts
