; RUN: ( ksc-mlir MLIR %s 2>&1 || true) | FileCheck %s --check-prefix=MLIR

(def f Floatx () 2)
; MLIR: error_ret.ks:[[# @LINE - 1]]:1: error: Unknown return type [Floatx]
