; RUN: ( ksc-mlir MLIR %s 2>&1 || true) | FileCheck %s --check-prefix=MLIR

(def f Float () 2)
; MLIR: error_type.ks:[[# @LINE - 1]]:17: error: Return type declared as [Float], but body has type [Integer]
