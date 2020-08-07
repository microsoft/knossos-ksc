; RUN: ( ksc-mlir MLIR %s 2>&1 || true) | tr 'ƒ' x | FileCheck %s --check-prefix=MLIR
; TODO: FileCheck doesn't like 8-bit ascii? 

(def ƒ Float () 1.0)
; MLIR: error_badchar.ks:[[# @LINE - 1]]:6: Unhandled character [x], code 198 0xc6
