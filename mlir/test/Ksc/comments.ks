; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

; This file should only have the main function returning 42.
; Everything else are comments of various types.

; Single line comment

#| single line multi-line comment |#

#| single #| line #| nested |# multi-line |# comment |#

#|
   multi line multi-line comment
|#

#| multi
  #| line
    #| nested
    |# multi-line
  |# comment
|#

(def main Integer #| inline multi-line comment |# () 42) ; Mid-line comment
; MLIR:       func @main() -> i64 {
; MLIR-NEXT:    %c42{{.*}} = constant 42 : i64
; MLIR-NEXT:    return %c42{{.*}} : i64
; MLIR-NEXT:  }

; LLVM:       define i64 @main() {
; LLVM-NEXT:    ret i64 42
; LLVM-NEXT:  }
