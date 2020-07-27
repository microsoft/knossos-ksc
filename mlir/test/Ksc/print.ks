; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

; Print does not work for now, it only lowers the non-string expressions and
; does not print at all. This is a TODO item on the compiler, but MLIR support
; for strings is not trivial, and we do not need it yet, so we blisfully ignore.

(def main Integer ((argc : Integer) (argv : Vec Integer))
  (print "Hello world"
      10.0
      42
      (add argc (index 1 argv)))
)
; MLIR: func @main(%arg0: i64, %arg1: memref<?xi64>) -> i64 {
;         Strings are ignored, for now
; MLIR:   %cst = constant 1.000000e+01 : f64
; MLIR:   %c42{{.*}} = constant 42 : i64
; MLIR:   %c1{{.*}} = constant 1 : i64
; MLIR:   %[[idx:[0-9]+]] = index_cast %c1{{.*}} : i64 to index
; MLIR:   %[[load:[0-9]+]] = load %arg1[%[[idx]]] : memref<?xi64>
; MLIR:   %[[add:[0-9]+]] = addi %arg0, %[[load]] : i64
; MLIR:   %c4{{.*}} = constant 4 : i64
; MLIR:   return %c4{{.*}} : i64

; LLVM: define i64 @main(i64 %0, i64* %1,
; LLVM:   %[[gep:[0-9]+]] = getelementptr i64, i64* %{{.*}}, i64 1
; LLVM:   %[[load:[0-9]+]] = load i64, i64* %[[gep]]
; LLVM:   %[[add:[0-9]+]] = add i64 %0, %[[load]]
; LLVM:   ret i64 4
