; RUN: ksc-mlir MLIR %s  | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s  | FileCheck %s --check-prefix=LLVM

(edef print Float (Float))
; MLIR: func private @print$af(f64) -> f64
; LLVM: declare double @"print$af"(double %0)

(def fun Integer ((x : Integer) (y : Float))
                 (add x 10))
; MLIR: func private @fun$aif(%arg0: i64, %arg1: f64) -> i64 {
; MLIR-NEXT:  %c10{{.*}} = constant 10 : i64
; MLIR-NEXT:  %[[add:[0-9]+]] = addi %arg0, %c10{{.*}} : i64
; MLIR-NEXT:  return %[[add]] : i64

; LLVM: define i64 @"fun$aif"(i64 %0, double %1) {
; LLVM-NEXT:  [[add:[0-9]+]] = add i64 %0, 10
; LLVM-NEXT:  ret i64 %[[add]]


(def main Integer () (fun 42 -1e38)) ; comment
; MLIR:       func private @main() -> i64 {
; MLIR-NEXT:    %c42{{.*}} = constant 42 : i64
; MLIR-NEXT:    %cst = constant -9.9999999999999997E+37 : f64
; MLIR-NEXT:    %[[fun:[0-9]+]] = call @fun$aif(%c42{{.*}}, %cst) : (i64, f64) -> i64
; MLIR-NEXT:    return %[[fun]] : i64

; LLVM:       define i64 @main() {
; LLVM-NEXT:    %[[fun:[0-9]+]] = call i64 @"fun$aif"(i64 42, double 0xC7D2CED32A16A1B1)
; LLVM-NEXT:    ret i64 %[[fun]]
