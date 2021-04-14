; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

(def fun Integer ((x : Integer) (y : Float))
                 (fun (add x 10) (sub (to_float x) y)))
; MLIR: func private @fun$aif(%arg0: i64, %arg1: f64) -> i64 {
; MLIR-NEXT:  %c10{{.*}} = constant 10 : i64
; MLIR-NEXT:  %[[add:[0-9]+]] = addi %arg0, %c10{{.*}} : i64
; MLIR-NEXT:  %[[tof:[0-9]+]] = sitofp %arg0 : i64 to f64
; MLIR-NEXT:  %[[sub:[0-9]+]] = subf %[[tof]], %arg1 : f64
; MLIR-NEXT:  %[[call:[0-9]+]] = call @fun$aif(%[[add]], %[[sub]]) : (i64, f64) -> i64
; MLIR-NEXT:  return %[[call]] : i64

; LLVM: define i64 @"fun$aif"(i64 %0, double %1) {
; LLVM-NEXT:  %[[add:[0-9]+]] = add i64 %0, 10
; LLVM-NEXT:  %[[tof:[0-9]+]] = sitofp i64 %0 to double
; LLVM-NEXT:  %[[sub:[0-9]+]] = fsub double %[[tof]], %1
; LLVM-NEXT:  %[[call:[0-9]+]] = call i64 @"fun$aif"(i64 %[[add]], double %[[sub]])
; LLVM-NEXT:  ret i64 %[[call]]
