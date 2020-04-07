; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

(edef fun Bool (Integer Float))
; MLIR: func @fun(i64, f64) -> i1
; LLVM: declare i1 @fun(i64 %0, double %1)

(edef fun2 (Vec Float) (Vec Float))
; MLIR:     func @fun2(memref<?xf64>) -> memref<?xf64>
; LLVM:     declare { double*, double*, i64, [1 x i64], [1 x i64] } @fun2(double* %0, double* %1, i64 %2, i64 %3, i64 %4)

(def fun@ii Integer ((ai : Integer) (bi : Integer) (ci : Integer)) (
  (add@ii (mul@ii ai bi) ci)
))
; MLIR:       func @"fun@ii"(%arg0: i64, %arg1: i64, %arg2: i64) -> i64 {
; MLIR-NEXT:    %[[mul:[0-9]+]] = muli %arg0, %arg1 : i64
; MLIR-NEXT:    %[[add:[0-9]+]] = addi %[[mul]], %arg2 : i64
; MLIR-NEXT:    return %[[add]] : i64
; MLIR-NEXT:  }

; LLVM:       define i64 @"fun@ii"(i64 %0, i64 %1, i64 %2) {
; LLVM-NEXT:    %[[mul:[0-9]+]] = mul i64 %0, %1
; LLVM-NEXT:    %[[add:[0-9]+]] = add i64 %[[mul]], %2
; LLVM-NEXT:    ret i64 %[[add]]
; LLVM-NEXT:  }

(def fun@ff Float ((af : Float) (bf : Float) (cf : Float)) (
  (add@ff (mul@ff af bf) cf)
))
; MLIR:       func @"fun@ff"(%arg0: f64, %arg1: f64, %arg2: f64) -> f64 {
; MLIR-NEXT:    %[[mul:[0-9]+]] = mulf %arg0, %arg1 : f64
; MLIR-NEXT:    %[[add:[0-9]+]] = addf %[[mul]], %arg2 : f64
; MLIR-NEXT:    return %[[add]] : f64
; MLIR-NEXT:  }

; LLVM:       define double @"fun@ff"(double %0, double %1, double %2) {
; LLVM-NEXT:    %[[mul:[0-9]+]] = fmul double %0, %1
; LLVM-NEXT:    %[[add:[0-9]+]] = fadd double %[[mul]], %2
; LLVM-NEXT:    ret double %[[add]]
; LLVM-NEXT:  }

(def main Integer () (
; MLIR:      func @main() -> i64 {
; LLVM:      define i64 @main() {

  (let ((a (fun@ff 10.0 20.0 30.0))
; MLIR-NEXT:  %cst = constant 1.000000e+01 : f64
; MLIR-NEXT:  %cst_0 = constant 2.000000e+01 : f64
; MLIR-NEXT:  %cst_1 = constant 3.000000e+01 : f64
; MLIR-NEXT:  %[[func:[0-9]+]] = call @"fun@ff"(%cst, %cst_0, %cst_1) : (f64, f64, f64) -> f64

; LLVM-NEXT:  %[[func:[0-9]+]] = call double @"fun@ff"(double 1.000000e+01, double 2.000000e+01, double 3.000000e+01)

        (b (fun@ii 10 20 30)))
; MLIR-NEXT:  %c10{{.*}} = constant 10 : i64
; MLIR-NEXT:  %c20{{.*}} = constant 20 : i64
; MLIR-NEXT:  %c30{{.*}} = constant 30 : i64
; MLIR-NEXT:  %[[func:[0-9]+]] = call @"fun@ii"(%c10{{.*}}, %c20{{.*}}, %c30{{.*}}) : (i64, i64, i64) -> i64

; LLVM-NEXT:  %[[func:[0-9]+]] = call i64 @"fun@ii"(i64 10, i64 20, i64 30)
  b
))
; AST does not return anything
; MLIR: return %[[func]] : i64
; LLVM: ret i64 %[[func]]
