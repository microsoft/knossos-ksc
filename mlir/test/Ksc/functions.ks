; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

; Definition without declaration
(edef fun Integer (Integer))
; MLIR: func @fun(i64) -> i64
; LLVM: declare i64 @fun(i64 %0)

; Definition with declaration and use
(edef foo Float (Float))
; The AST retains both, MLIR/LLVM deduplicates
; MLIR-NOT: func @foo(f64) -> f64
; LLVM-NOT: declare double @foo(double %0)

(def ten Integer () 10)
; MLIR: func @ten() -> i64 {
; MLIR-NEXT:   %c10{{.*}} = constant 10 : i64
; MLIR-NEXT:   return %c10{{.*}} : i64

; LLVM: define i64 @ten() {
; LLVM-NEXT:   ret i64 10

(def noargcall Integer () (ten))
; MLIR: func @noargcall() -> i64 {
; MLIR-NEXT:   %[[call:[0-9]+]] = call @ten() : () -> i64
; MLIR-NEXT:   return %[[call]] : i64

; LLVM: define i64 @noargcall() {
; LLVM-NEXT:   %[[call:[0-9]+]] = call i64 @ten()
; LLVM-NEXT:   ret i64 %[[call]]

(def foo Float ((x : Float)) x)
; MLIR:       func @foo(%arg0: f64) -> f64 {
; MLIR-NEXT:    return %arg0 : f64
; MLIR-NEXT:  }

; LLVM:       define double @foo(double %0) {
; LLVM-NEXT:    ret double %0
; LLVM-NEXT:  }

; Direct declaration with use
(def bar Integer ((y : Integer)) (add y 40))
; MLIR:       func @bar(%arg0: i64) -> i64 {
; MLIR-NEXT:    %c40{{.*}} = constant 40 : i64
; MLIR-NEXT:    %[[add:[0-9]+]] = addi %arg0, %c40{{.*}} : i64
; MLIR-NEXT:    return %[[add]] : i64
; MLIR-NEXT:  }

; LLVM:       define i64 @bar(i64 %0) {
; LLVM-NEXT:    %[[add:[0-9]+]] = add i64 %0, 40
; LLVM-NEXT:    ret i64 %[[add]]
; LLVM-NEXT:  }

; Single variable can be bare
(def baz Integer (z : Integer) (add z 50))
; MLIR:       func @baz(%arg0: i64) -> i64 {
; MLIR-NEXT:    %c50{{.*}} = constant 50 : i64
; MLIR-NEXT:    %0 = addi %arg0, %c50{{.*}} : i64
; MLIR-NEXT:    return %0 : i64
; MLIR-NEXT:  }

; LLVM:       define i64 @baz(i64 %0) {
; LLVM-NEXT:    %2 = add i64 %0, 50
; LLVM-NEXT:    ret i64 %2
; LLVM-NEXT:  }

; Main, testing calls to functions
(def main Integer () (
; MLIR: func @main() -> i64 {
; LLVM: define i64 @main() {
  (let ((f (foo 10.0))
       (b (bar (fun 30)))) b)
; MLIR: %cst = constant 1.000000e+01 : f64
; MLIR: call @foo(%cst) : (f64) -> f64
; MLIR:  %c30{{.*}} = constant 30 : i64
; MLIR:  %[[fun:[0-9]+]] = call @fun(%c30{{.*}}) : (i64) -> i64
; MLIR:  %[[bar:[0-9]+]] = call @bar(%[[fun]]) : (i64) -> i64
; MLIR: return %[[bar]] : i64

; LLVM: call double @foo(double 1.000000e+01)
; LLVM:  %[[fun:[0-9]+]] = call i64 @fun(i64 30)
; LLVM:  %[[bar:[0-9]+]] = call i64 @bar(i64 %[[fun]])
; LLVM: ret i64 %[[bar]]
))
