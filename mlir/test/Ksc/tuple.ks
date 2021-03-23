; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

; Tuple argument
(edef tfun1 Float (Tuple Integer Float))
; MLIR: func @tfun1$a$dif$b(i64, f64) -> f64
; LLVM: declare double @"tfun1$a$dif$b"(i64 %0, double %1)

; Tuple return
(edef tfun2 (Tuple Integer Float) (Tuple Bool Float))
; MLIR: func @tfun2$a$dbf$b(i1, f64) -> (i64, f64)
; LLVM: declare { i64, double } @"tfun2$a$dbf$b"(i1 %0, double %1)

; Both, with definition
(def tswap (Tuple Float Float) (tup : (Tuple Float Float))
    (tuple (get$2$2 tup) (get$1$2 tup))
)
; MLIR: func @tswap$a$dff$b(%arg0: f64, %arg1: f64) -> (f64, f64) {
; MLIR:   return %arg1, %arg0 : f64, f64
; LLVM: define { double, double } @"tswap$a$dff$b"(double %0, double %1) {
; LLVM:   %[[ins0:[0-9]+]] = insertvalue { double, double } undef, double %1, 0
; LLVM:   %[[ins1:[0-9]+]] = insertvalue { double, double } %[[ins0]], double %0, 1
; LLVM:   ret { double, double } %[[ins1]]

; Build, call round-trip, use
(def tfun3 Float ((i : Float) (j : Float) (k : Float))
    (add i (get$2$2 (tswap (tuple j k))))
)
; MLIR: func @tfun3$afff(%arg0: f64, %arg1: f64, %arg2: f64) -> f64 {
; MLIR:   %[[call:[0-9]+]]:2 = call @tswap$a$dff$b(%arg1, %arg2) : (f64, f64) -> (f64, f64)
; MLIR:   %[[add:[0-9]+]] = addf %arg0, %[[call]]#1 : f64
; MLIR:   return %[[add]] : f64
; LLVM: define double @"tfun3$afff"(double %0, double %1, double %2) {
; LLVM:   %[[call:[0-9]+]] = call { double, double } @"tswap$a$dff$b"(double %1, double %2)
; LLVM:   %[[ext0:[0-9]+]] = extractvalue { double, double } %[[call]], 0
; LLVM:   %[[ext1:[0-9]+]] = extractvalue { double, double } %[[call]], 1
; LLVM:   %[[add:[0-9]+]] = fadd double %0, %[[ext1]]
; LLVM:   ret double %[[add]]

(def amain Integer (argc : Integer) (
; MLIR: func @amain$ai(%arg0: i64) -> i64 {
; LLVM: define i64 @"amain$ai"(i64 %0) {

; Direct get from temp tuple
  (let (a (get$2$2 (tuple 10.0 42)))
; MLIR: %[[ret:[ci_0-9]+]] = constant 42 : i64

; Creating tuple of two elements, extracting each one, and returning the sum
  (let (t (tuple (add argc argc) 10.0 23))
; MLIR: %[[add:[0-9]+]] = addi %arg0, %arg0 : i64
; MLIR: %cst = constant 1.000000e+01 : f64
; MLIR: %c23{{.*}} = constant 23 : i64
; LLVM: %[[add:[0-9]+]] = add i64 %0, %0

        (mul (get$1$3 t) (get$3$3 t))
; MLIR: %[[mul:[0-9]+]] = muli %[[add]], %c23{{.*}} : i64
; MLIR: return %[[mul]] : i64
; LLVM: %[[mul:[0-9]+]] = mul i64 %[[add]], 23
; LLVM: ret i64 %[[mul:[0-9]+]]

  ))
))
