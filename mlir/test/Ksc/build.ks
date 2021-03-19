; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

(edef to_float Float (Integer))
; MLIR:   func @to_float$ai(i64) -> f64

; Length of the vector comes from a function argument
; build structure is the same, tested below
(def argLen (Vec Float) (N : Integer)
  (build N (lam (i : Integer) (to_float i))))
; MLIR: func @argLen$ai(%arg0: i64) -> memref<?xf64> {
; MLIR:   index_cast %arg0 : i64 to index
; MLIR:   alloc(%{{[0-9]+}}) : memref<?xf64>

; LLVM: define { double*, double*, i64, [1 x i64], [1 x i64] } @"argLen$ai"(i64 %0) {
; LLVM:   mul i64 %0, ptrtoint (double* getelementptr (double, double* null, i64 1) to i64)
; LLVM:   call i8* @malloc(i64 %{{[0-9]+}})

; Length of the vector comes from an expression
; build structure is the same, tested below
(def copyVec (Vec Float) (v : (Vec Float))
  (build (size v) (lam (i : Integer) (index i v))))
; MLIR: func @copyVec$avf(%arg0: memref<?xf64>) -> memref<?xf64> {
; MLIR:   %c0 = constant 0 : index
; MLIR:   dim %arg0, %c0 : memref<?xf64>
; MLIR:   index_cast %{{[0-9]+}} : i64 to index
; MLIR:   alloc(%{{[0-9]+}}) : memref<?xf64>

; LLVM: define { double*, double*, i64, [1 x i64], [1 x i64] } @"copyVec$avf"(double* %0, double* %1,
; LLVM:   extractvalue { double*, double*, i64, [1 x i64], [1 x i64] } %{{[0-9]+}}, 3, 0
; LLVM:   mul i64 %{{[0-9]+}}, ptrtoint (double* getelementptr (double, double* null, i64 1) to i64)
; LLVM:   call i8* @malloc(i64 %{{[0-9]+}})

; Size direct from a build
(def sizeBuild Integer ((x : Integer) (N : Integer))
  (size (build N (lam (i : Integer) i))))
; MLIR: func @sizeBuild$aii(%arg0: i64, %arg1: i64) -> i64 {
; MLIR:   %c0 = constant 0 : index
; MLIR:   %[[sz:[0-9]+]] = dim %{{.*}}, %c0 : memref<?xi64>
; MLIR:   %[[ret:[0-9]+]] = index_cast %[[sz]] : index to i64
; MLIR:   return %[[ret]] : i64


; LLVM: define i64 @"sizeBuild$aii"(i64 %0, i64 %1) {
; LLVM:   %[[size:[0-9]+]] = extractvalue { i64*, i64*, i64, [1 x i64], [1 x i64] } %{{.*}}, 3, 0
; LLVM:   ret i64 %[[size]]

; Index direct from a build
(def indexBuild Integer ((x : Integer) (N : Integer))
  (index x (build N (lam (i : Integer) i))))
; MLIR: func @indexBuild$aii(%arg0: i64, %arg1: i64) -> i64 {
; MLIR:   %{{.*}} = load %{{.*}}[%{{.*}}] : memref<?xi64>

; LLVM: define i64 @"indexBuild$aii"(i64 %0, i64 %1) {
; LLVM:   br label %[[tailBB:[0-9]+]]
; LLVM:   br label %[[tailBB]]
; LLVM:   %[[gep:[0-9]+]] = getelementptr i64, i64* %{{.*}}, i64 %{{.*}}
; LLVM:   %[[load:[0-9]+]] = load i64, i64* %[[gep]], align 4
; LLVM:   ret i64 %[[load]]

(def main Integer () 
; MLIR: func @main() -> i64 {
; LLVM: define i64 @main() {
  (let (argc 7)
; Creating a 10-element vector of integers, from 0 to 9
     (let (v (build 10 (lam (i : Integer) (add i argc)))) 
        (add (size v) (index 5 v)))
; MLIR:   %c10{{.*}} = constant 10 : i64
; MLIR:   %[[idxV:[0-9]+]] = index_cast %c10{{.*}} : i64 to index
; MLIR:   %[[vec:[0-9]+]] = alloc(%[[idxV]]) : memref<?xi64>
; MLIR:   %[[zero:[ci_0-9]+]] = constant 0 : i64
; MLIR:   %[[ten:[ci_0-9]+]] = constant 10 : i64
; MLIR:   br ^[[headBB:bb[0-9]+]](%c0_i64 : i64)
; MLIR: ^[[headBB]](%[[ivHead:[0-9]+]]: i64):	// 2 preds: ^bb0, ^[[bodyBB:bb[0-9]+]]
; MLIR:   %[[cond:[0-9]+]] = cmpi "slt", %[[ivHead]], %[[ten]] : i64
; MLIR:   cond_br %[[cond]], ^[[bodyBB]](%[[ivHead]] : i64), ^[[tailBB:bb[0-9]+]]
; MLIR: ^[[bodyBB]](%[[ivBody:[0-9]+]]: i64):	// pred: ^[[headBB]]
; MLIR:   %[[zero2:[ci_0-9]+]] = constant 0 : i64
; MLIR:   %[[expr:[0-9]+]] = addi %[[ivBody]], %c7_i64 : i64
; MLIR:   %[[idxW:[0-9]+]] = index_cast %[[ivBody]] : i64 to index
; MLIR:   store %[[expr]], %[[vec]][%[[idxW]]] : memref<?xi64>
; MLIR:   %[[one:[ci_0-9]+]] = constant 1 : i64
; MLIR:   %[[incr:[0-9]+]] = addi %[[ivBody]], %[[one]] : i64
; MLIR:   br ^[[headBB]](%[[incr]] : i64)
; MLIR: ^[[tailBB]]: // pred: ^[[headBB]]
; MLIR-DAG:   %[[cast:[0-9]+]] = memref_cast %[[vec]] : memref<?xi64> to memref<?xi64>
; MLIR-DAG:   %c0 = constant 0 : index
; MLIR-DAG:   %[[dim:[0-9]+]] = dim %[[cast]], %c0 : memref<?xi64>
; MLIR-DAG:   %[[dimcast:[0-9]+]] = index_cast %[[dim]] : index to i64
; MLIR-DAG:   %[[five:[ci_0-9]+]] = constant 5 : i64
; MLIR-DAG:   %[[idxR:[0-9]+]] = index_cast %c5_i64 : i64 to index
; MLIR-DAG:   %[[idx:[0-9]+]] = load %[[cast]][%[[idxR]]] : memref<?xi64>
; MLIR-DAG:   %[[ret:[0-9]+]] = addi %[[dimcast]], %[[idx]] : i64
; MLIR:   return %[[ret]] : i64

; LLVM:   %[[vec:[0-9]+]] = call i8* @malloc(i64 mul (i64 ptrtoint (i64* getelementptr (i64, i64* null, i64 1) to i64), i64 10))
; LLVM:   br label %[[headBB:[0-9]+]]
; LLVM: [[headBB]]:                                                ; preds = %[[bodyBB:[0-9]+]], %0
; LLVM:   %[[headPHI:[0-9]+]] = phi i64 [ %[[incr:[0-9]+]], %[[bodyBB]] ], [ 0, %0 ]
; LLVM:   %[[cond:[0-9]+]] = icmp slt i64 %[[headPHI]], 10
; LLVM:   br i1 %[[cond]], label %[[bodyBB]], label %[[tailBB:[0-9]+]]
; LLVM: [[bodyBB]]:                                               ; preds = %[[headBB]]
; LLVM:   %[[bodyPHI:[0-9]+]] = phi i64 [ %[[headPHI]], %[[headBB]] ]
; LLVM:   %[[expr:[0-9]+]] = add i64 %[[bodyPHI]], 7
; LLVM:   %[[ptrW:[0-9]+]] = getelementptr i64
; LLVM:   store i64 %[[expr]], i64* %[[ptrW]]
; LLVM:   %[[incr]] = add i64 %[[bodyPHI]], 1
; LLVM:   br label %[[headBB]]
; LLVM: [[tailBB]]:                                               ; preds = %[[headBB]]
; LLVM:   %[[ptrR:[0-9]+]] = getelementptr i64, i64* %{{.*}}, i64 5
; LLVM:   %[[idx:[0-9]+]] = load i64, i64* %[[ptrR]]
; LLVM:   %[[ret:[0-9]+]] = add i64 %{{.*}}, %[[idx]]
; LLVM:   ret i64 %[[ret]]

))
