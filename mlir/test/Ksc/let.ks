; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

; Forward declaration, for use below
(edef fun Integer (Integer))
; MLIR: func @fun(i64) -> i64
; LLVM: declare i64 @fun(i64 %0)

; Just defines the variable, returns void
(def fun1 Integer () (
; MLIR: func @fun1() -> i64 {
; LLVM: define i64 @fun1() {
  (let (variable 123.456) 10)
; MLIR: %cst = constant 1.234560e+02 : f64
; MLIR: %c10{{.*}} = constant 10 : i64 
; MLIR: return %c10{{.*}} : i64

; LLVM: ret i64 10
))

; Return the value of x
(def fun2 Integer () (
; MLIR: func @fun2() -> i64 {
; LLVM: define i64 @fun2() {
  (let (x 10) x)
; MLIR: %c10{{.*}} = constant 10 : i64
; MLIR: return %c10{{.*}}

; LLVM: ret i64 10
))

; Call an operation with y, return the value
(def fun3 Integer (a : Integer) (
; MLIR: func @fun3(%arg0: i64) -> i64 {
; LLVM: define i64 @fun3(i64 %0) {
  (let (y 20) (add@ii y a))
; MLIR: %c20{{.*}} = constant 20 : i64
; MLIR: %[[ret:[0-9]+]] = addi %c20{{.*}}, %arg0 : i64
; MLIR: return %[[ret]] : i64

; LLVM: %[[ret:[0-9]+]] = add i64 20, %0
; LLVM: ret i64 %[[ret]]
))

; Return the value of z, expanded from a function call
(def fun4 Integer () (
; MLIR: func @fun4() -> i64 {
; LLVM: define i64 @fun4() {
  (let (z (fun3 10)) z)
; MLIR generation creates SSA value if not constant
; MLIR: %c10{{.*}} = constant 10 : i64
; MLIR: %[[ret:[0-9]+]] = call @fun3(%c10{{.*}}) : (i64) -> i64
; MLIR: return %[[ret]] : i64

; LLVM: %[[ret:[0-9]+]] = call i64 @fun3(i64 10)
; LLVM: ret i64 %[[ret]]
))

; Nested lets
(def fun5 Integer (b : Integer) (
; MLIR: func @fun5(%arg0: i64) -> i64 {
; LLVM: define i64 @fun5(i64 %0) {
  (let (l1 (mul@ii b b))
    (let (l2 (add@ii b l1))
      (mul@ii l1 l2)))
; MLIR: %[[mul:[0-9]+]] = muli %arg0, %arg0 : i64
; MLIR: %[[add:[0-9]+]] = addi %arg0, %[[mul]] : i64
; MLIR: %[[ret:[0-9]+]] = muli %[[mul]], %[[add]] : i64
; MLIR: return %[[ret]] : i64

; LLVM: %[[mul:[0-9]+]] = mul i64 %0, %0
; LLVM: %[[add:[0-9]+]] = add i64 %0, %[[mul]]
; LLVM: %[[ret:[0-9]+]] = mul i64 %[[mul]], %[[add]]
; LLVM: ret i64 %[[ret]]
))

; Multiple bind lets
(def fun6 Integer (argc : Integer) (
; MLIR: func @fun6(%arg0: i64) -> i64 {
; LLVM: define i64 @fun6(i64 %0) {
  (let ((i argc) (j 20) (k 30)) (add@ii (mul@ii i j) k))
; MLIR: %c20{{.*}} = constant 20 : i64
; MLIR: %c30{{.*}} = constant 30 : i64
; MLIR: %[[ij:[0-9]+]] = muli %arg0, %c20{{.*}} : i64
; MLIR: %[[ret:[0-9]+]] = addi %[[ij]], %c30{{.*}} : i64
; MLIR: return %[[ret]] : i64

; LLVM: %[[ij:[0-9]+]] = mul i64 %0, 20
; LLVM: %[[ret:[0-9]+]] = add i64 %[[ij]], 30
; LLVM: ret i64 %[[ret]]
))
