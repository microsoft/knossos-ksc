; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

(edef foo Integer (Integer))
(edef bar Integer (Integer))

(def main Integer () (
; MLIR: func @main() -> i64 {
; LLVM: define i64 @main() {

  (let (
; All literals, LLVM does not support, so we only lower the right block
        (a (if true 10 20))
; MLIR:    %c10{{.*}} = constant 10 : i64
; LLVM optimises constants away

        (b (if false 10 20))
; MLIR:    %c20{{.*}} = constant 20 : i64
; LLVM optimises constants away

; All constant expressions
        (c (if (eq 10 20) (foo 30) (bar 40)))
; MLIR-DAG: %c30{{.*}} = constant 30 : i64
; MLIR-DAG: %[[foo1:[0-9]+]] = call @foo(%c30{{.*}}) : (i64) -> i64
; MLIR-DAG: %c40{{.*}} = constant 40 : i64
; MLIR-DAG: %[[bar1:[0-9]+]] = call @bar(%c40{{.*}}) : (i64) -> i64
; MLIR-DAG: %c10{{.*}} = constant 10 : i64
; MLIR-DAG: %c20{{.*}} = constant 20 : i64
; MLIR-DAG: %[[eq1:[0-9]+]] = cmpi "eq", %c10{{.*}}, %c20{{.*}} : i64
; MLIR: %[[sel1:[0-9]+]] = select %[[eq1]], %[[foo1]], %[[bar1]] : i64

; LLVM: %[[foo1:[0-9]+]] = call i64 @foo(i64 30)
; LLVM: %[[bar1:[0-9]+]] = call i64 @bar(i64 40)
; LLVM: %[[sel1:[0-9]+]] = select i1 false, i64 %[[foo1]], i64 %[[bar1]]


; Inside let, with variables
        (d (let (x (foo 50)) (if (eq x 60) (foo 70) (bar 80))))
; MLIR: %c50{{.*}} = constant 50 : i64
; MLIR: %[[foo_cond:[0-9]+]] = call @foo(%c50{{.*}}) : (i64) -> i64
; MLIR: %c70{{.*}} = constant 70 : i64
; MLIR: %[[foo2:[0-9]+]] = call @foo(%c70{{.*}}) : (i64) -> i64
; MLIR: %c80{{.*}} = constant 80 : i64
; MLIR: %[[bar2:[0-9]+]] = call @bar(%c80{{.*}}) : (i64) -> i64
; MLIR: %c60{{.*}} = constant 60 : i64
; MLIR: %[[eq2:[0-9]+]] = cmpi "eq", %[[foo_cond]], %c60{{.*}} : i64
; MLIR: %[[sel2:[0-9]+]] = select %[[eq2]], %[[foo2]], %[[bar2]] : i64

; LLVM: %[[foo_cond:[0-9]+]] = call i64 @foo(i64 50)
; LLVM: %[[foo2:[0-9]+]] = call i64 @foo(i64 70)
; LLVM: %[[bar2:[0-9]+]] = call i64 @bar(i64 80)
; LLVM: %[[eq2:[0-9]+]] = icmp eq i64 %[[foo_cond]], 60
; LLVM: %[[sel2:[0-9]+]] = select i1 %[[eq2]], i64 %[[foo2]], i64 %[[bar2]]

      ) d)
; MLIR: return %[[sel2]] : i64
; LLVM: ret i64 %[[sel2]]
))
