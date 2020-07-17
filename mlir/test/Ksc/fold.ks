; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

(def prod_fold Integer ((v : (Vec Integer)) (closure : Integer))
; MLIR: func @prod_fold(%arg0: memref<?xi64>, %arg1: i64) -> i64 {
; LLVM: define i64 @prod_fold(i64* %0, i64* %1, i64 %2, i64 %3, i64 %4, i64 %5) {

     (fold (lam (acc_x : (Tuple Integer Integer))
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  (mul@ii (add acc x) closure)))
           1
           v
     )
; MLIR:   %c1{{.*}} = constant 1 : i64
; MLIR:   %c0{{.*}} = constant 0 : i64
; MLIR:   %[[DI:[0-9]+]] = dim %arg0, 0 : memref<?xi64>
; MLIR:   %[[dim:[0-9]+]] = index_cast %[[DI]] : index to i64
; MLIR:   %c0{{.*}} = constant 0 : i64
; MLIR:   br ^[[headBB:bb[0-9]+]](%c1{{.*}}, %c0{{.*}} : i64, i64)
; MLIR: ^[[headBB]](%[[hAcc:[0-9]+]]: i64, %[[hIv:[0-9]+]]: i64):	// 2 preds: ^bb0, ^bb3
; MLIR:   %[[cmp:[0-9]+]] = cmpi "slt", %[[hIv]], %[[dim]] : i64
; MLIR:   cond_br %4, ^[[loadBB:bb[0-9]+]](%[[hAcc]], %[[hIv]] : i64, i64), ^[[tailBB:bb[0-9]+]](%[[hAcc]] : i64)
; MLIR: ^[[loadBB]](%[[lAcc:[0-9]+]]: i64, %[[lIv:[0-9]+]]: i64):	// pred: ^bb1
; MLIR:   %[[idx:[0-9]+]] = index_cast %[[lIv]] : i64 to index
; MLIR:   %[[load:[0-9]+]] = load %arg0[%[[idx]]] : memref<?xi64>
; MLIR:   br ^[[bodyBB:bb[0-9]+]](%[[lAcc]], %[[load]], %[[lIv]] : i64, i64, i64)
; MLIR: ^[[bodyBB]](%[[bAcc:[0-9]+]]: i64, %[[bX:[0-9]+]]: i64, %[[bIv:[0-9]+]]: i64):	// pred: ^bb2
; MLIR:   %[[sum:[0-9]+]] = addi %[[bAcc]], %[[bX]] : i64
; MLIR:   %[[mul:[0-9]+]] = muli %[[sum]], %arg1 : i64
; MLIR:   %c1{{.*}} = constant 1 : i64
; MLIR:   %[[newIv:[0-9]+]] = addi %[[bIv]], %c1{{.*}} : i64
; MLIR:   br ^[[headBB]](%[[mul]], %[[newIv]] : i64, i64)
; MLIR: ^[[tailBB]](%[[acc:[0-9]+]]: i64):	// pred: ^bb1
; MLIR:   return %[[acc]] : i64

; LLVM:   %[[dim:[0-9]+]] = extractvalue { i64*, i64*, i64, [1 x i64], [1 x i64] }
; LLVM:   br label %[[headBB:[0-9]+]]
; LLVM: [[headBB]]:                             ; preds = %[[bodyBB:[0-9]+]], %6
; LLVM:   %[[hAcc:[0-9]+]] = phi i64 [ %[[mul:[0-9]+]], %[[bodyBB]] ], [ 1, %6 ]
; LLVM:   %[[hIv:[0-9]+]]  = phi i64 [ %[[add:[0-9]+]], %[[bodyBB]] ], [ 0, %6 ]
; LLVM:   %[[comp:[0-9]+]] = icmp slt i64 %[[hIv:[0-9]+]], %[[dim]]
; LLVM:   br i1 %[[comp]], label %[[loadBB:[0-9]+]], label %[[tailBB:[0-9]+]]
; LLVM: [[loadBB]]:                             ; preds = %[[headBB]]
; LLVM:   %[[lAcc:[0-9]+]] = phi i64 [ %[[hAcc]], %[[headBB]] ]
; LLVM:   %[[lIv:[0-9]+]]  = phi i64 [ %[[hIv]],  %[[headBB]] ]
; LLVM:     extractvalue { i64*, i64*, i64, [1 x i64], [1 x i64] }
; LLVM:     mul i64 %{{.*}}, 1
; LLVM:     add i64 0, %{{.*}}
; LLVM:   %[[ptr:[0-9]+]] = getelementptr i64, i64* %{{.*}}, i64 %{{.*}}
; LLVM:   %[[load:[0-9]+]] = load i64, i64* %[[ptr]]
; LLVM:   br label %[[bodyBB]]
; LLVM: [[bodyBB]]:                             ; preds = %[[loadBB]]
; LLVM:   %[[bAcc:[0-9]+]] = phi i64 [ %[[lAcc]], %[[loadBB]] ]
; LLVM:   %[[bX:[0-9]+]]   = phi i64 [ %[[load]], %[[loadBB]] ]
; LLVM:   %[[bIv:[0-9]+]]  = phi i64 [ %[[lIv]],  %[[loadBB]] ]
; LLVM:   %[[sum:[0-9]+]] = add i64 %[[bAcc]], %[[bX]]
; LLVM:   %[[mul:[0-9]+]] = mul i64 %[[sum]], %5
; LLVM:   %[[newIv:[0-9]+]] = add i64 %[[bIv]], 1
; LLVM:   br label %[[headBB]]
; LLVM: [[tailBB]]:                              ; preds = %[[headBB]]
; LLVM:   %[[acc:[0-9]+]] = phi i64 [ %[[hAcc]], %[[headBB]] ]
; LLVM:   ret i64 %[[acc]]

)

; Builds a vector of 10 elements where v[i] = i+i
; Accumulates all elements: acc *= v[i] * 2
(def main Integer ()
; MLIR: func @main() -> i64 {
; LLVM: define i64 @main() {

  (let ((vec (build 10 (lam (i : Integer) (add i i))))
        (m   (prod_fold vec 2)))
    m)

; We are not interested in the build construct, just the call and return
; MLIR:  %c10{{.*}} = constant 10 : i64
; MLIR:  %[[idxV:[0-9]+]] = index_cast %c10{{.*}} : i64 to index
; MLIR:  %[[vec:[0-9]+]] = alloc(%[[idxV]]) : memref<?xi64>
; ... build ...
; MLIR:  %[[cast:[0-9]+]] = memref_cast %[[vec]] : memref<?xi64> to memref<?xi64>
; MLIR:  %c2{{.*}} = constant 2 : i64
; MLIR:  %[[ret:[0-9]+]] = call @prod_fold(%[[cast]], %c2{{.*}}) : (memref<?xi64>, i64) -> i64
; MLIR:  return %[[ret]] : i64

; LLVM converts memref<?xi64> into a bunch of insertvalue / extractvalue
; LLVM: %[[vec:[0-9]+]] = call i8* @malloc
; LLVM: insertvalue
; ... build ...
; LLVM: extractvalue
; LLVM: %[[ret:[0-9]+]] = call i64 @prod_fold(
; LLVM: ret i64 %[[ret]]

)
