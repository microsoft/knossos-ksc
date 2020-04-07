; RUN: ksc-mlir MLIR %s 2>&1 | FileCheck %s --check-prefix=MLIR
; RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM

; MLIR-NOT: error:

; ========================= Arithmetic (Integer/Float)

(def ia Integer ((x : Integer) (y : Integer))
  (let (a (div (mul (add x y) (sub y x)) 10)) a)
)
; MLIR: func @ia(%arg0: i64, %arg1: i64) -> i64 {
; MLIR:   %[[add:[0-9]+]] = addi %arg0, %arg1 : i64
; MLIR:   %[[sub:[0-9]+]] = subi %arg1, %arg0 : i64
; MLIR:   %[[mul:[0-9]+]] = muli %[[add]], %[[sub]] : i64
; MLIR:   %c10{{.*}} = constant 10 : i64
; MLIR:   %[[div:[0-9]+]] = divi_signed %[[mul]], %c10{{.*}} : i64
; MLIR:   return %[[div]] : i64

; LLVM: define i64 @ia(i64 %0, i64 %1) {
; LLVM:   %[[add:[0-9]+]] = add i64 %0, %1
; LLVM:   %[[sub:[0-9]+]] = sub i64 %1, %0
; LLVM:   %[[mul:[0-9]+]] = mul i64 %[[add]], %[[sub]]
; LLVM:   %[[div:[0-9]+]] = sdiv i64 %[[mul]], 10
; LLVM:   ret i64 %[[div]]

(def fa Float ((x : Float) (y : Float))
  (let (b (div (mul (add x y) (sub y x)) 10.0)) b)
)
; MLIR: func @fa(%arg0: f64, %arg1: f64) -> f64 {
; MLIR:   %[[add:[0-9]+]] = addf %arg0, %arg1 : f64
; MLIR:   %[[sub:[0-9]+]] = subf %arg1, %arg0 : f64
; MLIR:   %[[mul:[0-9]+]] = mulf %[[add]], %[[sub]] : f64
; MLIR:   %cst = constant 1.000000e+01 : f64
; MLIR:   %[[div:[0-9]+]] = divf %[[mul]], %cst : f64
; MLIR:   return %[[div]] : f64

; LLVM: define double @fa(double %0, double %1) {
; LLVM:   %[[add:[0-9]+]] = fadd double %0, %1
; LLVM:   %[[sub:[0-9]+]] = fsub double %1, %0
; LLVM:   %[[mul:[0-9]+]] = fmul double %[[add]], %[[sub]]
; LLVM:   %[[div:[0-9]+]] = fdiv double %[[mul]], 1.000000e+01
; LLVM:   ret double %[[div]]

; ========================= Comparison (Integer/Float) / Logic

(def ic Bool ((x : Integer) (y : Integer))
  (let (c (if (and (or (eq x y) (ne y x))
                   (or (lt x y) (gt y x)))
              (gte x y)
              (lte x y)
          ))
        c)
)
; MLIR: func @ic(%arg0: i64, %arg1: i64) -> i1 {
; MLIR:   %[[sge:[0-9]+]] = cmpi "sge", %arg0, %arg1 : i64
; MLIR:   %[[sle:[0-9]+]] = cmpi "sle", %arg0, %arg1 : i64
; MLIR:   %[[eq:[0-9]+]] = cmpi "eq", %arg0, %arg1 : i64
; MLIR:   %[[ne:[0-9]+]] = cmpi "ne", %arg1, %arg0 : i64
; MLIR:   %[[or1:[0-9]+]] = or %[[eq]], %[[ne]] : i1
; MLIR:   %[[slt:[0-9]+]] = cmpi "slt", %arg0, %arg1 : i64
; MLIR:   %[[sgt:[0-9]+]] = cmpi "sgt", %arg1, %arg0 : i64
; MLIR:   %[[or2:[0-9]+]] = or %[[slt]], %[[sgt]] : i1
; MLIR:   %[[and:[0-9]+]] = and %[[or1]], %[[or2]] : i1
; MLIR:   %[[sel:[0-9]+]] = select %[[and]], %[[sge]], %[[sle]] : i1
; MLIR:   return %[[sel]] : i1

; LLVM: define i1 @ic(i64 %0, i64 %1) {
; LLVM:   %[[sge:[0-9]+]] = icmp sge i64 %0, %1
; LLVM:   %[[sle:[0-9]+]] = icmp sle i64 %0, %1
; LLVM:   %[[eq:[0-9]+]] = icmp eq i64 %0, %1
; LLVM:   %[[ne:[0-9]+]] = icmp ne i64 %1, %0
; LLVM:   %[[or1:[0-9]+]] = or i1 %[[eq]], %[[ne]]
; LLVM:   %[[slt:[0-9]+]] = icmp slt i64 %0, %1
; LLVM:   %[[sgt:[0-9]+]] = icmp sgt i64 %1, %0
; LLVM:   %[[or2:[0-9]+]] = or i1 %[[slt]], %[[sgt]]
; LLVM:   %[[and:[0-9]+]] = and i1 %[[or1]], %[[or2]]
; LLVM:   %[[sel:[0-9]+]] = select i1 %[[and]], i1 %[[sge]], i1 %[[sle]]
; LLVM:   ret i1 %[[sel]]

(def fc Bool ((x : Float) (y : Float))
  (let (d (if (and (or (eq x y) (ne y x))
                   (or (lt x y) (gt y x)))
              (gte x y)
              (lte x y)
          ))
        d)
)
; MLIR: func @fc(%arg0: f64, %arg1: f64) -> i1 {
; MLIR:   %[[oge:[0-9]+]] = cmpf "oge", %arg0, %arg1 : f64
; MLIR:   %[[ole:[0-9]+]] = cmpf "ole", %arg0, %arg1 : f64
; MLIR:   %[[oeq:[0-9]+]] = cmpf "oeq", %arg0, %arg1 : f64
; MLIR:   %[[one:[0-9]+]] = cmpf "one", %arg1, %arg0 : f64
; MLIR:   %[[or1:[0-9]+]] = or %[[oeq]], %[[one]] : i1
; MLIR:   %[[olt:[0-9]+]] = cmpf "olt", %arg0, %arg1 : f64
; MLIR:   %[[ogt:[0-9]+]] = cmpf "ogt", %arg1, %arg0 : f64
; MLIR:   %[[or2:[0-9]+]] = or %[[olt]], %[[ogt]] : i1
; MLIR:   %[[and:[0-9]+]] = and %[[or1]], %[[or2]] : i1
; MLIR:   %[[sel:[0-9]+]] = select %[[and]], %[[oge]], %[[ole]] : i1
; MLIR:   return %[[sel]] : i1

; LLVM: define i1 @fc(double %0, double %1) {
; LLVM:   %[[oge:[0-9]+]] = fcmp oge double %0, %1
; LLVM:   %[[ole:[0-9]+]] = fcmp ole double %0, %1
; LLVM:   %[[oeq:[0-9]+]] = fcmp oeq double %0, %1
; LLVM:   %[[one:[0-9]+]] = fcmp one double %1, %0
; LLVM:   %[[or1:[0-9]+]] = or i1 %[[oeq]], %[[one]]
; LLVM:   %[[olt:[0-9]+]] = fcmp olt double %0, %1
; LLVM:   %[[ogt:[0-9]+]] = fcmp ogt double %1, %0
; LLVM:   %[[or2:[0-9]+]] = or i1 %[[olt]], %[[ogt]]
; LLVM:   %[[and:[0-9]+]] = and i1 %[[or1]], %[[or2]]
; LLVM:   %[[sel:[0-9]+]] = select i1 %[[and]], i1 %[[oge]], i1 %[[ole]]
; LLVM:   ret i1 %[[sel]]

; ========================= Unary (Integer/Float) / Conversion

(def fu Float ((x : Integer))
  (let (e (abs (neg (exp (log (to_float x)))))) e)
)
; MLIR: func @fu(%arg0: i64) -> f64 {
; MLIR:   %[[cast:[0-9]+]] = sitofp %arg0 : i64 to f64
; MLIR:   %[[log:[0-9]+]] = log %[[cast]] : f64
; MLIR:   %[[exp:[0-9]+]] = exp %[[log]] : f64
; MLIR:   %[[neg:[0-9]+]] = negf %[[exp]] : f64
; MLIR:   %[[abs:[0-9]+]] = absf %[[neg]] : f64
; MLIR:   return %[[abs]] : f64

; LLVM: define double @fu(i64 %0) {
; LLVM:   %[[cast:[0-9]+]] = sitofp i64 %0 to double
; LLVM:   %[[log:[0-9]+]] = call double @llvm.log.f64(double %[[cast]])
; LLVM:   %[[exp:[0-9]+]] = call double @llvm.exp.f64(double %[[log]])
; LLVM:   %[[neg:[0-9]+]] = fneg double %[[exp]]
; LLVM:   %[[abs:[0-9]+]] = call double @llvm.fabs.f64(double %[[neg]])
; LLVM:   ret double %[[abs]]

; ========================= User function clash

(edef add Bool (Integer Float))
(edef add@ii Integer (Integer Integer))

(def userDef Bool ((arg0 : Integer) (arg1 : Float))
  (and (add arg0 arg1) (eq (add@ii arg0 arg0) 10))
)
; MLIR: func @userDef(%arg0: i64, %arg1: f64) -> i1 {
; MLIR:   %[[add:[0-9]+]] = call @add(%arg0, %arg1) : (i64, f64) -> i1
; MLIR:   %[[call:[0-9]+]] = call @"add@ii"(%arg0, %arg0) : (i64, i64) -> i64
; MLIR:   %c10{{.*}} = constant 10 : i64
; MLIR:   %[[eq:[0-9]+]] = cmpi "eq", %[[call]], %c10{{.*}} : i64
; MLIR:   %[[and:[0-9]+]] = and %[[add]], %[[eq]] : i1
; MLIR:   return %[[and]] : i1


; LLVM: define i1 @userDef(i64 %0, double %1) {
; LLVM:   %[[add:[0-9]+]] = call i1 @add(i64 %0, double %1)
; LLVM:   %[[call:[0-9]+]] = call i64 @"add@ii"(i64 %0, i64 %0)
; LLVM:   %[[eq:[0-9]+]] = icmp eq i64 %[[call]], 10
; LLVM:   %[[and:[0-9]+]] = and i1 %[[add]], %[[eq]]
; LLVM:   ret i1 %[[and]]

