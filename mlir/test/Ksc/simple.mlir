// RUN: ksc-mlir MLIR %s 2>&1
// RUN: ksc-mlir LLVM %s 2>&1 | FileCheck %s --check-prefix=LLVM


module {
  func @print(f64) -> f64
  // LLVM: declare double @print(double %0)

  func @fun(%arg0: i64, %arg1: f64) -> i64 {
    %c10_i64 = constant 10 : i64
    %0 = addi %arg0, %c10_i64 : i64
    return %0 : i64
  }
  // LLVM: define i64 @fun(i64 %0, double %1) {{.*}} {
  // LLVM:   %[[fun:[0-9]+]] = add i64 %0, 10
  // LLVM:   ret i64 %[[fun]]
  // LLVM: }

  func @main() -> i64 {
    %c42_i64 = constant 42 : i64
    %cst = constant 1.000000e+01 : f64
    %0 = call @fun(%c42_i64, %cst) : (i64, f64) -> i64
    return %0 : i64
  }
  // LLVM: define i64 @main() {{.*}} {
  // LLVM:   %[[main:[0-9]+]] = call i64 @fun(i64 42, double 1.000000e+01)
  // LLVM:   ret i64 %[[main]]
  // LLVM: }

}
