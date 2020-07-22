// This is a tesbed for the Knossos dialect, very much work-in-progress
// The dialect has been deprecated, these examples are not used in the program
// RUN: ksc-mlir MLIR %s 2>&1

module {
  // Declare a function
  func @add(f64, f64) -> f64

  // Define a function
  func @add10(%arg0: f64) -> f64 {
    %cst = constant 1.000000e+01 : f64
    %0 = addf %arg0, %cst : f64
    return %0 : f64
  }

  // Build lambda
  func @build0(%arg0: i64) -> f64 {
    %cst = constant 1.000000e+01 : f64
    return %cst : f64
  }

  // Fold init
  func @init0() -> f64 {
    %cst = constant 1.000000e+01 : f64
    return %cst : f64
  }

  // Fold lambda
  func @fold0(%arg0: tensor<?xf64>) -> f64 {
    %cst = constant 1.000000e+01 : f64
    return %cst : f64
  }

  // Condition
  func @cond0() -> i1 {
    %true = constant 1 : i1
    return %true : i1
  }

  // True branch
  func @true0() -> f64 {
    %cst = constant 1.000000e+01 : f64
    return %cst : f64
  }

  // False branch
  func @false0() -> f64 {
    %cst = constant 0.000000e+01 : f64
    return %cst : f64
  }

  // Constructs tests
  func @main() -> i64 {
    %c42_i64 = constant 42 : i64

    // Call a function
    %cst = constant 2.000000e+01 : f64
    %0 = call @add10(%cst) : (f64) -> f64

    // Tuple (from constants and expressions)
    %t0 = constant 42 : i64
    %t1 = constant 2.000000e+01 : f64
    %t2 = constant 1 : i1
    %tup = knossos.tuple %t0, %t1, %t2, %0 : (i64, f64, i1, f64) -> tuple<i64, f64, i1, f64>

    // Get (starts at one)
    %tidx = constant 2 : i64
    %snd = knossos.get %tidx, %tup : (i64, tuple<i64, f64, i1, f64>) -> f64

    // Build vector
    %N = constant 8 : i64
    %vec = knossos.build @build0(%N) : (i64) -> (tensor<?xf64>)

    // Get an element of a vector
    %vidx = constant 4 : i64
    %elm = knossos.index %vidx, %vec : (i64, tensor<?xf64>) -> f64

    // Get the size of a vector
    %len = knossos.size %vec : (tensor<?xf64>) -> i64

    // Reduce vector to a scalar
    %sum = knossos.fold @init0(), @fold0(%vec) : (tensor<?xf64>) -> f64

    // let
    //%var1 = knossos.let {
    //  %x = constant 2.000000e+01 : f64
    //  %res = call @add10(%x) : (f64) -> f64
    //  return %res
    //}
    //%var2 = knossos.let {
    //  %x = constant 2.000000e+01 : f64
    //  %res = knossos.let {
    //    %y = constant 3.000000e+01 : f64 
    //    %res = call @add(%x, %y) : (f64,f64) -> f64
    //    return %res
    //  }
    //  return %res
    //}

    // Conditional, for now, calls on functions
    %value = knossos.if @cond0(), @true0(), @false0() : () -> f64

    return %c42_i64 : i64
  }
}
