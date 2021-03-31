// mlir-opt -convert-scf-to-std -convert-std-to-llvm -canonicalize mlir/test/mlir/relu3_scf.mlir | build/bin/mlir-cpu-runner -shared-libs=build/lib/libmlir_runner_utils.so -entry-point-result=void

//  def relu3(x: float) -> float
//  if x < 0.0:
//    return 0.0
//  elif x < 1.0:
//    return x ** 3
//  else
//    return x

// here relu3 is outlined into a function to see the border between setting up
// the input / printing to output and the computation itself. In the other examples
// this is inlined.
func @relu3(%arg0: memref<4x4xf32>, %arg1: memref<4x4xf32>) {
  %cf0 = constant 0.0 : f32
  %cf1 = constant 1.0 : f32
  %cf3 = constant 3.0 : f32

  %i0 = constant 0 : index
  %i1 = constant 1 : index
  %i4 = constant 4 : index

  scf.for %i = %i0 to %i4 step %i1 {
    scf.for %j = %i0 to %i4 step %i1 {
      %elem = load %arg0[%i, %j] : memref<4x4xf32>
      %condlt0 = cmpf "ult", %elem, %cf0 : f32
      %res = scf.if %condlt0 -> (f32) {         // if (x < 0)
        scf.yield %cf0 : f32
      } else {
        %condlt1 = cmpf "ult", %elem, %cf1 : f32
        %res = scf.if %condlt1 -> (f32) {       // if (x < 1)
          %res = std.mulf %elem, %cf3 : f32     //   return x * 3 (** not supported by cuda-runner)
          scf.yield %res : f32                  //
        } else {                                //
          scf.yield %elem : f32                 // return x
        }
        scf.yield %res : f32
      }
      store %res, %arg1[%i, %j] : memref<4x4xf32>
    }
  }

  return
}

func @main() {
  %i0 = constant 0 : index
  %i1 = constant 1 : index
  %i2 = constant 2 : index
  %i3 = constant 3 : index

  %cf0 = constant 0.0 : f32
  %cf1 = constant 0.5 : f32
  %cf2 = constant 2.0 : f32
  %cf3 = constant 3.0 : f32
  %cf4 = constant 4.0 : f32

  // prepare input
  %A = alloc() : memref<4x4xf32>

  %increment = constant 0.100000e+00 : f32
  %initVal = alloc() : memref<f32>
  store %cf0, %initVal[] : memref<f32>

  %c0 = constant 0 : index
  %csize = constant 4 : index
  %c1 = constant 1 : index
  scf.for %arg0 = %c0 to %csize step %c1 {
      scf.for %arg1 = %c0 to %csize step %c1 {
          %val_loaded = load %initVal[] : memref<f32>
          store %val_loaded, %A[%arg0, %arg1] : memref<4x4xf32>
          %incremented = addf %val_loaded, %increment : f32
          store %incremented, %initVal[] : memref<f32>
      }
  }

  // initialize output
  %output = alloc() : memref<4x4xf32>

  call @relu3(%A, %output) : (memref<4x4xf32>, memref<4x4xf32>) -> ()

  // print input & output
  %printA = memref_cast %A :  memref<4x4xf32> to memref<*xf32>
  call @print_memref_f32(%printA): (memref<*xf32>) -> ()
  %printOutput = memref_cast %output :  memref<4x4xf32> to memref<*xf32>
  call @print_memref_f32(%printOutput): (memref<*xf32>) -> ()

  return
}
func private @print_memref_f32(memref<*xf32>) attributes { llvm.emit_c_interface }

