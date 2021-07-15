// mlir-opt -lower-affine -convert-scf-to-std -convert-std-to-llvm -canonicalize mlir/test/mlir/relu3_scf.mlir | build/bin/mlir-cpu-runner -shared-libs=build/lib/libmlir_runner_utils.so -entry-point-result=void

//  def relu3(x: float) -> float
//  if x < 0.0:
//    return 0.0
//  elif x < 1.0:
//    return x ** 3
//  else
//    return x

func @main() {
  %i0 = constant 0 : index
  %i1 = constant 1 : index
  %i2 = constant 2 : index
  %i3 = constant 3 : index

  %cfm2 = constant -0.2 : f32
  %cf0 = constant 0.0 : f32
  %cf1_3 = constant 0.3333333333 : f32
  %cf2_3 = constant 0.6666666667 : f32
  %cf1 = constant 1.0 : f32

  // prepare input
  %A = alloc() : memref<4x4xf32>

  %increment = constant 0.100000e+00 : f32
  %initVal = alloc() : memref<f32>
  store %cfm2, %initVal[] : memref<f32>

  %csize = constant 4 : index
  scf.for %arg0 = %i0 to %csize step %i1 {
      scf.for %arg1 = %i0 to %csize step %i1 {
          %val_loaded = load %initVal[] : memref<f32>
          store %val_loaded, %A[%arg0, %arg1] : memref<4x4xf32>
          %incremented = addf %val_loaded, %increment : f32
          store %incremented, %initVal[] : memref<f32>
      }
  }

  // initialize output
  %output = alloc() : memref<4x4xf32>

  // inlined for now
  affine.for %i = 0 to 4 {
    affine.for %j = 0 to 4 {
      %elem = affine.load %A[%i, %j] : memref<4x4xf32>
      %condlt0 = cmpf "ult", %elem, %cf0 : f32
      %res = scf.if %condlt0 -> (f32) {         // if (elem < 0)
        scf.yield %cf0 : f32
      } else {
        %condlt1 = cmpf "ult", %elem, %cf1 : f32
        %res = scf.if %condlt1 -> (f32) {       // if (elem < 1)
          %x1 = std.mulf %elem, %elem : f32
          %x2 = std.mulf %x1, %elem : f32
          %res = std.mulf %cf1_3, %x2 : f32
          scf.yield %res : f32
        } else {                                // else
          %res = std.subf %elem, %cf2_3 : f32 
          scf.yield %res : f32
        }
        scf.yield %res : f32
      }
      affine.store %res, %output[%i, %j] : memref<4x4xf32>
    }
  }

  // print input & output
  %printA = memref_cast %A :  memref<4x4xf32> to memref<*xf32>
  call @print_memref_f32(%printA): (memref<*xf32>) -> ()
  %printOutput = memref_cast %output :  memref<4x4xf32> to memref<*xf32>
  call @print_memref_f32(%printOutput): (memref<*xf32>) -> ()

  return
}
func private @print_memref_f32(memref<*xf32>) attributes { llvm.emit_c_interface }

