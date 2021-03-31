// mlir-opt -convert-parallel-loops-to-gpu

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
  %i3 = constant 3 : index

  %cf0 = constant 0.0 : f32
  %cf1 = constant 1.0 : f32
  %cf3 = constant 3.0 : f32

  %c0 = constant 0 : index
  %c1 = constant 1 : index
  %c3 = constant 3 : index
  %c4 = constant 4 : index

  // prepare input
  %A = alloc() : memref<4x4xf32>
  %increment = constant 0.100000e+00 : f32
  %initVal = alloc() : memref<f32>
  store %cf0, %initVal[] : memref<f32>
  %csize = constant 4 : index

  // Filling the input array %A with values starting at 0.0, each increasing by 0.1
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

  // actual relu3
  scf.parallel (%i, %j) = (%c0, %c0) to (%c3, %c3) step (%c1, %c1) {
    %elem = load %A[%i, %j] : memref<4x4xf32>
    %condlt0 = cmpf "ult", %elem, %cf0 : f32
    %res = scf.if %condlt0 -> (f32) {         // if (x < 0)
      scf.yield %cf0 : f32                    //   return 0.0
    } else {
      %condlt1 = cmpf "ult", %elem, %cf1 : f32
      %res = scf.if %condlt1 -> (f32) {       // if (x < 1)
        %res = std.mulf %elem, %cf3 : f32     //
        scf.yield %res : f32                  //   return x * 3 (** not supported by cuda-runner)
      } else {                                //
        scf.yield %elem : f32                 // return x
      }
      scf.yield %res : f32
    }
    store %res, %output[%i, %j] : memref<4x4xf32>
    // here we could map the parallel dimensions to hardware dimensions. For now 1 to 1 mapping
  } { mapping = [{processor = 1, map = affine_map<(d0) -> (d0)>, bound = affine_map<(d0) -> (d0)>}, {processor = 0, map = affine_map<(d0) -> (d0)>, bound = affine_map<(d0) -> (d0)>}] }

  // print input & output
  %printA = memref_cast %A :  memref<4x4xf32> to memref<*xf32>
  call @print_memref_f32(%printA): (memref<*xf32>) -> ()
  %printOutput = memref_cast %output :  memref<4x4xf32> to memref<*xf32>
  call @print_memref_f32(%printOutput): (memref<*xf32>) -> ()

  return
}
func @print_memref_f32(memref<*xf32>)

