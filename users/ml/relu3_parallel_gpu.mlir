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

  %cfm2 = constant -0.2 : f32
  %cf0 = constant 0.0 : f32
  %cf1_3 = constant 0.3333333333 : f32
  %cf2_3 = constant 0.6666666667 : f32
  %cf1 = constant 1.0 : f32
  %cf3 = constant 3.0 : f32

  %c0 = constant 0 : index
  %c1 = constant 1 : index
  %c4 = constant 4 : index

  // prepare input
  %A = alloc() : memref<4x4xf32>
  %increment = constant 0.100000e+00 : f32
  %initVal = alloc() : memref<f32>
  store %cfm2, %initVal[] : memref<f32>
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

  // Allocate memory on GPU
  %t0 = gpu.wait async
  %gpu_input, %t1 = gpu.alloc async [%t0] () : memref<4x4xf32>
  %gpu_output, %t2 = gpu.alloc async [%t1] () : memref<4x4xf32>
  %t3 = gpu.memcpy async [%t2] %gpu_input, %A : memref<4x4xf32>, memref<4x4xf32>
  gpu.wait [%t3]

  // actual relu3
  scf.parallel (%i, %j) = (%c0, %c0) to (%c4, %c4) step (%c1, %c1) {
    %elem = load %gpu_input[%i, %j] : memref<4x4xf32>
    %condlt0 = cmpf "ult", %elem, %cf0 : f32
    %res = scf.if %condlt0 -> (f32) {         // if (x < 0)
      scf.yield %cf0 : f32                    //   return 0.0
    } else {
      %condlt1 = cmpf "ult", %elem, %cf1 : f32
      %res = scf.if %condlt1 -> (f32) {       // if (x < 1)
        %x1 = std.mulf %elem, %elem : f32     //
        %x2 = std.mulf %x1, %elem : f32       //
        %res = std.mulf %cf1_3, %x2 : f32     //    return 1/3 * x ** 3
        scf.yield %res : f32                  //
      } else {                                //
        %res = std.subf %elem, %cf2_3 : f32   //
        scf.yield %res : f32                  // return x - 2/3
      }
      scf.yield %res : f32
    }
    store %res, %gpu_output[%i, %j] : memref<4x4xf32>
    // here we could map the parallel dimensions to hardware dimensions. For now 1 to 1 mapping
  } { mapping = [{processor = 1, map = affine_map<(d0) -> (d0)>, bound = affine_map<(d0) -> (d0)>}, {processor = 0, map = affine_map<(d0) -> (d0)>, bound = affine_map<(d0) -> (d0)>}] }

  // Copy result from GPU and deallocate GPU memory
  %output = alloc() : memref<4x4xf32>
  %t10 = gpu.wait async
  %t11 = gpu.memcpy async [%t10] %output, %gpu_output : memref<4x4xf32>, memref<4x4xf32>
  %t12 = gpu.dealloc async [%t11] %gpu_input : memref<4x4xf32>
  %t13 = gpu.dealloc async [%t12] %gpu_output : memref<4x4xf32>
  gpu.wait[%t13]

  // print input & output
  %printA = memref_cast %A :  memref<4x4xf32> to memref<*xf32>
  call @print_memref_f32(%printA): (memref<*xf32>) -> ()
  %printOutput = memref_cast %output :  memref<4x4xf32> to memref<*xf32>
  call @print_memref_f32(%printOutput): (memref<*xf32>) -> ()

  return
}
func private @print_memref_f32(memref<*xf32>)

