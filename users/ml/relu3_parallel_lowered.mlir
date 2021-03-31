// mlir-opt -convert-scf-to-std -gpu-kernel-outlining -canonicalize mlir/test/mlir/relu3_parallel_lowered.mlir  | mlir-cuda-runner -shared-libs=build/lib/libmlir_runner_utils.so,build/lib/libcuda-runtime-wrappers.so -entry-point-result=void

module {
  func @main() {
    %cst = constant 0.000000e+00 : f32
    %cst_1 = constant 1.0 : f32
    %cst_3 = constant 3.000000e+00 : f32
    %c0 = constant 0 : index
    %cst_01 = constant 1.000000e-01 : f32
    %c4 = constant 4 : index
    %c1 = constant 1 : index
    %c3 = constant 3 : index
    %A = alloc() : memref<4x4xf32>
    %1 = alloc() : memref<f32>
    store %cst, %1[] : memref<f32>
    scf.for %arg0 = %c0 to %c4 step %c1 {
      scf.for %arg1 = %c0 to %c4 step %c1 {
        %5 = load %1[] : memref<f32>
        store %5, %A[%arg0, %arg1] : memref<4x4xf32>
        %6 = addf %5, %cst_01 : f32
        store %6, %1[] : memref<f32>
      }
    }
    %output = alloc() : memref<4x4xf32>

    // manually added to make %A and %output available to the GPU
    %cast_A = memref_cast %A : memref<4x4xf32> to memref<*xf32>
    gpu.host_register %cast_A : memref<*xf32>
    %cast_out = memref_cast %output : memref<4x4xf32> to memref<*xf32>
    gpu.host_register %cast_out : memref<*xf32>

    // lowered from relu3_parallel.mlir
    gpu.launch blocks(%arg0, %arg1, %arg2) in (%arg6 = %c4, %arg7 = %c4, %arg8 = %c1) threads(%arg3, %arg4, %arg5) in (%arg9 = %c1, %arg10 = %c1, %arg11 = %c1) {
      %5 = load %A[%arg1, %arg0] : memref<4x4xf32>
      %6 = cmpf "olt", %5, %cst : f32
      %7 = scf.if %6 -> (f32) {
        scf.yield %cst : f32
      } else {
        %8 = cmpf "olt", %5, %cst_1 : f32
        %9 = scf.if %8 -> (f32) {
          %10 = mulf %5, %cst_3 : f32
          scf.yield %10 : f32
        } else {
          scf.yield %5 : f32
        }
        scf.yield %9 : f32
      }
      store %7, %output[%arg1, %arg0] : memref<4x4xf32>
      gpu.terminator
    }
    %3 = memref_cast %A : memref<4x4xf32> to memref<*xf32>
    call @print_memref_f32(%3) : (memref<*xf32>) -> ()
    %4 = memref_cast %output : memref<4x4xf32> to memref<*xf32>
    call @print_memref_f32(%4) : (memref<*xf32>) -> ()
    return
  }
  func private @print_memref_f32(memref<*xf32>)
}