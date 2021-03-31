# Relu3 MLIR GPU Case Study

### Relu3 ground truth
```LISP
def relu3(x: float) -> float
if x < 0.0:
  return 0.0
elif x < 1.0:
  return x ** 3
else
  return x
```

In this file we only show the relevant relu3 IR. The sections preparing the input / printing the output are omitted.  
They can be examined in the actual files.

Note: The cuda-runner does not support the powf operation. **All instances of powf are substituted with mulf**, such that the lowering functions properly.

### scf sequential
`mlir/test/mlir/relu3_scf.mlir` - A simple sequential version of relu3 applied to a matrix included here for reference.
```mlir
scf.for %i = %i0 to %i4 step %i1 {
  scf.for %j = %i0 to %i4 step %i1 {
    %elem = load %arg0[%i, %j] : memref<4x4xf32>
    %condlt0 = cmpf "ult", %elem, %cf0 : f32
    %res = scf.if %condlt0 -> (f32) {         // if (elem < 0)
      scf.yield %cf0 : f32
    } else {
      %condlt1 = cmpf "ult", %elem, %cf1 : f32
      %res = scf.if %condlt1 -> (f32) {       // if (elem < 1)
        %res = std.mulf %elem, %cf3 : f32
        scf.yield %res : f32
      } else {                                // else
        scf.yield %elem : f32
      }
      scf.yield %res : f32
    }
    store %res, %arg1[%i, %j] : memref<4x4xf32>
  }
}
```
Pass pipeline for CPU execution as sanity check: (same output as GPU execution way at the bottom)
```bash
mlir-opt -convert-scf-to-std -convert-std-to-llvm -canonicalize mlir/test/mlir/relu3_scf.mlir | build/bin/mlir-cpu-runner -shared-libs=build/lib/libmlir_runner_utils.so -entry-point-result=void
```
For further reference there is also a version in the Affine dialect: `mlir/test/mlir/relu3_affine.mlir` with the pipeline:
```bash
mlir-opt -lower-affine -convert-scf-to-std -convert-std-to-llvm -canonicalize mlir/test/mlir/relu3_scf.mlir | build/bin/mlir-cpu-runner -shared-libs=build/lib/libmlir_runner_utils.so -entry-point-result=void
```

### scf parallel
`mlir/test/mlir/relu3_parallel.mlir` - A parallel version of the above.  The two loops have been replaced with `scf.parallel`, keeping the two induction variables.
The op provides a mechanism to specify a mapping of parallel to phyiscal hardware dimensions.

```mlir
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
```

#### Passes for lowering relu3_parallel to IR ready to be passed to cuda-runner:
Note: Applying the passes one by one is very helpful to understand how they transform the IR.
- -convert-parallel-loops-to-gpu: Replaces `scf.parallel` with `gpu.launch`.
Here the number of CUDA threads and blocks to start is determined based on the mapping specified above.
They should be chosen depending on the computation and the device they are to be executed on.
**Important note**: memrefs which are accessed in the body of `scf.parallel` have to be registered to the GPU.
It is also necessary what exactly this registration does, i.e. to which memory of the GPU is the memref copied to/from?
During experimentation this did not happen automatically in this pass. Hence, the registration was added manually
for the output and input (see `mlir/test/mlir/relu3_parallel_lowered.mlir`):
```mlir
%cast_A = memref_cast %A : memref<4x4xf32> to memref<*xf32>
gpu.host_register %cast_A : memref<*xf32>
```

- -convert-scf-to-std: Replacing the remaining scf operations with the corresponding operations of the standard dialect.

- -gpu-kernel-outlining: Outline the GPU IR portion into its own module

```bash
mlir-opt -convert-parallel-loops-to-gpu -convert-scf-to-std -gpu-kernel-outlining -canonicalize mlir/test/mlir/relu3_parallel.mlir
```
yields:

```mlir
  [...]
  "gpu.launch_func"(%c3, %c3, %c1, %c1, %c1, %c1, %0, %10) {kernel = @main_kernel::@main_kernel} : (index, index, index, index, index, index, memref<4x4xf32>, memref<4x4xf32>) -> ()
  [...]

  gpu.module @main_kernel {
    gpu.func @main_kernel(%arg0: memref<4x4xf32>, %arg1: memref<4x4xf32>) kernel {
      %cst = constant 0.000000e+00 : f32
      %cst_0 = constant 1.000000e+00 : f32
      %cst_1 = constant 3.000000e+00 : f32
      %0 = "gpu.block_id"() {dimension = "x"} : () -> index
      %1 = "gpu.block_id"() {dimension = "y"} : () -> index
      %2 = load %arg0[%1, %0] : memref<4x4xf32>
      %3 = cmpf "ult", %2, %cst : f32
      cond_br %3, ^bb3(%cst : f32), ^bb1
    ^bb1:  // pred: ^bb0
      %4 = cmpf "ult", %2, %cst_0 : f32
      cond_br %4, ^bb2, ^bb3(%2 : f32)
    ^bb2:  // pred: ^bb1
      %5 = mulf %2, %cst_1 : f32
      br ^bb3(%5 : f32)
    ^bb3(%6: f32):  // 3 preds: ^bb0, ^bb1, ^bb2
      store %6, %arg1[%1, %0] : memref<4x4xf32>
      gpu.return
    }
  }
```

This IR can be passed on to cuda-runner for execution on a Nvidia GPU. Make sure to actually build with CUDA support, i.e. `-DLLVM_TARGETS_TO_BUILD="NVPTX" -DMLIR_CUDA_RUNNER_ENABLED=ON`

```bash
mlir-opt -convert-linalg-to-loops -convert-parallel-loops-to-gpu -convert-scf-to-std -gpu-kernel-outlining -canonicalize mlir/test/mlir/relu3_parallel.mlir  | ./build/bin/mlir-cuda-runner -shared-libs=build/lib/libmlir_runner_utils.so,build/lib/libcuda-runtime-wrappers.so -entry-point-result=void
```
Output (input to the computation on top, output at the bottom):
```bash
Unranked Memref base@ = 0x55e27b674050 rank = 2 offset = 0 sizes = [4, 4] strides = [4, 1] data = 
[[0,   0.1,   0.2,   0.3], 
 [0.4,   0.5,   0.6,   0.7], 
 [0.8,   0.9,   1,   1.1], 
 [1.2,   1.3,   1.4,   1.5]]
Unranked Memref base@ = 0x55e27c3c4fb0 rank = 2 offset = 0 sizes = [4, 4] strides = [4, 1] data = 
[[0,   0.3,   0.6,   0.9], 
 [1.2,   1.5,   1.8,   2.1], 
 [2.4,   2.7,   1,   1.1], 
 [1.2,   1.3,   1.4,   1.5]]
```
As expected, all input values < 1 are multiplied by 3 (as powf was not available on the interface to CUDA during this experimentation).

For reproduction without any manual editing execute the following pass pipeline
using `mlir/test/mlir/relu3_parallel_lowered.mlir`, which already has the necessary fixes:
```bash
mlir-opt -convert-scf-to-std -gpu-kernel-outlining -canonicalize mlir/test/mlir/relu3_parallel_lowered.mlir  | build/bin/mlir-cuda-runner -shared-libs=build/lib/libmlir_runner_utils.so,build/lib/libcuda-runtime-wrappers.so -entry-point-result=void
```