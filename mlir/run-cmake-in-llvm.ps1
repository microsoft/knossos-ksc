# Powershell syntax, use 
#  sh mlir/run-cmake-in-llvm.sh | sh -x
# to run in bash

cmake ../llvm`
    -G Ninja`
    -DCMAKE_BUILD_TYPE=Debug`
    -DCMAKE_C_COMPILER=clang`
    -DCMAKE_CXX_COMPILER=clang++`
    -DLLVM_ENABLE_PROJECTS=mlir`
    -DLLVM_BUILD_EXAMPLES=OFF`
    -DLLVM_TARGETS_TO_BUILD="X86;NVPTX"`
    -DLLVM_ENABLE_ASSERTIONS=ON`
    -DLLVM_ENABLE_LLD=ON`
    -DMLIR_CUDA_RUNNER_ENABLED=ON
