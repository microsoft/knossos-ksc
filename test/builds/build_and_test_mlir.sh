set -e

# Known good git commit for LLVM
# Make sure to keep in sync with .yml build scripts
GOOD_HASH=e03394c6a6f
if [ "$1" != "" ]; then
  GOOD_HASH="$1"
fi

# Clone LLVM and build MLIR
# This configuration requires clang+lld
if [ ! -d llvm-project ]; then
  git clone https://github.com/llvm/llvm-project
fi
cd llvm-project
git checkout -b ksc-mlir "$GOOD_HASH"
mkdir build && cd build
cmake -G Ninja ../llvm \
        -DLLVM_ENABLE_PROJECTS=mlir \
        -DLLVM_BUILD_EXAMPLES=ON \
        -DLLVM_TARGETS_TO_BUILD="X86" \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_ENABLE_ASSERTIONS=ON
ninja check-mlir
