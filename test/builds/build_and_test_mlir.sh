set -e

# Known good git commit for LLVM
if [ "$1" != "" ]; then
  GOOD_HASH="$1"
  echo "Using GOOD_HASH=$1 from command line"
else
  GOOD_HASH=$(cat "${0%/*}/../../etc/llvm-branch.txt")
  echo "Using GOOD_HASH=${GOOD_HASH} from ${0%/*}/../../etc/llvm-branch.txt"
fi

# Clone LLVM and build MLIR
# This configuration requires clang+lld
if [ ! -d llvm-project ]; then
  git clone https://github.com/llvm/llvm-project
fi
cd llvm-project
git checkout -b ksc-mlir "$GOOD_HASH"
mkdir -p build && cd build
cmake -G Ninja ../llvm \
        -DLLVM_ENABLE_PROJECTS=mlir \
        -DLLVM_BUILD_EXAMPLES=ON \
        -DLLVM_TARGETS_TO_BUILD="X86" \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_ENABLE_ASSERTIONS=ON
ninja check-mlir
