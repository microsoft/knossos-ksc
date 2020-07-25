set -e

# LLVM build directory path
LLVM_BUILD="$(pwd)/llvm-project/build"
if [ "$1" != "" ]; then
  LLVM_BUILD="$*"
fi
if [ ! -f "$LLVM_BUILD/llvm.spec" ]; then
  echo "$LLVM_BUILD doesn't seen to be an LLVM build dir"
  return 1
fi
export LLVM_BUILD="$LLVM_BUILD"

# Clone Knossos and build ksc-mlir
# This configuration requires clang+lld
#if [ ! -d knossos-ksc ]; then
#  git clone https://github.com/microsoft/knossos-ksc.git
#fi
#cd knossos-ksc/mlir

if [ ! -f "SYNTAX.md" ]; then
  echo "SYNTAX.md isn't present, is this a Knossos repository? Aborting"
  return 1
fi

KSC_MLIR_SRC="$(pwd)"
#git pull
rm -rf build
mkdir build && cd build
cmake -G Ninja "$KSC_MLIR_SRC" \
        -DMLIR_DIR="$LLVM_BUILD/lib/cmake/mlir" \
        -DLLVM_EXTERNAL_LIT="$LLVM_BUILD/bin/llvm-lit"
ninja check-ksc
