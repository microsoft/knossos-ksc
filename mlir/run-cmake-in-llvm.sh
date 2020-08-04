# Translate from ps1 script so there's only one place to update
tr '`' '\\' < mlir/run-cmake-in-llvm.ps1 | sh -x
