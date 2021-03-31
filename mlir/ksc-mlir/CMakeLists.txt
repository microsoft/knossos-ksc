get_property(dialect_libs GLOBAL PROPERTY MLIR_DIALECT_LIBS)
get_property(conversion_libs GLOBAL PROPERTY MLIR_CONVERSION_LIBS)
set(LIBS
        ${dialect_libs}
        ${conversion_libs}
        MLIRExecutionEngine
        MLIROptLib
        )
add_llvm_executable(ksc-mlir main.cpp test.cpp)

llvm_update_compile_flags(ksc-mlir)
target_link_libraries(ksc-mlir PRIVATE ${LIBS})
