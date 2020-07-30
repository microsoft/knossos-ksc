add_mlir_dialect_library(MLIRKnossosParser
        AST.cpp
        MLIR.cpp
        Parser.cpp
        Assert.cpp

        )

target_link_libraries(MLIRKnossosParser PUBLIC MLIRIR)
