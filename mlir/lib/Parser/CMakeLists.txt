add_mlir_dialect_library(MLIRKnossosParser
        Assert.cpp
        Lexer.cpp
        AST.cpp
        Parser.cpp
        MLIR.cpp
        )

target_link_libraries(MLIRKnossosParser PUBLIC MLIRIR)
