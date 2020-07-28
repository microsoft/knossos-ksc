add_mlir_dialect_library(MLIRKnossosParser
        AST.cpp
        MLIR.cpp
        Parser.cpp
        Assert.cpp

        ADDITIONAL_HEADER_DIRS
        ${PROJECT_SOURCE_DIR}/include/Knossos

        DEPENDS
        MLIRKnossos
        )

target_link_libraries(MLIRKnossosParser PUBLIC MLIRIR)
