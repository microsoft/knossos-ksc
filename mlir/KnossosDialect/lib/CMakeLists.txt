add_mlir_dialect_library(MLIRKnossos
        KnossosDialect.cpp
        KnossosOps.cpp

        ADDITIONAL_HEADER_DIRS
        ${PROJECT_SOURCE_DIR}/include/Knossos

        DEPENDS
        MLIRKnossosOpsIncGen
        )

target_link_libraries(MLIRKnossos PUBLIC MLIRIR)
