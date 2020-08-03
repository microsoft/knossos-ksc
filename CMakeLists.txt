cmake_minimum_required(VERSION 3.10)

project(KSC)

include("user.cmake")

# Check uer.cmake defined what we need
if (NOT EXISTS "${MLIR_DIR}/MLIRConfig.cmake")  
	message(FATAL_ERROR 
		"Could not find MLIRConfig.cmake\n" 
		"${CMAKE_SOURCE_DIR}/user.cmake defined MLIR_DIR as ${MLIR_DIR}\n"   
		"Expected to find MLIRConfig.cmake there")
endif()

add_subdirectory(mlir)
