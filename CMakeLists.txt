cmake_minimum_required(VERSION 3.10)

project(KSC)

# Project-wide.  llvm requires at least C++14.
set(CMAKE_CXX_STANDARD 14 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

include("user.cmake")

# Check uer.cmake defined what we need
if (NOT EXISTS "${MLIR_DIR}/MLIRConfig.cmake")  
	message(FATAL_ERROR 
		"Could not find MLIRConfig.cmake\n" 
		"${CMAKE_SOURCE_DIR}/user.cmake defined MLIR_DIR as ${MLIR_DIR}\n"   
		"Expected to find MLIRConfig.cmake there")
endif()

add_subdirectory(mlir)
