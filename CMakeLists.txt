cmake_minimum_required(VERSION 3.10)

project(KSC)

# Project-wide.  llvm requires at least C++14.
set(CMAKE_CXX_STANDARD 14 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

include("user.cmake")

add_subdirectory(mlir)
