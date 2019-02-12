# CMakeList.txt : CMake project for ksc-test
cmake_minimum_required (VERSION 3.8)

# We need C++17 to compile KSC output
set(CMAKE_CXX_STANDARD 17)

set(KNOSSOS ${CMAKE_CURRENT_SOURCE_DIR}/../..)

include_directories(${KNOSSOS} ${KNOSSOS}/src/runtime)

# Add source to this project's executable.
add_executable("test-ksc" "test-ksc.cpp" "test-aux.cpp")
