#include <cstdint>

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/operators.h>

namespace py = pybind11;

#include "knossos.h"

ks::allocator g_alloc{ 1'000'000'000 };

// Convert functor to one which takes a first argument g_alloc 
template<typename RetType, typename... ParamTypes>
auto with_ks_allocator(RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f](ParamTypes... params) {
    return f(&g_alloc, params...);
  };
}
