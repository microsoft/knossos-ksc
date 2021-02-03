#include <cstdint>

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/operators.h>

namespace py = pybind11;

#include "knossos.h"

namespace ks {

int main(ks::allocator *) { return 0; };

}

ks::allocator g_alloc{ 1'000'000'000 };

template<typename T>
void declare_tensor_2(py::module &m, char const* name) {
  // Wrap ks_tensor<Dim, T> to point to supplied python memory
  py::class_<ks::tensor<2, T>>(m, name, py::buffer_protocol(), py::module_local())
    .def(py::init([](std::uintptr_t v, size_t m, size_t n) {
        if (v < 1u<<24) {
            // probably a misplaced size
            throw std::domain_error("generate_and_compile_cpp_from_ks: probable misplaced size");
        }
        ks::tensor_dimension<2>::index_type size {m,n};
        return ks::tensor<2, T>(size, reinterpret_cast<T*>(v)); // Reference to caller's data 
    }))
    // And describe buffer shape to Python
    // Returned tensors will be living on g_alloc, so will become invalid after allocator_reset()
    .def_buffer([](ks::tensor<2, T> &t) -> py::buffer_info {
        return py::buffer_info(
            t.data(),                               /* Pointer to buffer */
            sizeof(T),                              /* Size of one scalar */
            py::format_descriptor<T>::format(),     /* Python struct-style format descriptor */
            2,                                      /* Number of dimensions */
            { ks::get_dimension<0>(t.size()), ks::get_dimension<1>(t.size()) },         /* Buffer dimensions */
            { sizeof(T) * ks::get_dimension<1>(t.size()),             /* Strides (in bytes) for each index */
               sizeof(T) }
        );
    })
    ;
}

template<typename T>
void declare_tensor_1(py::module &m, char const* name) {
  // Wrap ks_tensor<1, T> to point to supplied python memory
  constexpr int Dim = 1;
  py::class_<ks::tensor<Dim, T>>(m, name, py::buffer_protocol(), py::module_local())
    .def(py::init([](std::uintptr_t v, size_t n) {
        if (v < 1u<<24) {
            // probably a misplaced size
            throw std::domain_error("generate_and_compile_cpp_from_ks: probable misplaced size");
        }
        ks::tensor_dimension<Dim>::index_type size {n};
        return ks::tensor<Dim, T>(size, reinterpret_cast<T*>(v)); // Reference to caller's data 
    }))
    // And describe buffer shape to Python
    // Returned tensors will be living on g_alloc, so will become invalid after allocator_reset()
    .def_buffer([](ks::tensor<Dim, T> &t) -> py::buffer_info {
        return py::buffer_info(
            t.data(),                               /* Pointer to buffer */
            sizeof(T),                              /* Size of one scalar */
            py::format_descriptor<T>::format(),     /* Python struct-style format descriptor */
            Dim,                                      /* Number of dimensions */
            { ks::get_dimension<0>(t.size()) },         /* Buffer dimensions */
            { sizeof(T) }
        );
    })
    ;
}


// Convert functor to one which takes a first argument g_alloc 
template<typename RetType, typename... ParamTypes>
auto with_ks_allocator(RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f](ParamTypes... params) { return f(&g_alloc, params...); };
}
