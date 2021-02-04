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

template <class T>
auto to_std_tuple(T const& t)
{
    return t;
}

template<typename TupleT, size_t ...Indices>
auto to_std_tuple_impl(TupleT const& t, std::index_sequence<Indices...>)
{
  return std::make_tuple(to_std_tuple(ks::get<Indices>(t))...);
}

template<typename ...Ts>
auto to_std_tuple(ks::tuple<Ts...> const& t)
{
  return to_std_tuple_impl(t, std::index_sequence_for<Ts...>{});
}

template <class T>
auto to_ks_tuple(T const& t)
{
    return t;
}

template<typename TupleT, size_t ...Indices>
auto to_ks_tuple_impl(TupleT const& t, std::index_sequence<Indices...>)
{
  return ks::make_tuple(to_std_tuple(std::get<Indices>(t))...);
}

template<typename ...Ts>
auto to_ks_tuple(std::tuple<Ts...> const& t)
{
  return to_ks_tuple_impl(t, std::index_sequence_for<Ts...>{});
}

/*
	template<typename TupleT, typename F, size_t ...Indices>
	auto transform_tuple_impl(TupleT const& t, F f, std::index_sequence<Indices...>)
	{
		return ks::make_tuple(f(ks::get<Indices>(t))...);
	}

	template<typename ...Ts, typename F>
	auto transform_tuple(tuple<Ts...> const& t, F f)
	{
		return transform_tuple_impl(t, f, std::index_sequence_for<Ts...>{});
	}
*/


template<typename T> using to_std_tuple_t = decltype(to_std_tuple(T{}));

// Convert functor to one which takes a first argument g_alloc 
template<typename RetType, typename... ParamTypes>
auto with_ks_allocator(RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f](to_std_tuple_t<ParamTypes>... params) {
    return to_std_tuple(f(&g_alloc, to_ks_tuple(params)...)); 
  };
}
