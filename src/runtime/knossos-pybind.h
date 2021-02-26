#include <cstdint>
#include <stdexcept>

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/operators.h>

namespace py = pybind11;

#include "knossos.h"

ks::allocator g_alloc{ 1'000'000'000 };

namespace pybind11 { namespace detail {

/* Specialize type_caster for the ks::tuple type.
   Ideally we'd just inherit from pybind11::detail::tuple_caster, but our ks::tuple
   doesn't quite conform to the required interface (it uses ks::get instead of
   std::get), so this is a slightly-modified version. */
template <typename... Ts> class type_caster<ks::tuple<Ts...>> {
    using type = ks::tuple<Ts...>;
    static constexpr auto size = sizeof...(Ts);
    using indices = make_index_sequence<size>;
public:
    bool load(handle src, bool convert) {
        if (!isinstance<sequence>(src))
            return false;
        const auto seq = reinterpret_borrow<sequence>(src);
        if (seq.size() != size)
            return false;
        return load_impl(seq, convert, indices{});
    }

    template <typename T>
    static handle cast(T &&src, return_value_policy policy, handle parent) {
        return cast_impl(std::forward<T>(src), policy, parent, indices{});
    }

    static constexpr auto name = _("Tuple[") + concat(make_caster<Ts>::name...) + _("]");

    template <typename T> using cast_op_type = type;

    operator type() & { return implicit_cast(indices{}); }
    operator type() && { return std::move(*this).implicit_cast(indices{}); }

private:
    template <size_t... Is>
    type implicit_cast(index_sequence<Is...>) & { return type{cast_op<Ts>(ks::get<Is>(subcasters))...}; }
    template <size_t... Is>
    type implicit_cast(index_sequence<Is...>) && { return type{cast_op<Ts>(std::move(ks::get<Is>(subcasters)))...}; }

    static constexpr bool load_impl(const sequence &, bool, index_sequence<>) { return true; }

    template <size_t... Is>
    bool load_impl(const sequence &seq, bool convert, index_sequence<Is...>) {
        for (bool r : {ks::get<Is>(subcasters).load(seq[Is], convert)...})
            if (!r)
                return false;
        return true;
    }

    /* Implementation: Convert a C++ tuple into a Python tuple */
    template <typename T, size_t... Is>
    static handle cast_impl(T &&src, return_value_policy policy, handle parent, index_sequence<Is...>) {
        std::array<object, size> entries{{
            reinterpret_steal<object>(make_caster<Ts>::cast(ks::get<Is>(std::forward<T>(src)), policy, parent))...
        }};
        for (const auto &entry: entries)
            if (!entry)
                return handle();
        tuple result(size);
        int counter = 0;
        for (auto & entry: entries)
            PyTuple_SET_ITEM(result.ptr(), counter++, entry.release().ptr());
        return result.release();
    }

    ks::tuple<make_caster<Ts>...> subcasters;
};

}}


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

static void check_valid_pointer(std::uintptr_t v)
{
    if (v < 1u<<24) {
        // v should be a pointer, if it's smaller than 0x00ffffff, it's probably a misplaced size
        throw std::domain_error("generate_and_compile_cpp_from_ks: probable misplaced size");
    }
}

template<typename T>
void declare_tensor_2(py::module &m, char const* name) {
  // Wrap ks_tensor<Dim, T> to point to supplied python memory
  py::class_<ks::tensor<2, T>>(m, name, py::buffer_protocol(), py::module_local())
    .def(py::init([](std::uintptr_t v, size_t m, size_t n) {
        check_valid_pointer(v);
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
        check_valid_pointer(v);
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
  return [f](ParamTypes... params) {
    return f(&g_alloc, params...);
  };
}
