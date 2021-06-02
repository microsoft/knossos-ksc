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
  static constexpr size_t Dim = 2;
  py::class_<ks::tensor<2, T>>(m, name, py::buffer_protocol(), py::module_local())
    .def(py::init([](std::uintptr_t v, size_t m, size_t n) {
        check_valid_pointer(v);
        ks::tensor_dimension<Dim>::index_type size {int(m),int(n)};
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
  static constexpr size_t Dim = 1;
  py::class_<ks::tensor<Dim, T>>(m, name, py::buffer_protocol(), py::module_local())
    .def(py::init([](std::uintptr_t v, size_t n) {
        check_valid_pointer(v);
        ks::tensor_dimension<Dim>::index_type size {int(n)};
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

bool g_ks_logging = false;

bool ks_logging(bool enable) {
    bool prev = g_ks_logging;
    g_ks_logging = enable;
    return prev;
}

// Convert functor to one which takes a first argument g_alloc,
// and optionally logs inputs and outputs to cerr
template<typename RetType, typename... ParamTypes>
auto with_ks_allocator(const char * tracingMessage, RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f, tracingMessage](ParamTypes... params) {
    if (g_ks_logging) {
        std::cerr << tracingMessage << "(";
        (std::cerr << ... << params);
        std::cerr << ") =" << std::endl;
        auto ret = f(&g_alloc, params...);
        std::cerr << ret << std::endl;
        return ret;
    } else {
        return f(&g_alloc, params...);
    }
  };
}
