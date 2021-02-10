#include <cstdint>

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

// Convert functor to one which takes a first argument g_alloc 
template<typename RetType, typename... ParamTypes>
auto with_ks_allocator(RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f](ParamTypes... params) {
    return f(&g_alloc, params...);
  };
}
