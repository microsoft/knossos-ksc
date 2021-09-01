#pragma once

#include "knossos.h"

#include <iostream>

namespace ks {
namespace entry_points {

#ifdef KS_ALLOCATOR
extern ks::allocator g_alloc;
#endif

void reset_allocator();
size_t allocator_top();
size_t allocator_peak();

template<typename KSType, typename EntryPointType>
KSType convert_to_ks_viewing_tensordata(EntryPointType arg);

template<typename EntryPointType, typename KSType>
EntryPointType convert_from_ks(KSType ret);

template<typename KSType, typename EntryPointType>
struct Converter
{
  static_assert(std::is_same<KSType, EntryPointType>::value, "Entry point type is not supported");

  // Convert arg to ks type.
  // Scalars and tuples will copy, tensors will be viewed.
  static KSType to_ks(EntryPointType arg) {
    return arg;
  }

  // Convert ks type to caller's type.  
  // Scalars and tuples will copy, tensors may be copied, or may view the Knossos heap.
  // TODO: bool parameter/template parameter which clarifies whether copy or view.
  static EntryPointType from_ks(KSType ret) {
    return ret;
  }
};

template<typename ...KsElementTypes, typename ...EntryPointElementTypes>
struct Converter<ks::Tuple<KsElementTypes...>, std::tuple<EntryPointElementTypes...>>
{
  template<size_t ...Indices>
  static ks::Tuple<KsElementTypes...> to_ks_impl(std::tuple<EntryPointElementTypes...> arg, std::index_sequence<Indices...>) {
    return ks::make_Tuple(convert_to_ks_viewing_tensordata<KsElementTypes>(std::get<Indices>(arg))...);
  }

  static ks::Tuple<KsElementTypes...> to_ks(std::tuple<EntryPointElementTypes...> arg) {
    return to_ks_impl(arg, std::index_sequence_for<EntryPointElementTypes...>{});
  }

  template<size_t ...Indices>
  static std::tuple<EntryPointElementTypes...> from_ks_impl(ks::Tuple<KsElementTypes...> ret, std::index_sequence<Indices...>) {
    return std::make_tuple(convert_from_ks<EntryPointElementTypes>(ks::get<Indices>(ret))...);
  }

  static std::tuple<EntryPointElementTypes...> from_ks(ks::Tuple<KsElementTypes...> ret) {
    return from_ks_impl(ret, std::index_sequence_for<KsElementTypes...>{});
  }
};

template<typename KSType, typename EntryPointType>
KSType convert_to_ks_viewing_tensordata(EntryPointType arg) {
  return Converter<KSType, EntryPointType>::to_ks(arg);
}

template<typename EntryPointType, typename KSType>
EntryPointType convert_from_ks(KSType ret) {
  return Converter<KSType, EntryPointType>::from_ks(ret);
}

}
}

