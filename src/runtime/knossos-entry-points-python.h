#pragma once

#include "knossos-entry-points.h"

namespace ks {
namespace entry_points {

template<typename KsElementType, typename EntryPointElementType>
struct Converter<ks::tensor<1, KsElementType>, std::vector<EntryPointElementType>>
{
  static ks::tensor<1, KsElementType> to_ks(std::vector<EntryPointElementType> const& arg) {
    auto ks_arg = ks::tensor<1, KsElementType>::create(&g_alloc, arg.size());
    for (int i = 0; i != ks_arg.size(); ++i) {
      ks_arg[i] = convert_to_ks_viewing_tensordata<KsElementType>(arg[i]);
    }
    return ks_arg;
  }

  static std::vector<EntryPointElementType> from_ks(ks::tensor<1, KsElementType> const& ks_ret) {
    std::vector<EntryPointElementType> ret;
    for (int i = 0; i != ks_ret.size(); ++i) {
      ret.push_back(convert_from_ks<EntryPointElementType>(ks_ret[i]));
    }
    return ret;
  }
};

template<typename KsType>
struct PurePythonEntryPointType
{
  using type = KsType;
};

template<typename ...KsTypes>
struct PurePythonEntryPointType<ks::Tuple<KsTypes...>>
{
  using type = std::tuple<typename PurePythonEntryPointType<KsTypes>::type...>;
};

template<typename KsElementType>
struct PurePythonEntryPointType<ks::tensor<1, KsElementType>>
{
  using type = std::vector<typename PurePythonEntryPointType<KsElementType>::type>;
};

template<typename RetType, typename... ParamTypes>
auto python_entry_point(RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f](typename PurePythonEntryPointType<ParamTypes>::type ...params) {
    return convert_from_ks<typename PurePythonEntryPointType<RetType>::type>(
      f(&g_alloc, convert_to_ks_viewing_tensordata<ParamTypes>(params)...)
    );
  };
}

}
}