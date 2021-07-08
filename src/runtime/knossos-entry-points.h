#pragma once

#include "knossos.h"

#include <torch/extension.h>

#include <iostream>

namespace ks {
namespace entry_points {

extern ks::allocator g_alloc;
void reset_allocator();
size_t allocator_top();
size_t allocator_peak();

extern bool g_logging;
bool logging(bool enable);

// Convert functor to one which takes a first argument g_alloc,
// and optionally logs inputs and outputs to cerr
template<typename RetType, typename... ParamTypes>
auto with_ks_allocator(const char * tracingMessage, RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f, tracingMessage](ParamTypes... params) {
    if (g_logging) {
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

constexpr at::ScalarType scalar_type_of_Float = c10::CppTypeToScalarType<Float>::value;

template<typename KSType, typename EntryPointType>
struct Converter
{
  static_assert(std::is_same<KSType, EntryPointType>::value, "Entry point type is not supported");

  static KSType to_ks(EntryPointType arg) {
    return arg;
  }

  static EntryPointType from_ks(KSType ret) {
    return ret;
  }
};

template<>
struct Converter<ks::tensor<1, Float>, torch::Tensor>
{
  static ks::tensor<1, Float> to_ks(torch::Tensor arg) {
    KS_ASSERT(arg.sizes().size() == 1u);
    KS_ASSERT(arg.is_contiguous());
    KS_ASSERT(arg.scalar_type() == scalar_type_of_Float);
    return ks::tensor<1, Float>((int)arg.size(0), arg.data_ptr<Float>());
  }

  static torch::Tensor from_ks(ks::tensor<1, Float> ret) {
    torch::Tensor torch_ret = torch::empty(ret.size(), torch::TensorOptions().dtype(scalar_type_of_Float));
    std::memcpy(torch_ret.data_ptr(), ret.data(), ret.size() * sizeof(Float));
    return torch_ret;
  }
};

template<>
struct Converter<ks::tensor<2, Float>, torch::Tensor>
{
  static ks::tensor<2, Float> to_ks(torch::Tensor arg) {
    KS_ASSERT(arg.sizes().size() == 2u);
    KS_ASSERT(arg.is_contiguous());
    KS_ASSERT(arg.scalar_type() == scalar_type_of_Float);
    return ks::tensor<2, Float>({(int)arg.size(0), (int)arg.size(1)}, arg.data_ptr<Float>());
  }

  static torch::Tensor from_ks(ks::tensor<2, Float> ret) {
    auto [size0, size1] = ret.size();
    torch::Tensor torch_ret = torch::empty({size0, size1}, torch::TensorOptions().dtype(scalar_type_of_Float));
    std::memcpy(torch_ret.data_ptr(), ret.data(), size0 * size1 * sizeof(Float));
    return torch_ret;
  }
};

template<typename KSType, typename EntryPointType>
KSType convert_argument(EntryPointType arg) {
  return Converter<KSType, EntryPointType>::to_ks(arg);
}

template<typename EntryPointType, typename KSType>
EntryPointType convert_return_value(KSType ret) {
  return Converter<KSType, EntryPointType>::from_ks(ret);
}

}
}

