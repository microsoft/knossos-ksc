#pragma once

#include "knossos-entry-points.h"

#include <torch/extension.h>

namespace ks {
namespace entry_points {

constexpr at::ScalarType scalar_type_of_Float = c10::CppTypeToScalarType<Float>::value;

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

// TODO: common these? 
template<>
struct Converter<ks::tensor<3, Float>, torch::Tensor>
{
  static ks::tensor<3, Float> to_ks(torch::Tensor arg) {
    KS_ASSERT(arg.sizes().size() == 3u);
    KS_ASSERT(arg.is_contiguous());
    KS_ASSERT(arg.scalar_type() == scalar_type_of_Float);
    ks::tensor<3, Float>::index_type size {(int)arg.size(0), (int)arg.size(1), (int)arg.size(2)};
    return ks::tensor<3, Float>(size, arg.data_ptr<Float>());
  }

  static torch::Tensor from_ks(ks::tensor<3, Float> ret) {
    auto [size0, size1, size2] = ret.size();
    torch::Tensor torch_ret = torch::empty({size0, size1, size2}, torch::TensorOptions().dtype(scalar_type_of_Float));
    std::memcpy(torch_ret.data_ptr(), ret.data(), size0 * size1 * size2 * sizeof(Float));
    return torch_ret;
  }
};

}
}
