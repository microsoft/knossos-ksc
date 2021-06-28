#include <torch/extension.h>

#include <cuda.h>
#include <cuda_runtime.h>

using ks_float = float;

#define CHECK_SCALAR_TYPE(x) TORCH_CHECK(x.scalar_type() == at::ScalarType::Float, #x " must use ks floating-point type")
#define CHECK_CUDA(x) TORCH_CHECK(x.is_cuda(), #x " must be a CUDA tensor")
#define CHECK_CONTIGUOUS(x) TORCH_CHECK(x.is_contiguous(), #x " must be contiguous")
#define CHECK_INPUT(x) CHECK_CUDA(x); CHECK_CONTIGUOUS(x)

template<typename scalar_t> using tensor_accessor_1 =
    torch::PackedTensorAccessor32<scalar_t,1,torch::RestrictPtrTraits>;
template<typename scalar_t> using tensor_accessor_2 =
    torch::PackedTensorAccessor32<scalar_t,2,torch::RestrictPtrTraits>;

template <typename scalar_t, typename F>
__global__ void map_kernel_1(
    const tensor_accessor_1<scalar_t> input,
    tensor_accessor_1<scalar_t> output,
    F f) {
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < input.size(0))
    output[i] = f(input[i]);
}

template <typename scalar_t, typename F>
__global__ void map_kernel_2(
    const tensor_accessor_2<scalar_t> input,
    tensor_accessor_2<scalar_t> output,
    F f) {
  const int n = blockIdx.y;
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < input.size(1))
    output[n][i] = f(input[n][i]);
}

template<typename F>
torch::Tensor map_gpu(
    torch::Tensor input,
    F f) {
  CHECK_INPUT(input);

  auto output = torch::zeros_like(input);

  switch (input.sizes().size()) {
    case 1: {
      const auto input_size = input.size(0);

      // TODO: find out how PyTorch chooses these parameters
      const int threads = 1024;
      const int blocks = (input_size + threads - 1) / threads;

      map_kernel_1<ks_float><<<blocks, threads>>>(
          input.packed_accessor32<ks_float,1,torch::RestrictPtrTraits>(),
          output.packed_accessor32<ks_float,1,torch::RestrictPtrTraits>(),
          f);
      break;
    }
    case 2: {
      const auto input_size_0 = input.size(0);
      const auto input_size_1 = input.size(1);

      const int threads = 1024;
      const dim3 blocks((input_size_1 + threads - 1) / threads, input_size_0);

      map_kernel_2<ks_float><<<blocks, threads>>>(
          input.packed_accessor32<ks_float,2,torch::RestrictPtrTraits>(),
          output.packed_accessor32<ks_float,2,torch::RestrictPtrTraits>(),
          f);
      break;
    }
    default:
      TORCH_CHECK(false, "Unsupported tensor rank");
  }
  return output;
}

template <typename scalar_t, typename RevF>
__global__ void revmap_kernel_1(
    tensor_accessor_1<scalar_t> d_x,
    const tensor_accessor_1<scalar_t> grad,
    const tensor_accessor_1<scalar_t> x,
    RevF rev_f) {
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < x.size(0))
    d_x[i] = rev_f(grad[i], x[i]);
}

template <typename scalar_t, typename RevF>
__global__ void revmap_kernel_2(
    tensor_accessor_2<scalar_t> d_x,
    const tensor_accessor_2<scalar_t> grad,
    const tensor_accessor_2<scalar_t> x,
    RevF rev_f) {
  const int n = blockIdx.y;
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < x.size(1))
    d_x[n][i] = rev_f(grad[n][i], x[n][i]);
}

template<typename RevF>
torch::Tensor revmap_gpu(
    torch::Tensor grad,
    torch::Tensor x,
    RevF rev_f) {
  CHECK_INPUT(grad);
  CHECK_INPUT(x);

  auto d_x = torch::zeros_like(x);
  switch (x.sizes().size()) {
    case 1: {
      auto x_size = x.size(0);

      // TODO: find out how PyTorch chooses these parameters
      const int threads = 1024;
      const int blocks = (x_size + threads - 1) / threads;

      revmap_kernel_1<ks_float><<<blocks, threads>>>(
          d_x.packed_accessor32<ks_float,1,torch::RestrictPtrTraits>(),
          grad.packed_accessor32<ks_float,1,torch::RestrictPtrTraits>(),
          x.packed_accessor32<ks_float,1,torch::RestrictPtrTraits>(),
          rev_f);
      break;
    }
    case 2: {
      auto x_size_0 = x.size(0);
      auto x_size_1 = x.size(1);

      const int threads = 1024;
      const dim3 blocks((x_size_1 + threads - 1) / threads, x_size_0);

      revmap_kernel_2<ks_float><<<blocks, threads>>>(
          d_x.packed_accessor32<ks_float,2,torch::RestrictPtrTraits>(),
          grad.packed_accessor32<ks_float,2,torch::RestrictPtrTraits>(),
          x.packed_accessor32<ks_float,2,torch::RestrictPtrTraits>(),
          rev_f);
      break;
    }
    default:
      TORCH_CHECK(false, "Unsupported tensor rank");
  }
  return d_x;
}
