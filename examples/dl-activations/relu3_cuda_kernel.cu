#include <torch/extension.h>

#include <cuda.h>
#include <cuda_runtime.h>

#include <vector>

template <typename scalar_t>
__global__ void relu3_cuda_forward_kernel(
    const torch::PackedTensorAccessor32<scalar_t,2,torch::RestrictPtrTraits> input,
    torch::PackedTensorAccessor32<scalar_t,2,torch::RestrictPtrTraits> output) {
  // element index
  const int n = blockIdx.y;
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < input.size(1)){
    auto input_elem = input[n][i];
    if (input_elem >= 0) {
      if (input_elem > 1) {
        output[n][i] = input_elem - (scalar_t)2/3;
      } else {
        output[n][i] = (scalar_t)1/3 * input_elem * input_elem * input_elem;
      }
    }
  }
}

torch::Tensor relu3_cuda_forward(
    torch::Tensor input) {
  auto output = torch::zeros_like(input);

  // TODO: check rank of input (assumed to be rank 2 here)
  const auto input_size_0 = input.size(0);
  const auto input_size_1 = input.size(1);

  // TODO: find out how PyTorch chooses these parameters
  const int threads = 1024;
  const dim3 blocks((input_size_1 + threads - 1) / threads, input_size_0);

  AT_DISPATCH_FLOATING_TYPES(input.type(), "relu3_forward_cuda", ([&] {
    relu3_cuda_forward_kernel<scalar_t><<<blocks, threads>>>(
        input.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>(),
        output.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>());
  }));

  return output;
}

template <typename scalar_t>
__global__ void relu3_cuda_backward_kernel(
    torch::PackedTensorAccessor32<scalar_t,2,torch::RestrictPtrTraits> d_x,
    const torch::PackedTensorAccessor32<scalar_t,2,torch::RestrictPtrTraits> grad,
    const torch::PackedTensorAccessor32<scalar_t,2,torch::RestrictPtrTraits> x) {
  const int n = blockIdx.y;
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < x.size(1)){
    if (x[n][i] >= 0) {
      if (x[n][i] > 1) {
        d_x[n][i] = grad[n][i];
      } else {
        d_x[n][i] = x[n][i] * x[n][i] * grad[n][i];
      }
    }
  }
}

torch::Tensor relu3_cuda_backward(
    torch::Tensor grad,
    torch::Tensor x) {
  auto d_x = torch::zeros_like(x);
  auto x_size_0 = x.size(0);
  auto x_size_1 = x.size(1);

  // TODO: find out how PyTorch chooses these parameters
  const int threads = 1024;
  const dim3 blocks((x_size_1 + threads - 1) / threads, x_size_0);

  AT_DISPATCH_FLOATING_TYPES(x.type(), "relu3_backward_cuda", ([&] {
    relu3_cuda_backward_kernel<scalar_t><<<blocks, threads>>>(
        d_x.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>(),
        grad.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>(),
        x.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>());
  }));

  return d_x;
}


