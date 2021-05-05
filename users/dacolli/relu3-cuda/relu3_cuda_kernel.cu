#include <torch/extension.h>

#include <cuda.h>
#include <cuda_runtime.h>

#include <vector>

template <typename scalar_t>
__global__ void relu3_cuda_forward_kernel(
    const torch::PackedTensorAccessor32<scalar_t,1,torch::RestrictPtrTraits> input,
    torch::PackedTensorAccessor32<scalar_t,1,torch::RestrictPtrTraits> output) {
  // element index
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < input.size(0)){
    if (input[i] >= 0) {
      if (input[i] > 1) {
        output[i] = input[i] - (scalar_t)2/3;
      } else {
        output[i] = (scalar_t)1/3 * input[i] * input[i] * input[i];
      }
    }
  }
}

torch::Tensor relu3_cuda_forward(
    torch::Tensor input) {
  auto output = torch::zeros_like(input);
  const auto input_size = input.size(0);

  const int threads = 1024;
  const int blocks = (input_size + threads - 1) / threads;

  AT_DISPATCH_FLOATING_TYPES(input.type(), "relu3_forward_cuda", ([&] {
    relu3_cuda_forward_kernel<scalar_t><<<blocks, threads>>>(
        input.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>(),
        output.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>());
  }));

  return output;
}

template <typename scalar_t>
__global__ void relu3_cuda_backward_kernel(
    torch::PackedTensorAccessor32<scalar_t,1,torch::RestrictPtrTraits> d_x,
    const torch::PackedTensorAccessor32<scalar_t,1,torch::RestrictPtrTraits> grad,
    const torch::PackedTensorAccessor32<scalar_t,1,torch::RestrictPtrTraits> x) {
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < x.size(0)){
    if (x[i] >= 0) {
      if (x[i] > 1) {
        d_x[i] = grad[i];
      } else {
        d_x[i] = x[i] * x[i] * grad[i];
      }
    }
  }
}

torch::Tensor relu3_cuda_backward(
    torch::Tensor grad,
    torch::Tensor x) {
  auto d_x = torch::zeros_like(x);
  auto x_size = x.size(0);

  const int threads = 1024;
  const int blocks = (x_size + threads - 1) / threads;

  AT_DISPATCH_FLOATING_TYPES(x.type(), "relu3_backward_cuda", ([&] {
    relu3_cuda_backward_kernel<scalar_t><<<blocks, threads>>>(
        d_x.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>(),
        grad.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>(),
        x.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>());
  }));

  return d_x;
}


