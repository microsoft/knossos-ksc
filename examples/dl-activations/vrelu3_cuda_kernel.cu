#include <torch/extension.h>

#include <cuda.h>
#include <cuda_runtime.h>

#include <vector>

template<typename scalar_t> using tensor_accessor_1 =
    torch::PackedTensorAccessor32<scalar_t,1,torch::RestrictPtrTraits>;
template<typename scalar_t> using tensor_accessor_2 =
    torch::PackedTensorAccessor32<scalar_t,2,torch::RestrictPtrTraits>;

template <typename scalar_t>
inline __device__ scalar_t relu3_forward(scalar_t input) {
  if (input < (scalar_t)0.0) {
    return (scalar_t)0.0;
  } else if (input < (scalar_t)1.0) {
    return (scalar_t)1/3 * input * input * input;
  } else {
    return input - (scalar_t)2/3;
  }
}

template <typename scalar_t>
__global__ void vrelu3_cuda_forward_kernel_1(
    const tensor_accessor_1<scalar_t> input,
    tensor_accessor_1<scalar_t> output) {
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < input.size(0))
    output[i] = relu3_forward(input[i]);
}

template <typename scalar_t>
__global__ void vrelu3_cuda_forward_kernel_2(
    const tensor_accessor_2<scalar_t> input,
    tensor_accessor_2<scalar_t> output) {
  const int n = blockIdx.y;
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < input.size(1))
    output[n][i] = relu3_forward(input[n][i]);
}

torch::Tensor vrelu3_cuda_forward(torch::Tensor input) {
  auto output = torch::zeros_like(input);

  switch (input.sizes().size()) {
    case 1: {
      const auto input_size = input.size(0);

      // TODO: find out how PyTorch chooses these parameters
      const int threads = 1024;
      const int blocks = (input_size + threads - 1) / threads;

      AT_DISPATCH_FLOATING_TYPES(input.type(), "vrelu3_forward_cuda (rank 1)", ([&] {
        vrelu3_cuda_forward_kernel_1<scalar_t><<<blocks, threads>>>(
            input.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>(),
            output.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>());
      }));
      break;
    }
    case 2: {
      const auto input_size_0 = input.size(0);
      const auto input_size_1 = input.size(1);

      const int threads = 1024;
      const dim3 blocks((input_size_1 + threads - 1) / threads, input_size_0);

      AT_DISPATCH_FLOATING_TYPES(input.type(), "vrelu3_forward_cuda (rank 2)", ([&] {
        vrelu3_cuda_forward_kernel_2<scalar_t><<<blocks, threads>>>(
            input.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>(),
            output.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>());
      }));
      break;
    }
    default:
      TORCH_CHECK(false, "Unsupported tensor rank");
  }
  return output;
}

template <typename scalar_t>
inline __device__ scalar_t relu3_backward(scalar_t grad, scalar_t x) {
  if (x < (scalar_t)0.0) {
    return (scalar_t)0.0;
  } else if (x < (scalar_t)1.0) {
    return x * x * grad;
  } else {
    return grad;
  }
}

template <typename scalar_t>
__global__ void vrelu3_cuda_backward_kernel_1(
    tensor_accessor_1<scalar_t> d_x,
    const tensor_accessor_1<scalar_t> grad,
    const tensor_accessor_1<scalar_t> x) {
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < x.size(0))
    d_x[i] = relu3_backward(grad[i], x[i]);
}

template <typename scalar_t>
__global__ void vrelu3_cuda_backward_kernel_2(
    tensor_accessor_2<scalar_t> d_x,
    const tensor_accessor_2<scalar_t> grad,
    const tensor_accessor_2<scalar_t> x) {
  const int n = blockIdx.y;
  const int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < x.size(1))
    d_x[n][i] = relu3_backward(grad[n][i], x[n][i]);
}

torch::Tensor vrelu3_cuda_backward(
    torch::Tensor grad,
    torch::Tensor x) {
  auto d_x = torch::zeros_like(x);
  switch (x.sizes().size()) {
    case 1: {
      auto x_size = x.size(0);

      // TODO: find out how PyTorch chooses these parameters
      const int threads = 1024;
      const int blocks = (x_size + threads - 1) / threads;

      AT_DISPATCH_FLOATING_TYPES(x.type(), "vrelu3_backward_cuda (rank 1)", ([&] {
        vrelu3_cuda_backward_kernel_1<scalar_t><<<blocks, threads>>>(
            d_x.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>(),
            grad.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>(),
            x.packed_accessor32<scalar_t,1,torch::RestrictPtrTraits>());
      }));
      break;
    }
    case 2: {
      auto x_size_0 = x.size(0);
      auto x_size_1 = x.size(1);

      const int threads = 1024;
      const dim3 blocks((x_size_1 + threads - 1) / threads, x_size_0);

      AT_DISPATCH_FLOATING_TYPES(x.type(), "vrelu3_backward_cuda (rank 2)", ([&] {
        vrelu3_cuda_backward_kernel_2<scalar_t><<<blocks, threads>>>(
            d_x.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>(),
            grad.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>(),
            x.packed_accessor32<scalar_t,2,torch::RestrictPtrTraits>());
      }));
      break;
    }
    default:
      TORCH_CHECK(false, "Unsupported tensor rank");
  }
  return d_x;
}


