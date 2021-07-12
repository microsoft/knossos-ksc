#include "knossos-kernel.cuh"

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
inline __device__ scalar_t relu3_backward(scalar_t grad, scalar_t x) {
  if (x < (scalar_t)0.0) {
    return (scalar_t)0.0;
  } else if (x < (scalar_t)1.0) {
    return x * x * grad;
  } else {
    return grad;
  }
}

struct Relu3_forward
{
  template<typename scalar_t>
  inline __device__ scalar_t operator()(scalar_t input) {
    return relu3_forward(input);
  }
};

struct Relu3_backward
{
  template<typename scalar_t>
  inline __device__ scalar_t operator()(scalar_t grad, scalar_t x) {
    return relu3_backward(grad, x);
  }
};

torch::Tensor vrelu3_cuda_forward(torch::Tensor input) {
  return map_gpu(input, Relu3_forward{});
}

torch::Tensor vrelu3_cuda_backward(
    torch::Tensor grad,
    torch::Tensor x) {
  return revmap_gpu(grad, x, Relu3_backward{});
}

