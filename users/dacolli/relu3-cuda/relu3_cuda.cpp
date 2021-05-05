#include <torch/extension.h>

#include <vector>

// CUDA forward declarations

torch::Tensor relu3_cuda_forward(
    torch::Tensor input);

torch::Tensor relu3_cuda_backward(
    torch::Tensor grad,
    torch::Tensor x);

// C++ interface

#define CHECK_CUDA(x) TORCH_CHECK(x.type().is_cuda(), #x " must be a CUDA tensor")
#define CHECK_CONTIGUOUS(x) TORCH_CHECK(x.is_contiguous(), #x " must be contiguous")
#define CHECK_INPUT(x) CHECK_CUDA(x); CHECK_CONTIGUOUS(x)

torch::Tensor relu3_forward(
    torch::Tensor input) {
  CHECK_INPUT(input);
  return relu3_cuda_forward(input);
}

torch::Tensor relu3_backward(
    torch::Tensor grad,
    torch::Tensor x) {
  CHECK_INPUT(grad);
  CHECK_INPUT(x);
  return relu3_cuda_backward(
      grad,
      x);
}

PYBIND11_MODULE(TORCH_EXTENSION_NAME, m) {
  m.def("forward", &relu3_forward, "vrelu3 forward (CUDA)");
  m.def("backward", &relu3_backward, "vrelu3 backward (CUDA)");
}

