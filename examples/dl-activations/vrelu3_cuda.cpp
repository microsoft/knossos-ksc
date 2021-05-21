#include <torch/extension.h>

#include <vector>

// CUDA forward declarations

torch::Tensor vrelu3_cuda_forward(
    torch::Tensor input);

torch::Tensor vrelu3_cuda_backward(
    torch::Tensor grad,
    torch::Tensor x);

// C++ interface

#define CHECK_CUDA(x) TORCH_CHECK(x.type().is_cuda(), #x " must be a CUDA tensor")
#define CHECK_CONTIGUOUS(x) TORCH_CHECK(x.is_contiguous(), #x " must be contiguous")
#define CHECK_INPUT(x) CHECK_CUDA(x); CHECK_CONTIGUOUS(x)

torch::Tensor vrelu3_forward(
    torch::Tensor input) {
  CHECK_INPUT(input);
  return vrelu3_cuda_forward(input);
}

torch::Tensor vrelu3_backward(
    torch::Tensor grad,
    torch::Tensor x) {
  CHECK_INPUT(grad);
  CHECK_INPUT(x);
  return vrelu3_cuda_backward(
      grad,
      x);
}

PYBIND11_MODULE(TORCH_EXTENSION_NAME, m) {
  m.def("forward", &vrelu3_forward, "vrelu3 forward (CUDA)");
  m.def("backward", &vrelu3_backward, "vrelu3 backward (CUDA)");
}

