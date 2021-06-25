#include <torch/extension.h>

torch::Tensor vrelu3_cuda_forward(torch::Tensor input);
torch::Tensor vrelu3_cuda_backward(torch::Tensor grad, torch::Tensor x);

PYBIND11_MODULE(TORCH_EXTENSION_NAME, m) {
  m.def("forward", &vrelu3_cuda_forward, "vrelu3 forward (CUDA)");
  m.def("backward", &vrelu3_cuda_backward, "vrelu3 backward (CUDA)");
}

