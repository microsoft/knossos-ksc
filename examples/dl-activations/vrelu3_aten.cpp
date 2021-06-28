#include <torch/extension.h>

torch::Tensor vrelu3_forward(torch::Tensor input) {
  return input * 3.14159;
}

torch::Tensor vrelu3_backward(torch::Tensor grad, torch::Tensor x) {
  return torch::full_like(x, 3.14159);
}

PYBIND11_MODULE(TORCH_EXTENSION_NAME, m) {
  m.def("forward", &vrelu3_forward, "vrelu3 forward");
  m.def("backward", &vrelu3_backward, "vrelu3 backward");
}

