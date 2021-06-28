#include <torch/extension.h>

torch::Tensor vrelu3_forward(torch::Tensor input) {
  torch::Tensor x = input;
  torch::Tensor mask1_inf = x > 1.0;
  torch::Tensor mask0_1 = (x > 0.0) & ~mask1_inf;
  torch::Tensor val_0_1 = 1.0 / 3.0 * torch::pow(x, 3);
  torch::Tensor val_1_inf = x - 2.0 / 3.0;

  return mask0_1 * val_0_1 + mask1_inf * val_1_inf;
}

torch::Tensor vrelu3_backward(torch::Tensor grad, torch::Tensor x) {
  torch::Tensor mask1_inf = x > 1.0;
  torch::Tensor mask0_1 = (x > 0.0) & ~mask1_inf;
  torch::Tensor val_0_1 = torch::pow(x, 2);
  torch::Tensor val_1_inf = torch::full_like(x, 1.0);

  return (mask0_1 * val_0_1 + mask1_inf * val_1_inf) * grad;
}

PYBIND11_MODULE(TORCH_EXTENSION_NAME, m) {
  m.def("forward", &vrelu3_forward, "vrelu3 forward");
  m.def("backward", &vrelu3_backward, "vrelu3 backward");
}

