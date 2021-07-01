#include <torch/extension.h>

torch::Tensor vrelu3_forward(torch::Tensor input) {
  TORCH_CHECK(input.is_contiguous(), "Not contiguous");
  TORCH_CHECK(input.ndimension() == 1, "Not one dimensional");

  auto ret = torch::empty_like(input);

  auto input_data = input.data_ptr<double>();
  auto ret_data = input.data_ptr<double>();

  auto n = input.size(0);

  for (int i = 0; i < n; i++) {
    auto x = input_data[i];
    // auto mask1_inf = x > 1.0;
    // auto mask0_1 = (x > 0.0) & ~mask1_inf;
    // auto val_0_1 = 1.0 / 3.0 * x * x * x;
    // auto val_1_inf = x - 2.0 / 3.0;

    // auto r = mask0_1 * val_0_1 + mask1_inf * val_1_inf;

    double r;

    if (x < 0.0) {
      r = 0.0;
    } else if (x < 1.0) {
      r = 1.0 / 3.0 * x * x * x;
    } else {
      r = x - 2.0 / 3.0;
    }

    ret_data[i] = r;
  }

  return ret;
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

