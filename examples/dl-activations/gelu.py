
# Gelu and activations
def gelu(x : float) -> float:
  return 0.5*x*(1+erf(x/sqrt(2)))

def gelu_approx_sigmoid(x : float) -> float:
  # From https://github.com/hendrycks/GELUs: fast but somewhat inaccurate
  return sigmoid(1.702 * x) * x

def gelu_approx_tanh(x : float) -> float:
  # From https://github.com/microsoft/onnxruntime/pull/3398/files
  B = 0.7978845608028654    // sqrt(2.0 / M_PI)
  C = 0.035677408136300125  // 0.044715 * sqrt(2.0 / M_PI)

  return 0.5 * (1 + tanh(x * (C * x * x + B))) * x
