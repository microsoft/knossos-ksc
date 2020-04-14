import ksc
from ksc.tracing.functions import core, math, nn
import numpy as np

@ksc.trace
def dense(w, x):
  return math.dot(x, math.transpose(w))

@ksc.trace
def gdense(w, x, gout):
  gw = math.dot(math.transpose(gout), x)         # gradient of w
  gx = math.dot(gout, w)                         # gradient of x
  return core.make_tuple(gw, gx)

@ksc.trace
def loss(x, y):
  l = nn.logistic_cross_entropy(x, y)
  gb = nn.logistic_cross_entropy_grad(x, y)
  return core.make_tuple(l, gb)

@ksc.trace
def opt(w, gw):
  '''Simple gradient descent.'''
  alpha = 1.0  # Hyperparameter, fixing it for simplicity
  return core.sub(w, core.mul(alpha, gw))

@ksc.trace
def train(wA, wB, x, y):
  a = dense(wA, x)                               # Forward layer A
  b = dense(wB, a)                               # Forward layer B
  l, gb = loss(b, y)                             # Loss computation
  gwB, ga = gdense(wB, a, gb)                    # Backward layer B
  wB = opt(wB, gwB)                              # Optimizer/weight-update B
  gwA, _ = gdense(wA, x, ga)                     # Backward layer A
  wA = opt(wA, gwA)                              # Optimizer/weight-udpate A
  return core.make_tuple(wA, wB)                 # Return new weights


def main():
  import numpy as np
  num_samples = 4
  num_features = 6
  x = np.ones((num_samples, num_features))
  wA = np.ones((num_features, num_features))
  wB = np.ones((1, num_features))
  y = np.ones((num_samples, 1))
  out = train(wA, wB, x, y)
  ks_str = out.creator._jitted.combined_ks_str()
  with open("toy.ks", "w") as f:
    f.write(ks_str)
  print(out.get_data_with_backend('jax'))

if __name__ == "__main__":
    main()