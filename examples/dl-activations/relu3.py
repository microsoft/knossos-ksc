import torch
from ksc.torch_utils import elementwise_apply, elementwise_apply_pt18

# BEGINDOC
def relu3(x: float) -> float:
  """
  Like ReLu, but smoother
  Like GeLu, but cheaper
  """
  if x < 0.0:
    return 0.0
  elif x < 1.0:
    return 1 / 3 * x ** 3
  else:
    return x - 2 / 3

@torch.jit.ignore
def mymap(f : str, x : torch.Tensor) -> torch.Tensor:
  pass

def vrelu3(x : torch.Tensor):
  return mymap("relu3", x)
#  return torch.tensor([relu3(xij) for xij in x.flatten()]).reshape(x.shape)
#  return torch.tensor([relu3(xij) for xij in x])


# run-bench: PyTorch reference implementation
def vrelu3_pytorch(x: torch.Tensor):
    mask1_inf = x > 1.0
    mask0_1 = (x > 0.0) & ~mask1_inf
    val_0_1 = 1 / 3 * x ** 3
    val_1_inf = x - 2 / 3

    return mask0_1 * val_0_1 + mask1_inf * val_1_inf


# run-bench: PyTorch "nice" implementation
def relu3_pytorch_nice(x: float) -> float:
    if x < 0.0:
        return 0.0 * x  # Needed for PyTorch, not for Knossos.  See [Note: zeros]
    elif x < 1.0:
        return 1 / 3 * x ** 3
    else:
        return x - 2 / 3


# Note: zeros
# Need to multiply by x for pytorch to get the gradient back through x.
# This is something of a long story, related to tracing.
# Sort of related discussion https://discuss.pytorch.org/t/custom-loss-function-error-element-0-of-tensors-does-not-require-grad-and-does-not-have-grad-fn/87944/16

def vrelu3_pytorch_nice(x : torch.Tensor):
  return torch.vmap(relu3_pytorch_nice, x)

# run-bench: Define a range of values at which to call the methods
def vrelu3_bench_configs():
  yield torch.randn((4,))
  yield torch.randn((16,))
# yield torch.randn((256,256)) too slow to bench...


def plot_relu3(filename):
    import matplotlib.pyplot as plt
    import numpy as np

    t = torch.arange(-3, 3, step=0.1)

    plt.plot(t, vrelu3(t), "b")
    plt.plot(t, vrelu3_pytorch(t), "r--")
    plt.gca().set_aspect(1)
    print(f"Saving figure to {filename}")
    plt.savefig(filename)


def relu3_in_fcdnn():
    # Initialize the model using nn.Sequential
    model = nn.Sequential(
        OrderedDict(
            [
                ("fc1", nn.Linear(784, 256)),
                ("activation1", vrelu3),
                ("fc2", nn.Linear(256, 128)),
                ("bn2", nn.BatchNorm1d(num_features=128)),
                ("activation2", vrelu3),
                ("dropout", nn.Dropout(0.3)),
                ("fc3", nn.Linear(128, 64)),
                ("bn3", nn.BatchNorm1d(num_features=64)),
                ("activation3", vrelu3),
                ("logits", nn.Linear(64, 10)),
                ("logsoftmax", nn.LogSoftmax(dim=1)),
            ]
        )
    )

    # Run training
    train_model(model)
