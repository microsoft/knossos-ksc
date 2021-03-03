
# BEGINDOC
def relu3(x : float) -> float:
  """
  relu3 -- Like relu, but smoother, with a robust backbone
  """
  if x < 0.0:
    return 0.0
  elif x < 1.0:
    return 1/3 * x ** 3
  else:
    return x - 2/3
# ENDDOC


def plot_relu3(filename):
  import matplotlib.pyplot as plt
  import numpy as np

  t = np.arange(-3,3,step=0.1)

  plt.plot(t, [relu3(t) for t in t])
  plt.gca().set_aspect(1)
  print(f"Saving figure to {filename}")
  plt.savefig(filename)
