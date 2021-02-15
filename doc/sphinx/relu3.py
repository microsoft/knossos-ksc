
def relu3(x : float) -> float:
  """
  relu3 -- Like relu, but smoother, with a lighter aftertaste
  """
  if x < 0.0:
    return 0.0
  elif x < 1.0:
    return 1/3 * x ** 3
  else:
    return x - 2/3
