import torch

from gmm import gmm_objective

print(gmm_objective.graph)

# extracted from adbench test
alphas = torch.tensor(
    [-0.6490, 1.1812, -0.7585], dtype=torch.float64, requires_grad=True
)
means = torch.tensor(
    [[0.0923, 0.1863], [0.3456, 0.3968], [0.5388, 0.4192]],
    dtype=torch.float64,
    requires_grad=True,
)
icf = torch.tensor(
    [[0.5864, -0.8519, 0.8003], [-1.5094, 0.8759, -0.2428], [0.1668, -1.9654, -1.2701]],
    dtype=torch.float64,
    requires_grad=True,
)
x = torch.tensor([[1.1752, 2.0292]], dtype=torch.float64)
wishart_gamma = torch.tensor(1.0, dtype=torch.float64)
wishart_m = torch.tensor(0.0, dtype=torch.float64)

result = gmm_objective(alphas, means, icf, x, wishart_gamma, wishart_m)

print(result)

print(means.grad)

result.backward(retain_graph = True)

print(means.grad)
