import math
import torch
import time
from torch.utils.cpp_extension import load

relu3_cuda = load("relu3", sources=["relu3_cuda.cpp", "relu3_cuda_kernel.cu"])

class ReLu3Function(torch.autograd.Function):
    @staticmethod
    def forward(ctx, input):
        output = relu3_cuda.forward(input)
        ctx.save_for_backward(input)
        return output

    @staticmethod
    def backward(ctx, grad):
        return relu3_cuda.backward(grad.contiguous(), *ctx.saved_variables)

class ReLu3(torch.nn.Module):
    def __init__(self):
        super(ReLu3, self).__init__()

    def forward(self, input):
        return ReLu3Function.apply(input)

assert torch.cuda.is_available()
cuda_device = torch.device("cuda")

input_features = 32

X = torch.randn(input_features, device=cuda_device, requires_grad=True)

rnn = ReLu3().to(cuda_device)

forward = 0
backward = 0
for _ in range(100000):
    start = time.time()
    # new_h, new_C = rnn(X, (h, C))
    fwd_result = rnn(X)
    torch.cuda.synchronize()
    forward += time.time() - start

    start = time.time()
    fwd_result.sum().backward()
    torch.cuda.synchronize()
    backward += time.time() - start

print('Forward: {:.3f} us | Backward {:.3f} us'.format(forward * 1e6/1e5, backward * 1e6/1e5))
