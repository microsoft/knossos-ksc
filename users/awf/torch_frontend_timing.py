import torch
import time
import numpy

from ksc.torch_frontend import ts2mod


class time_sampler:
    def __init__(self, minimizing=False):
        self.minimizing = minimizing
        if self.minimizing:
            self.time = 1e10
        else:
            self.time = 0
            self.ncalls = 0

    def duration(self):
        if self.minimizing:
            return self.time
        else:
            return self.time / self.ncalls

    @property
    def us(self):
        return self.duration() * 1e6

    @staticmethod
    def get_time():
        return time.time_ns() * 1e-9

    def mark(self):
        self.start = time_sampler.get_time()

    def record(self):
        delta = time_sampler.get_time() - self.start
        if self.minimizing:
            self.time = min(delta, self.time)
        else:
            self.time += delta
            self.ncalls += 1


# %%

if __name__ == "__xmain__":
    # %%
    import math
    import torch
    import torch.nn.functional as F

    do_original = False

    if do_original:

        def lltm_forward_py_orig(input, weights, bias, old_h, old_cell):
            X = torch.cat([old_h, input], dim=1)

            # Compute the input, output and candidate cell gates with one MM.
            gate_weights = F.linear(X, weights, bias)

            # Split the combined gate weight matrix into its components.
            gates = gate_weights.chunk(3, dim=1)

            input_gate = torch.sigmoid(gates[0])
            output_gate = torch.sigmoid(gates[1])
            # Here we use an ELU instead of the usual tanh.
            candidate_cell = F.elu(gates[2])

            # Compute the new cell state.
            new_cell = old_cell + candidate_cell * input_gate
            # Compute the new hidden state and output.
            new_h = torch.tanh(new_cell) * output_gate

            return new_h, new_cell

    else:
        # Simpler model to test zero-runtime implementation
        def lltm_forward_py(input, weights, bias, old_h, old_cell):
            X = torch.cat([old_h, input], dim=1)

            # Compute the input, output and candidate cell gates with one MM.
            gate_weights = F.linear(X, weights, bias)

            input_gate = torch.tanh(gate_weights)
            output_gate = torch.tanh(gate_weights)
            candidate_cell = torch.tanh(gate_weights)

            # Compute the new cell state.
            new_cell = old_cell + candidate_cell * input_gate
            # Compute the new hidden state and output.
            new_h = torch.tanh(new_cell) * output_gate

            return new_h, new_cell

    lltm_forward = lltm_forward_py

    class LLTM(torch.nn.Module):
        def __init__(self, input_features, state_size):
            super(LLTM, self).__init__()
            self.input_features = input_features
            self.state_size = state_size
            if do_original:
                # 3 * state_size for input gate, output gate and candidate cell gate.
                # input_features + state_size because we will multiply with [input, h].
                self.weights = torch.nn.Parameter(
                    torch.empty(3 * state_size, input_features + state_size)
                )
                self.bias = torch.nn.Parameter(torch.empty(3 * state_size))
            else:
                self.weights = torch.nn.Parameter(
                    torch.empty(state_size, input_features + state_size)
                )
                self.bias = torch.nn.Parameter(torch.empty(state_size))

            self.reset_parameters()

        def reset_parameters(self):
            stdv = 1.0 / math.sqrt(self.state_size)
            for weight in self.parameters():
                weight.data.uniform_(-stdv, +stdv)

        def forward(self, input, state):
            return lltm_forward(input, self.weights, self.bias, *state)

    # run it...
    batch_size = 16
    input_features = 32
    state_size = 30

    X = torch.randn(batch_size, input_features)
    h = torch.randn(batch_size, state_size)
    C = torch.randn(batch_size, state_size)

    rnn = LLTM(input_features, state_size)

    def myloss(X, h, c):
        new_h, new_C = rnn(X, (h, C))
        return new_h.sum() + new_C.sum()

    def timeit_local(msg):
        print("Timing: ", msg, lltm_forward)
        forward = time_sampler()
        backward = time_sampler()
        nruns = 50
        for _ in range(nruns):
            forward.mark()
            loss = myloss(X, h, C)
            forward.record()

            backward.mark()
            loss.backward()
            backward.record()

        print(f"Forward: {forward.us:.3f} us | Backward {backward.us:.3f} us")

    timeit_local("py")

    lltm_forward_ts = torch.jit.script(lltm_forward_py)
    lltm_forward = lltm_forward_ts
    timeit_local("ts")
    #%%
    example_inputs = (X, rnn.weights, rnn.bias, h, C)
    fn = torch.jit.script(lltm_forward_py)
    # print(fn.graph)
    #     graph(%input.1 : Tensor,
    #       %weights.1 : Tensor,
    #       %bias.1 : Tensor,
    #       %old_h.1 : Tensor,
    #       %old_cell.1 : Tensor):
    #   %30 : Function = prim::Constant[name="elu"]()
    #   %29 : bool = prim::Constant[value=0]()
    #   %28 : float = prim::Constant[value=1.]()
    #   %13 : Function = prim::Constant[name="linear"]()
    #   %8 : int = prim::Constant[value=1]() # <ipython-input-4-ecbf56b83299>:7:38
    #   %16 : int = prim::Constant[value=3]() # <ipython-input-4-ecbf56b83299>:12:31
    #   %19 : int = prim::Constant[value=0]() # <ipython-input-4-ecbf56b83299>:14:37
    #   %26 : int = prim::Constant[value=2]() # <ipython-input-4-ecbf56b83299>:17:33
    #   %7 : Tensor[] = prim::ListConstruct(%old_h.1, %input.1)
    #   %X.1 : Tensor = aten::cat(%7, %8) # <ipython-input-4-ecbf56b83299>:7:8
    #   %gate_weights.1 : Tensor = prim::CallFunction(%13, %X.1, %weights.1, %bias.1) # <ipython-input-4-ecbf56b83299>:10:19
    #   %gates.1 : Tensor[] = aten::chunk(%gate_weights.1, %16, %8) # <ipython-input-4-ecbf56b83299>:12:12
    #   %20 : Tensor = aten::__getitem__(%gates.1, %19) # <ipython-input-4-ecbf56b83299>:14:31
    #   %input_gate.1 : Tensor = aten::sigmoid(%20) # <ipython-input-4-ecbf56b83299>:14:17
    #   %23 : Tensor = aten::__getitem__(%gates.1, %8) # <ipython-input-4-ecbf56b83299>:15:32
    #   %output_gate.1 : Tensor = aten::sigmoid(%23) # <ipython-input-4-ecbf56b83299>:15:18
    #   %27 : Tensor = aten::__getitem__(%gates.1, %26) # <ipython-input-4-ecbf56b83299>:17:27
    #   %candidate_cell.1 : Tensor = prim::CallFunction(%30, %27, %28, %29) # <ipython-input-4-ecbf56b83299>:17:21
    #   %35 : Tensor = aten::mul(%candidate_cell.1, %input_gate.1) # <ipython-input-4-ecbf56b83299>:20:26
    #   %new_cell.1 : Tensor = aten::add(%old_cell.1, %35, %8) # <ipython-input-4-ecbf56b83299>:20:15
    #   %39 : Tensor = aten::tanh(%new_cell.1) # <ipython-input-4-ecbf56b83299>:22:12
    #   %new_h.1 : Tensor = aten::mul(%39, %output_gate.1) # <ipython-input-4-ecbf56b83299>:22:12
    #   %44 : (Tensor, Tensor) = prim::TupleConstruct(%new_h.1, %new_cell.1)
    #   return (%44)

    ks_fun = ts2mod(lltm_forward_py, example_inputs=example_inputs)

    def torch_from_ks(ks_object):
        if isinstance(ks_object, tuple):
            return tuple(torch_from_ks(ks) for ks in ks_object)

        return torch.from_numpy(numpy.array(ks_object, copy=True))

    class KnossosLLTMFunction(torch.autograd.Function):
        @staticmethod
        def forward(ctx, input, weights, bias, old_h, old_cell):
            args = (input, weights, bias, old_h, old_cell)

            ks_fun._py_mod.reset_allocator()
            ks_args = (ks_fun.torch_to_ks(x) for x in args)

            # Call it
            outputs = ks_fun(*ks_args)

            ctx.save_for_backward(*args)

            return torch_from_ks(outputs)

        @staticmethod
        def backward(ctx, grad_h, grad_cell):
            ks_args = tuple(ks_fun.torch_to_ks(x) for x in ctx.saved_tensors)
            grad_args = (grad_h, grad_cell)
            ks_grad_args = tuple(ks_fun.torch_to_ks(x) for x in grad_args)
            outputs = ks_fun.rev(ks_args, ks_grad_args)

            return torch_from_ks(outputs)

    lltm_forward = KnossosLLTMFunction.apply
    timeit_local("Knossos")

    #%%
    import torch.utils.cpp_extension

    print("Compiling extension ...", end="")
    lltm_cpp = torch.utils.cpp_extension.load(
        name="lltm_cpp", sources=[utils.get_ksc_dir() + "/src/ts2k/ts2ks/lltm.cpp"]
    )
    print("done.")

    class LLTMFunction(torch.autograd.Function):
        @staticmethod
        def forward(ctx, input, weights, bias, old_h, old_cell):
            outputs = lltm_cpp.forward(input, weights, bias, old_h, old_cell)
            new_h, new_cell = outputs[:2]
            variables = outputs[1:] + [weights]
            ctx.save_for_backward(*variables)

            return new_h, new_cell

        @staticmethod
        def backward(ctx, grad_h, grad_cell):
            outputs = lltm_cpp.backward(
                grad_h.contiguous(), grad_cell.contiguous(), *ctx.saved_tensors
            )
            d_old_h, d_input, d_weights, d_bias, d_old_cell = outputs
            return d_input, d_weights, d_bias, d_old_h, d_old_cell

    lltm_forward = LLTMFunction.apply
    timeit_local("native")


#%%

if __name__ == "__xmain__":
    from math import sin

    def bar(a: int, x: float):
        M = torch.tensor([[1.1, -x], [x + 2.1, 2.2]])
        v = torch.tensor([2.2, 3.3])

        Mv = torch.matmul(M, v)

        b = torch.dot(Mv, v)

        if a < 0:
            t = -0.125 * x
        else:
            t = 1 / 2 * x * float(b)
        return sin(t) * t

    def foofilter(xs: torch.Tensor):
        t = torch.zeros(xs.shape)
        for n, x in enumerate(xs):
            if x < 0:
                t[n] = -0.125 * x
            else:
                t[n] = 1 / (n + 1) * x ** 2

        return torch.mean(torch.sin(t) * t)

    def foofilter_comp(xs: torch.Tensor):
        t = torch.tensor(
            [
                (-0.125 * x if x < 0.0 else 1 / (n + 1) * x ** 2).item()
                for n, x in enumerate(xs)
            ]
        )
        return torch.mean(torch.sin(t) * t)

    def foofilter_mask(x: torch.Tensor):
        mask = x < 0
        t = mask * (-0.125 * x) + (1 - mask) * 1 / 2 * x ** 2
        return torch.mean(torch.sin(t) * t)

    x_example = torch.rand((23,))

    fn = torch.jit.script(foofilter_comp)
    print(fn.code)
    # print(fn(x_example))

    # #AWF: TODO: check "training" attribute -- does that enable faster AD?
    # with open("/tmp/t.onnx", "w") as temp:
    #     torch.onnx.export(model=fn,
    #                   args=x_example,
    #                   example_outputs=fn(x_example),
    #                   f=temp,
    #                   verbose=True)
    print(fn.graph)
    ks_str = ts2ks_fromgraph(False, fn.name, fn.graph, (x_example,))
    cpprint(ks_str)

    ks_fun = ts2mod(foofilter_comp, example_inputs=(x_example,))


if __name__ == "__main__":
    print("\n\n*************************\n\n")

    # "Squared Leaky Relu"?
    def squirrel(x: torch.Tensor):
        y = torch.mean(x)
        if y < 0.0:
            t = -0.125 * x
        else:
            t = 1 / 2 * x ** 2
        return torch.mean(torch.sin(t) * t)

    # Compile function and gradients for example input of ones(2,3)
    x_example = torch.ones((2, 3))

    fn = torch.jit.script(squirrel)
    print(fn.code)
    ks_str = ts2ks_fromgraph(False, fn.name, fn.graph, (x_example,))
    cpprint(ks_str)

    # Compile function and gradients for example input of ones(2,3)
    x_example = torch.ones((2, 3))
    ks_fun = ts2mod(squirrel, example_inputs=(x_example,))

    # Call the function at different, interesting inputs
    x = torch.rand((4, 4))  # TODO: check non-square

    ans = squirrel(x)
    print("Python answer = ", ans.numpy())

    ts_squirrel = torch.jit.script(squirrel)
    print("TorchScript answer = ", ts_squirrel(x).numpy())

    kx = ks_fun.torch_to_ks(x)
    ans = ks_fun(kx)
    print("Knossos answer = ", ans)

    # Compute the gradient
    ans = ks_fun.rev(kx, 1.0)
    ansnp = numpy.array(ans, copy=False)
    print("Knossos gradient = \n", ansnp)

    # Compute the gradient using torch
    xtrace = x.clone().detach().requires_grad_(True)
    y = squirrel(xtrace)
    dy = torch.autograd.grad(y, xtrace)
    print("Torch gradient = \n", dy[0].numpy())

    print("Gradient diff = \n", ansnp - dy[0].numpy())

    # print(f"Knossos mem: {ks_fun._py_mod.allocator_top()}/{ks_fun._py_mod.allocator_peak()}")
    import timeit

    def time_ks(n):
        x = torch.rand((n, n))
        ks_fun._py_mod.reset_allocator()
        kx = ks_fun.torch_to_ks(x)
        ans = ks_fun.rev(kx, 1.0)
        # print(numpy.array(ans, copy=False))

    def time_pytorch(n):
        x = torch.rand((n, n))
        x.requires_grad_(True)
        y = ts_squirrel(x)
        dy = torch.autograd.grad(y, x)
        # print(dy)

    size = 4
    ntimes = 10000
    print("time_ks= ", timeit.timeit(lambda: time_ks(size), number=ntimes))
    print("time_pt= ", timeit.timeit(lambda: time_pytorch(size), number=ntimes))

    # Next:
    #  - foofilter
