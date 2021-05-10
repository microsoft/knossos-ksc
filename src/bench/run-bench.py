import time
import torch

from ts2ks import tsmod2ksmod

torch.set_default_dtype(torch.float64)


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

    @property
    def ms(self):
        return self.duration() * 1e3

    def mark(self):
        self.start = time_sampler.get_time()

    def record(self):
        delta = time_sampler.get_time() - self.start
        if self.minimizing:
            self.time = min(delta, self.time)
        else:
            self.time += delta
            self.ncalls += 1

    @staticmethod
    def get_time():
        return time.time_ns() * 1e-9


def fun_and_grad_matches(f, g, arg):
    fval = f(arg)
    gval = g(arg)
    if not torch.all(torch.isclose(fval, gval)):
        print("run-bench: ERROR: VALUE mismatch", f, g)
        print(fval, gval)
        return False

    return True


def timeit(msg, fn, arg):
    MAX_TIME = 5  # No no need to run beyond MAX_TIME sec to get accurate benchmarks
    end_time = time.time() + MAX_TIME
    inference_timer = time_sampler()
    forward_timer = time_sampler()
    backward_timer = time_sampler()
    nruns = 5000
    for _ in range(nruns):
        inference_timer.mark()
        with torch.no_grad():
            loss = fn(arg).sum()
        inference_timer.record()

        arg.requires_grad = True
        forward_timer.mark()
        loss = fn(arg).sum()
        forward_timer.record()

        backward_timer.mark()
        grad = torch.autograd.grad(loss, arg)
        backward_timer.record()

        if time.time() > end_time:
            print(f"# Ran to timeout: {fn} {msg} ")
            break

    csum = grad[0].sum()

    print(
        f"{msg:20} {csum:12.6e} Runs: {inference_timer.ncalls} | Inference: {inference_timer.ms:10.3f} ms |"
        f" Forward: {forward_timer.ms:10.3f} ms |"
        f" Backward {backward_timer.ms:10.3f} ms | {arg.shape}"
    )


def bench(module_name, bench_name):
    """
    Import MODULE_NAME, which defines these functions:
        bench_name           Knossos-compilable code, should be pretty
        bench_name_pt        PyTorch reference, should be fast, might not be pretty
        bench_name_pt_nice   PyTorch reference, should be pretty
        bench_name_config    Return a sequence of inputs on which to run benchmarking
    """
    import inspect
    import importlib

    mod = importlib.import_module(module_name)
    for fn in inspect.getmembers(mod, inspect.isfunction):
        fn_name, fn_obj = fn
        if fn_name == bench_name + "_bench_configs":
            configs = list(fn_obj())
        elif fn_name == bench_name + "_pytorch":
            pt_fast = fn_obj
        elif fn_name == bench_name + "_pytorch_nice":
            pt_nice = fn_obj
        elif fn_name == bench_name:
            ks_raw = fn_obj
        else:
            print(f"Ignoring {fn_name}")

    # TODO: elementwise_apply
    ks_compiled = tsmod2ksmod(mod, bench_name, example_inputs=(configs[0],))

    for arg in configs:
        # ptfast should always work, and be the timing reference
        timeit(bench_name + " PyTorch fast", pt_fast, arg)

        # TODO: make ks_raw runnable as pure python
        # assert fun_and_grad_matches(pt_fast, ks_raw, arg)
        # timeit(bench_name + " Knossos raw", ks_raw, arg)

        # TODO: make pt_nice runnable with vmap
        # assert fun_and_grad_matches(pt_fast, pt_nice, arg)
        # timeit(bench_name + " PyTorch nice", pt_nice, arg)

        if ks_compiled:
            assert fun_and_grad_matches(pt_fast, ks_compiled.apply, arg)
            timeit(bench_name + " Knossos", ks_compiled.apply, arg)


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 3:
        print("Usage: run-bench MODULE BENCH")
        sys.exit(1)
    bench(sys.argv[1], sys.argv[2])
