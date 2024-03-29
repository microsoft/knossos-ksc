import time
import os
import psutil
import torch


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


all_messages = []


def timeit(msg, fn, arg):
    MAX_TIME = 5  # No no need to run beyond MAX_TIME sec to get accurate benchmarks
    end_time = time.time() + MAX_TIME
    inference_timer = time_sampler()
    forward_timer = time_sampler()
    backward_timer = time_sampler()
    nruns = 5000

    mem_used_start = psutil.Process(os.getpid()).memory_info().rss / 1024 ** 2
    print(f"run-bench: Memory {mem_used_start} before {msg}")

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
            print(f"# Ran to timeout: {msg} ")
            break

    csum = grad[0].sum()

    mem_used_end = psutil.Process(os.getpid()).memory_info().rss / 1024 ** 2
    print(f"run-bench: Memory {mem_used_end} after {msg}")

    mem_used = mem_used_end - mem_used_start
    shape_str = "x".join([str(x) for x in arg.shape])
    msg = (
        f"{msg:20} {csum:12.5e} Runs: {inference_timer.ncalls:4d} | Inference: {inference_timer.ms:10.3f} ms |"
        f" Forward: {forward_timer.ms:10.3f} ms |"
        f" Backward {backward_timer.ms:10.3f} ms |"
        f" Memory {mem_used:10.3f} MB |"
        f" {shape_str}"
    )
    print(msg)
    all_messages.append(msg)


def bench(module_file, bench_name):
    """
    Import MODULE_NAME, which defines these functions:
        bench_name           Knossos-compilable code, should be pretty
        bench_name_pt        PyTorch reference, should be fast, might not be pretty
        bench_name_pt_nice   PyTorch reference, should be pretty
        bench_name_config    Return a sequence of inputs on which to run benchmarking
    """
    import inspect
    import importlib
    import os.path
    import sys

    module_dir, module_name = os.path.split(module_file)
    sys.path.append(module_dir)
    mod = importlib.import_module(module_name)
    for fn in inspect.getmembers(mod):
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

    ks_compiled = ks_raw.compile(
        torch_extension_name="ksc_run_bench_" + bench_name,
        example_inputs=(configs[0],),
    )

    for arg in configs:
        pt_arg = arg.detach()
        pt_arg.requires_grad = True
        pt_value = pt_fast(pt_arg)

        ks_arg = arg.detach()
        ks_arg.requires_grad = True
        ks_value = ks_compiled.apply(ks_arg)

        if (
            not torch.isclose(
                pt_value, ks_value, rtol=1e-05, atol=1e-06, equal_nan=False
            )
            .all()
            .numpy()
        ):
            print(pt_value)
            print(ks_value)
            raise ValueError("Knossos != torch")

        pt_loss = pt_value.sum()
        pt_grad = torch.autograd.grad(pt_loss, pt_arg)[0]

        ks_loss = ks_value.sum()
        ks_grad = torch.autograd.grad(ks_loss, ks_arg)[0]

        if (
            not torch.isclose(pt_grad, ks_grad, rtol=1e-05, atol=1e-05, equal_nan=False)
            .all()
            .numpy()
        ):
            import pandas as pd

            cols = (
                torch.stack((arg.detach(), pt_grad, ks_grad, pt_grad - ks_grad))
                .t()
                .numpy()
            )

            print(pd.DataFrame(cols, columns=["ARG", "PT", "KS", "Diff"]))
            raise ValueError("Knossos != torch")

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
    import argparse
    import ksc.utils

    parser = argparse.ArgumentParser()

    parser.add_argument("module")
    parser.add_argument("bench")
    parser.add_argument("-p", "--preserve-temporary-files", action="store_true")

    args = parser.parse_args()

    if args.preserve_temporary_files:
        print("run-bench: Will preserve temporary files")
        ksc.utils.preserve_temporary_files = True
    bench(args.module, args.bench)

    print("==================================")
    print(*all_messages, sep="\n")
