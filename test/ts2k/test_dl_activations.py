import pytest
import sys
import os.path
import torch
import inspect
import importlib

from ksc.torch_frontend import tsmod2ksmod


@pytest.mark.parametrize(
    "module_file,bench_name",
    [("examples/dl-activations/relu3", "vrelu3"), ("examples/dl-capsule/sqrl", "sqrl")],
)
def test_bench(module_file, bench_name):
    """
    Import MODULE_FILE, which defines these functions:
        bench_name           Knossos-compilable code, should be pretty
        bench_name_pt        PyTorch reference, should be fast, might not be pretty
        bench_name_config    Return a sequence of inputs on which to run benchmarking
    """
    module_dir, module_name = os.path.split(module_file)
    sys.path.append(module_dir)
    mod = importlib.import_module(module_name)
    for fn in inspect.getmembers(mod, inspect.isfunction):
        fn_name, fn_obj = fn
        if fn_name == bench_name + "_bench_configs":
            configs = list(fn_obj())
        elif fn_name == bench_name + "_pytorch":
            pt_fast = fn_obj
        else:
            print(f"Ignoring {fn_name}")

    arg = configs[0]

    ks_compiled = tsmod2ksmod(mod, bench_name, example_inputs=(arg,), generate_lm=False)

    ks_compiled.py_mod.logging(True)

    pt_arg = arg.detach()
    pt_arg.requires_grad = True
    pt_value = pt_fast(pt_arg)

    ks_arg = arg.detach()
    ks_arg.requires_grad = True
    ks_value = ks_compiled.apply(ks_arg)

    assert torch.isclose(
        pt_value, ks_value, rtol=1e-05, atol=1e-06, equal_nan=False
    ).all()

    pt_loss = pt_value.sum()
    pt_grad = torch.autograd.grad(pt_loss, pt_arg)[0]

    ks_loss = ks_value.sum()
    ks_grad = torch.autograd.grad(ks_loss, ks_arg)[0]

    assert torch.isclose(
        pt_grad, ks_grad, rtol=1e-05, atol=1e-06, equal_nan=False
    ).all()
