# set up for global PyTest
from dataclasses import dataclass, field
import pytest
import importlib
import inspect
import torch
import os
from pathlib import Path
from collections import namedtuple
from contextlib import contextmanager
from typing import Callable

from ksc.torch_frontend import tsmod2ksmod
from ksc import utils


def pytest_addoption(parser):
    parser.addoption(
        "--modulepath",
        action="store",
        default="examples/dl-activations/relu3",
        help="path of module to dynamically load",
    )
    parser.addoption(
        "--benchmarkname",
        action="store",
        default="vrelu3",
        help="name of benchmark to dynamically load",
    )


@pytest.fixture
def benchmarkname(request):
    return request.config.getoption("--benchmarkname")


@pytest.fixture
def modulepath(request):
    return request.config.getoption("--modulepath")


@dataclass(frozen=True)
class BenchmarkFunction:
    name: str
    func: Callable
    device: torch.device = field(default=torch.device("cpu"))

    def to_device(self, input: torch.Tensor):
        if self.device != torch.device("cpu"):
            return input.to(self.device)
        else:
            return input


def function_to_torch_benchmarks(func):
    yield BenchmarkFunction("PyTorch", func)

    if torch.cuda.is_available():

        def benchmark_without_transfers(x: torch.Tensor):
            ret = func(x)
            torch.cuda.synchronize()
            return ret

        yield BenchmarkFunction(
            "PyTorch CUDA", benchmark_without_transfers, torch.device("cuda"),
        )


def function_to_manual_cuda_benchmarks(func):
    cuda_device = torch.device("cuda")
    cpu_device = torch.device("cpu")
    torch_module = func()
    # Note we're assuming this has been implemented as a module hence .to(), may need to generalise later
    # https://pytorch.org/docs/stable/generated/torch.nn.Module.html?highlight=#torch.nn.Module.to
    torch_module.to(cuda_device)

    def benchmark_with_transfers(x: torch.Tensor):
        ondevice = x.to(cuda_device)
        ret = torch_module(ondevice)
        torch.cuda.synchronize()
        return ret.to(cpu_device)

    def benchmark_without_transfers(x: torch.Tensor):
        ret = torch_module(x)
        torch.cuda.synchronize()
        return ret

    yield BenchmarkFunction("Manual CUDA (with transfer)", benchmark_with_transfers)
    yield BenchmarkFunction(
        "Manual CUDA", benchmark_without_transfers, cuda_device,
    )


def functions_to_benchmark(mod, benchmark_name, example_inputs):
    for fn_name, fn_obj in inspect.getmembers(mod, lambda m: inspect.isfunction(m)):
        if fn_name.startswith(benchmark_name):
            if fn_name == benchmark_name + "_bench_configs":
                continue
            elif fn_name == benchmark_name + "_pytorch":
                yield from function_to_torch_benchmarks(fn_obj)
            elif fn_name == benchmark_name + "_pytorch_nice":
                yield BenchmarkFunction("PyTorch Nice", fn_obj)
            elif fn_name == benchmark_name:
                ks_mod = tsmod2ksmod(
                    mod, benchmark_name, example_inputs, generate_lm=False
                )
                yield BenchmarkFunction("Knossos", ks_mod.apply)
            elif fn_name == benchmark_name + "_cuda_init":
                if torch.cuda.is_available():
                    yield from function_to_manual_cuda_benchmarks(fn_obj)
            elif fn_name.startswith(benchmark_name + "_embedded_"):
                n = len(benchmark_name + "_embedded_")
                benchmark_display_name = "Embedded " + fn_name[n:]
                yield BenchmarkFunction(benchmark_display_name, fn_obj().apply)
            else:
                # perhaps we should just allow anything that matches the pattern?
                # would make it easier to add arbitrary comparisons e.g. TF
                print(f"Ignoring {fn_name}")


def func_namer(benchmark_func):
    return benchmark_func.name


def config_namer(config):
    return str(config.shape)


def pytest_configure(config):
    module_path = config.getoption("modulepath")
    benchmark_name = config.getoption("benchmarkname")

    module_dir, module_name = os.path.split(module_path)

    with utils.add_to_path(module_dir):
        mod = importlib.import_module(module_name)

        configs = list(getattr(mod, benchmark_name + "_bench_configs")())

        example_inputs = (configs[0],)

        config.reference_func = getattr(mod, benchmark_name + "_pytorch")
        config.functions_to_benchmark = list(
            functions_to_benchmark(mod, benchmark_name, example_inputs)
        )
        # We want to group by tensor size, it's not clear how to metaprogram the group mark cleanly.
        # pytest meta programming conflates arguments and decoration. I've not been able to find a way to directly
        # parameterize just marks so do the mark along with a oarameter
        config.config_and_group_marker = [
            pytest.param(config, marks=[pytest.mark.benchmark(group=str(config.shape))])
            for config in configs
        ]

        # Alternative use a dummy argument --benchmark-group-by=param:dummy_argument but messes with serialised versions and is just horrible


def pytest_generate_tests(metafunc):
    print("pytest_generate_tests")
    if "func" in metafunc.fixturenames:
        config = metafunc.config
        metafunc.parametrize(
            "reference_func", [config.reference_func],
        )

        metafunc.parametrize(
            "func", config.functions_to_benchmark, ids=func_namer,
        )

        metafunc.parametrize("config", config.config_and_group_marker, ids=config_namer)

        # https://github.com/pytest-dev/pytest/issues/1425#issuecomment-439220834
        # This runs, but doesn't affect the test. It seems like they've allowed FunctionDefinition to be mutable (due to bad inheritance)
        # group_marker = pytest.mark.benchmark(group="groupname_XYZ")
        # metafunc.definition.add_marker(group_marker)
