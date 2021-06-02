# set up for global PyTest
import pytest
import importlib
import inspect
import torch
import os
import sys
from pathlib import Path
from collections import namedtuple
from contextlib import contextmanager

from ts2ks import ts2mod
from ksc import utils


def pytest_addoption(parser):
    parser.addoption(
        "--modulepath", action="store", help="path of module to dynamically load"
    )
    parser.addoption(
        "--benchmarkname", action="store", help="name of benchmark to dynamically load"
    )


@pytest.fixture
def benchmarkname(request):
    return request.config.getoption("--benchmarkname")


@pytest.fixture
def modulepath(request):
    return request.config.getoption("--modulepath")


BenchmarkFunction = namedtuple("BenchmarkFunction", "name func")


def functions_to_benchmark(mod, benchmark_name, example_input):
    for fn in inspect.getmembers(
        mod, lambda m: inspect.isfunction(m) and m.__name__.startswith(benchmark_name)
    ):
        fn_name, fn_obj = fn
        if fn_name == benchmark_name + "_bench_configs":
            continue
        elif fn_name == benchmark_name + "_pytorch":
            yield BenchmarkFunction("PyTorch", fn_obj)
        elif fn_name == benchmark_name + "_pytorch_nice":
            yield BenchmarkFunction("PyTorch Nice", fn_obj)
        elif fn_name == benchmark_name:
            yield BenchmarkFunction("Knossos", ts2mod(fn_obj, example_input).apply)
        elif fn_name == benchmark_name + "_cuda_init":
            if torch.cuda.is_available():
                yield BenchmarkFunction("PyTorch CUDA", fn_obj())
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
