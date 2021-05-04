# set up for global PyTest
import pytest
import importlib
import inspect
from collections import namedtuple

from ts2ks import ts2mod


def pytest_addoption(parser):
    parser.addoption("--modulename", action="store", help="name of benchmark to dynamically load")
    parser.addoption("--benchmarkname", action="store", help="name of benchmark to dynamically load")


@pytest.fixture
def benchmarkname(request):
    return request.config.getoption("--benchmarkname")


@pytest.fixture
def modulename(request):
    return request.config.getoption("--modulename")


BenchmarkFunction = namedtuple("BenchmarkFunction", "name func")


def functions_to_benchmark(mod, benchmark_name, example_input):
    for fn in inspect.getmembers(mod, lambda m: inspect.isfunction(m) and m.__name__.startswith(benchmark_name)):
        fn_name, fn_obj = fn
        if fn_name == benchmark_name + "_bench_configs":
            continue
        elif fn_name == benchmark_name + "_pytorch":
            yield BenchmarkFunction("PyTorch", fn_obj)
        elif fn_name == benchmark_name + "_pytorch_nice":
            yield BenchmarkFunction("PyTorch Nice", fn_obj)
        elif fn_name == benchmark_name:
            yield BenchmarkFunction("Knossos", ts2mod(fn_obj, example_input).apply)
        else:
            # perhaps we should just allow anything that matches the pattern?
            # would make it easier to add arbitrary comparisons e.g. TF
            print(f"Ignoring {fn_name}")


def func_namer(benchmark_func):
    return benchmark_func.name


def config_namer(config):
    return str(config.shape)


def pytest_generate_tests(metafunc):
    if "func" in metafunc.fixturenames:

        module_name = metafunc.config.getoption("benchmarkname")
        benchmark_name = metafunc.config.getoption("benchmarkname")

        mod = importlib.import_module(module_name)

        configs = list(getattr(mod, benchmark_name + "_bench_configs")())

        example_inputs = (configs[0],)

        metafunc.parametrize(
            "func", functions_to_benchmark(mod, benchmark_name, example_inputs), ids=func_namer,
        )

        # We want to group by tensor size, it's not clear how to metaprogram the group mark cleanly.
        # pytest meta programming conflates arguments and decoration. I've not been able to find a way to directly
        # parameterize just marks so do the mark along with a oarameter
        config_and_group_marker = [
            pytest.param(config, marks=[pytest.mark.benchmark(group=str(config.shape))]) for config in configs
        ]
        metafunc.parametrize("config", config_and_group_marker, ids=config_namer)

        # Alternative use a dummy argument --benchmark-group-by=param:dummy_argument but messes with serialised versions and is just horrible

        # https://github.com/pytest-dev/pytest/issues/1425#issuecomment-439220834
        # This runs, but doesn't affect the test. It seems like they've allowed FunctionDefinition to be mutable (due to bad inheritance)
        # group_marker = pytest.mark.benchmark(group="groupname_XYZ")
        # metafunc.definition.add_marker(group_marker)
