# content of conftest.py
import pytest
import importlib
import inspect

from ts2ks import ts2mod

def pytest_addoption(parser):
    parser.addoption(
        "--benchmarkname", action="store", help="name of benchmark to dynamically load"
    )

@pytest.fixture
def benchmarkname(request):
    return request.config.getoption("--benchmarkname")


def pytest_generate_tests(metafunc):
    if "func" in metafunc.fixturenames:

        benchmark_name = metafunc.config.getoption('benchmarkname')

        mod = importlib.import_module(benchmark_name)

        functions_to_benchmark = []

        for fn in inspect.getmembers(mod, inspect.isfunction):
            fn_name, fn_obj = fn
            if fn_name == benchmark_name + '_bench_configs':
                configs = list(fn_obj())
            elif fn_name == benchmark_name + '_pt':
                pt_fast = fn_obj
                functions_to_benchmark.append(pt_fast)
            elif fn_name == benchmark_name + '_pt_nice':
                pt_nice = fn_obj
                functions_to_benchmark.append(pt_nice)
            elif fn_name == benchmark_name:
                ks_fun = fn_obj
                functions_to_benchmark.append(ks_fun)
            else:
                print(f"Ignoring {fn_name}")

        ks_compiled = ts2mod(ks_fun, example_inputs=(configs[0],))
        functions_to_benchmark.append(ks_compiled.apply)

        metafunc.parametrize("func", functions_to_benchmark)
        metafunc.parametrize("config", configs)