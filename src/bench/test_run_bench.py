import pytest

def test_benchmark(benchmark, func, config):
    # any assertion we can make about result?
    result = benchmark(func.func, config)
