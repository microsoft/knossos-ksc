import pytest

# @pytest.mark.parametrize
def test_benchmark_sqrl(benchmarkname, benchmark):

    assert benchmarkname == "sqrl"

    from sqrl import sqrl_pt, sqrl_bench_configs

    # can't call benchmark multiple times, need to use parameterization
    # https://github.com/ionelmc/pytest-benchmark#examples
    #for arg in sqrl_bench_configs():
    #    result = benchmark(sqrl_pt, x=arg)

    config = list(sqrl_bench_configs())

    result = benchmark(sqrl_pt, x=config[0])

    

    # random values so more involved to test results
    #assert result == 0.00125
