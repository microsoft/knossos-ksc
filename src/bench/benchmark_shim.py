from math import ceil
import sys

# pytest-benchmark has a simple benchmark() function and a pedantic() function
# we want an in-between, where we can give a setup function
# but allow "rounds" to be automatically determined
# TODO: potentially propose adding an optional setup function to the simple benchmark() function,
# but it's messy as you can't pass args AND setup.
def benchmark_semi_pedantic(
    benchmark, function_to_benchmark, *args, setup=None, **kwargs
):

    has_args = bool(args or kwargs)

    def make_arguments(args=args, kwargs=kwargs):
        if setup:
            maybe_args = setup()
            if maybe_args:
                if has_args:
                    raise TypeError(
                        "Can't use `args` or `kwargs` if `setup` returns the arguments."
                    )
                args, kwargs = maybe_args
        return args, kwargs

    runner_args, runner_kwargs = make_arguments()

    # using internal functions to match pytest-benchmark.
    # we might want to just go our own direction in terms of logic
    # https://github.com/ionelmc/pytest-benchmark/blob/996dbe519b5bcc9b103ea0e4aeb232c58b71fc8c/src/pytest_benchmark/fixture.py#L147-L154
    runner = benchmark._make_runner(
        function_to_benchmark, args=runner_args, kwargs=runner_kwargs
    )

    duration, iterations, _ = benchmark._calibrate_timer(runner)

    # Choose how many time we must repeat the test
    rounds = int(ceil(benchmark._max_time / duration))
    rounds = max(rounds, benchmark._min_rounds)
    rounds = min(rounds, sys.maxsize)

    if setup is None:
        return benchmark.pedantic(
            function_to_benchmark,
            rounds=rounds,
            iterations=iterations,
            args=args,
            kwargs=kwargs,
        )
    else:
        return benchmark.pedantic(
            function_to_benchmark, rounds=rounds, iterations=iterations, setup=setup
        )
