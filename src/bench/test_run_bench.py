import pytest
import torch
import time


def test_forward(benchmark, func, config):
    # any assertion we can make about result?
    result = benchmark(func.func, config)


def test_backwards(benchmark, func, config):

    def create_fresh_args():
        config.requires_grad = True
        loss = func.func(config).sum()
        return (loss, config), {}

    # We need to use pedantic mode to have a fresh arguments via a setup function,
    # but that means we need to specify rounds ourselves.
    # TODO: Investigate more 
    result = benchmark.pedantic(torch.autograd.grad, setup=create_fresh_args, rounds=500)