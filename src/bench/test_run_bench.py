import pytest
import torch
import time


def test_inference(benchmark, reference_func, func, config):
    result = benchmark(func.func, config)
    reference_result = reference_func(config)
    assert torch.allclose(result, reference_result), f"Result and reference differ too much {result} {reference_result}"

def test_forward(benchmark, reference_func, func, config):
    config.requires_grad = True
    result = benchmark(func.func, config)
    reference_result = reference_func(config)
    assert torch.allclose(result, reference_result), f"Result and reference differ too much {result} {reference_result}"


def test_backwards(benchmark, reference_func, func, config):
    config.requires_grad = True

    def create_fresh_args():
        loss = func.func(config).sum()
        return (loss, config), {}

    # We need to use pedantic mode to have a fresh arguments via a setup function,
    # but that means we need to specify rounds ourselves.
    # TODO: Investigate more 
    result = benchmark.pedantic(torch.autograd.grad, setup=create_fresh_args, rounds=1000)
    reference_loss = reference_func(config).sum()
    reference_result = torch.autograd.grad(reference_loss, config)
    assert torch.allclose(result[0], reference_result[0]), f"Result and reference differ too much {result} {reference_result}"