import pytest
import torch
from benchmark_shim import benchmark_semi_pedantic

cpu_device = torch.device("cpu")
torch.set_default_dtype(torch.float32)


def assert_close(result, reference_result):
    assert torch.allclose(
        result, reference_result, atol=1e-5
    ), f"Result and reference differ too much {result} {reference_result}"


def test_inference(benchmark, reference_func, func, config):
    config_on_func_device = func.to_device(config)
    with torch.no_grad():
        result = benchmark_semi_pedantic(
            benchmark, func.func, config_on_func_device
        ).to(cpu_device)
        reference_result = reference_func(config)
    assert_close(result, reference_result)


def test_forward(benchmark, reference_func, func, config):
    config.requires_grad = True
    config_on_func_device = func.to_device(config)
    result = benchmark_semi_pedantic(benchmark, func.func, config_on_func_device).to(
        cpu_device
    )
    reference_result = reference_func(config)
    assert_close(result, reference_result)


def test_backwards(benchmark, reference_func, func, config):
    config.requires_grad = True
    config_on_func_device = func.to_device(config)

    def create_fresh_args():
        loss = func.func(config_on_func_device).sum()
        return (loss, config), {}

    # We need to use pedantic mode to have a fresh arguments via a setup function,
    # but that means we need to specify rounds ourselves.
    # TODO: Investigate more
    result = benchmark_semi_pedantic(
        benchmark, torch.autograd.grad, setup=create_fresh_args
    )

    reference_loss = reference_func(config).sum()
    reference_result = torch.autograd.grad(reference_loss, config)
    assert_close(result[0].to(cpu_device), reference_result[0])

