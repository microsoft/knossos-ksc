import pytest


def pytest_addoption(parser):
    parser.addoption("--backend", action="store", default="jax")
