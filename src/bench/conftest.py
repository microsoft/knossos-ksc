# content of conftest.py
import pytest


def pytest_addoption(parser):
    parser.addoption(
        "--benchmarkname", action="store", help="name of benchmark to dynamically load"
    )

@pytest.fixture
def benchmarkname(request):
    return request.config.getoption("--benchmarkname")
