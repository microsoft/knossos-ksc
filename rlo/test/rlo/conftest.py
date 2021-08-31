# fmt: off
import os
import pytest
import sys
sys.path.append(os.path.join(os.path.dirname(__file__), os.pardir, os.pardir, "src"))
os.environ["TF_DETERMINISTIC_OPS"] = "1"

def pytest_addoption(parser):
    parser.addoption("--runslow", action="store_true", default=False, help="run slow tests")
    parser.addoption("--quick", action="store_true", default=False, help="Quick test: avoid those marked notquick")
    parser.addoption("--with-azurecli", action="store_true", default=False, help="Run tests marked azurecli")

def pytest_collection_modifyitems(config, items):
    skip_slow = pytest.mark.skip(reason="need --runslow option to run")
    skip_q = pytest.mark.skip(reason="test marked notquick and --quick specified")
    skip_azurecli = pytest.mark.skip(reason="test marked azurecli and --with-azurecli not specified")
    for item in items:
        if "slow" in item.keywords and not config.getoption("--runslow"):
            item.add_marker(skip_slow)
        if "notquick" in item.keywords and config.getoption("--quick"):
            item.add_marker(skip_q)
        if "azurecli" in item.keywords and not config.getoption("--with-azurecli"):
            item.add_marker(skip_azurecli)
