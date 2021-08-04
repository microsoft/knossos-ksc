"""
Test script for utils.py, not to be confused with utilities for tests in testutils.py
"""
import numpy as np
import pytest

from rlo.config_utils import unify_configs
from rlo import utils
from rlo.reporting import azureml


def test_random_state():
    np.random.seed(123)
    with utils.random_state_context(234):
        # stuff in here doesn't affect the outer random state
        np.random.uniform()
    x = np.random.uniform()
    np.random.seed(123)
    np.testing.assert_equal(x, np.random.uniform())


def test_azureml_context():
    run = azureml.get_current_context()
    run.log("metric1", 1.23)
    run.log("metric2", value=3.14)


@pytest.mark.parametrize(
    "expr_str",
    [
        "(add x y)",
        "(div (div 1.0 x) (add 1.0 (div 1.0 x)))",
        "(add (add (div x (sub y 1.0)) 1.0) (div (add (add x y) (div x (sub y 1.0))) 2.0))",
    ],
)
@pytest.mark.parametrize("expr_limit", [10, 50, 100])
def test_format_expr(expr_str, expr_limit):
    formatted_expr_str = utils.format_expr(expr_str, expr_limit)
    assert len(formatted_expr_str) <= expr_limit
    if len(expr_str) <= expr_limit:
        assert formatted_expr_str == expr_str


@pytest.mark.parametrize("alt_a_value", [0, 1])
def test_unify_configs(alt_a_value):
    config1 = {"a": 0, "b": [1, 2, 3], "c": 4}
    config2 = {"a": alt_a_value, "b": [1, 2, 3], "c": 5}
    if alt_a_value == config1["a"]:
        unify_configs([config1, config2], ["a", "b"])
    else:
        with pytest.raises(ValueError):
            unify_configs([config1, config2], ["a", "b"])


@pytest.mark.parametrize("allow_renames", [{}, {"c1": "c", "c2": "c"}])
def test_unify_configs_with_renames(allow_renames):
    config1 = {
        "a": 0,
        "b": [1, 2, 3],
        "c": 4,
    }
    config2 = {"a": 1, "b": [1, 2, 3], "c1": 4, "c2": 4}
    if len(allow_renames) > 0:
        unify_configs([config1, config2], ["b", "c1", "c2"], allow_renames)
    else:
        with pytest.raises(ValueError):
            unify_configs([config1, config2], ["b", "c1", "c2"], allow_renames)
