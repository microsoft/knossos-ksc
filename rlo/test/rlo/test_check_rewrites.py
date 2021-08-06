# fmt: off
import numpy as np
import os
import pytest
import random
import tempfile

# importing from src/check_rewrites.py
from check_rewrites import check_rewrites
from rlo import expr_sets
from rlo import utils
from testutils import parse_expr_typed, parse_best_episodes_file

def mock_input_expr(expr_str):
    return lambda choices_fn: utils.single_elem(choices_fn(expr_str).values())

def mock_select_by_name(name):
    return lambda choices_fn: choices_fn("")[name]

def mock_select_random_rewrite(choices_fn):
    choices = list(choices_fn("").values())
    return choices[random.randint(0, len(choices)-1)]

def sequence_input_funcs(input_funcs):
    """ Returns a function that
           - is like (the first element of input_funcs) the first time that it's called
           - is like (the second element of input_funcs) the second time that it's called, etc.
           - just returns None (ignoring its arguments) when there are no more elements of input_funcs
    """
    it = iter(input_funcs)
    def choose(*args):
        try:
            input_func = next(it)
        except StopIteration:
            return None
        return input_func(*args)
    return choose


@pytest.mark.parametrize("file, time_left", [
    ("ksc/blas/blas_combined.kso", 50),
    ("ksc/blas/blas_test.kso", 100)])
@pytest.mark.parametrize("rules_name", ["ml_rules_no_bind"])
@pytest.mark.parametrize("expr_name", ["axpy"])
@pytest.mark.parametrize("input_method", ["file", "ksc_str", "str"])
def test_check_rewrites(file, time_left, rules_name, expr_name, input_method):
    expr_set = expr_sets.get_expression_set(file)
    exprenv = dict(expr_set.named_exprenvs())[expr_name]
    if input_method == "file":
        expr_input_func = mock_select_by_name(expr_name)
    else:
        file = None
        if input_method == "ksc_str":
            expr_input_func = mock_input_expr(exprenv.expr.ksc_str())
        elif input_method == "str":
            expr_input_func = mock_input_expr(str(exprenv.expr))
        else:
            raise ValueError("Unknown input_method: {}".format(input_method))
    input_func = sequence_input_funcs([expr_input_func] + ([mock_select_random_rewrite] * 50))
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpfile = os.path.join(tmpdir, "log.kso")
        check_rewrites(file, None, rules_name, input_func, tmpfile, time_left=time_left)
        best_ep = utils.single_elem(parse_best_episodes_file(tmpfile).values())
    recorded_costs = best_ep.costs
    recorded_actions = best_ep.actions
    for (node_id, rule), recorded_cost in zip(recorded_actions, recorded_costs):
        print("current cost={}, recorded cost={}".format(exprenv.cost(), recorded_cost))
        assert np.isclose(exprenv.cost(), recorded_cost, rtol=1e-3)
        # Find the corresponding Rewrite object. This may be inefficient (discovering all actions before filtering);
        # it would be better to pass the desired node_id into get_local_rewrites, but right now this is considered OK for a test.
        rewrite = utils.single_elem([rw for rw in rule.get_all_rewrites(exprenv) if rw.node_id == node_id])
        exprenv = rewrite.apply(exprenv)
    assert np.allclose(exprenv.cost(), recorded_costs[-1], rtol=1e-3)

def test_check_rewrites_asum():
    # asum uses "abs" from the symtab, so tests a case not tested by axpy.
    # However, this will work (find the edef) only for input_method="file", hence the separate test.
    test_check_rewrites("ksc/blas/blas_test.kso", 100, "ml_rules_no_bind", "asum", "file")

def test_check_rewrites_named():
    sequence = ["mul_by_add@0", "commute_mul@6", "mul_one@6", "commute_mul@1", "assoc_mul_div@1", "mul_one@2", "div_by_self@1"]
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpfile = os.path.join(tmpdir, "log.kso")
        input_func = sequence_input_funcs(
            [mock_input_expr("(mul x (add (div 1.0 x) 1.0))")] + [mock_select_by_name(action) for action in sequence])
        check_rewrites(file=None, expert_name=None, rules_name="ml_rules", input_func=input_func, output_file=tmpfile)
        best_ep = utils.single_elem(parse_best_episodes_file(tmpfile).values())
    assert len(best_ep.actions) == len(sequence)
    for (node_id, rule), requested in zip(best_ep.actions, sequence):
        assert f"{rule.name}@{node_id}" == requested
    assert best_ep.costs[-1] == parse_expr_typed("(add x 1.0)").cost()
