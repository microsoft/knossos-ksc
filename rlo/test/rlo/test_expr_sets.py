import json

from rlo import expr_sets
from testutils import scenario_path


def test_generalization_gmm():
    with open(scenario_path("generalization_gmm")) as f:
        scenario = json.load(f)
    test_exprs = expr_sets.get_expression_set(
        scenario["test_exprs"], take_defs=scenario["test_on_defs"]
    )
    assert len(test_exprs) == 1
    assert test_exprs.named_exprenvs()[0].name == "rev$gmm_knossos_gmm_objective"
