import pytest

from rlo.analytics import LogEventsToList
from rlo.factory import (
    astar_searcher_from_config,
    beam_searcher_from_config,
    num_time_heads_from_config,
)
from rlo.expression import Expression
from rlo.expr_sets import get_expression_set
from rlo import rewrites
from rlo import summations
from rlo.flags import get_scenario_name_and_defaults
from rlo import utils
from ksc.type import Type

from testutils import scenario_path


def test_collect_terms():
    x1 = Expression.Variable("x1", Type.Integer)
    x2 = Expression.Variable("x2", Type.Integer)
    x3 = Expression.Variable("x3", Type.Integer)
    x4 = Expression.Variable("x4", Type.Integer)

    expressions = [
        (1 + x1 + x2 + 1, 2 + x1 + x2),
        (1 + x1 + x2 + x3 + x4 + 2, 3 + x1 + x2 + x3 + x4),
        (x1 + x2 + x3, x1 + x2 + x3),
        (1 - x1 - x2 + 1, 2 - (x1 + x2)),
        (1 + x1 - x2 - 1, x1 - x2),
        (1 - (x1 + x2 + 1), 0 - (x1 + x2)),
        (x1 + x1 + x1, 3 * x1),
        (x1 + x2 + x1 + x2 + x1 - x2, x2 + 3 * x1),
        (2 + x1 + x2 + x2 + x1, 2 + 2 * (x1 + x2)),
    ]

    for expr in expressions:
        assert (
            summations.optimize_by_collecting_terms(expr[0]).cost()
            == summations._symtab_and_defs.make_toplevel(expr[1]).cost()
        )


@pytest.mark.parametrize("scenario", ["summations", "summations20"])
def test_expert_solves_exprs(scenario):
    _, defaults = get_scenario_name_and_defaults(scenario_path(scenario))
    es = get_expression_set(defaults["test_exprs"])
    expert = summations.SummationsExpert()
    for _, exprenv in es.named_exprenvs():
        seq = expert.get_sequence(exprenv)
        assert (
            seq[-1].cost()
            == summations.optimize_by_collecting_terms(exprenv.expr).cost()
        )
        assert len(seq) <= defaults["simulation_depth_train"]
        assert len(seq) <= defaults["simulation_depth_eval"]
        for p, n in zip([exprenv] + seq, seq):
            assert n in [
                rw.apply(p) for rw in rewrites.simplify_rules.get_all_rewrites(p)
            ]


def test_expert_values():
    x1 = Expression.Variable("x1", Type.Integer)
    e = summations._symtab_and_defs.make_toplevel(1 + x1 + 2 + 3)
    expert = summations.SummationsExpert(10)
    assert expert.get_sequence_expr(e.expr) == [
        Expression.Constant(1) + 2 + x1 + 3,
        3 + x1 + 3,
        Expression.Constant(3) + 3 + x1,
        6 + x1,
    ]
    values = utils.single_elem(expert.evaluate_all_time_left([e]))
    assert values.tolist() == [0, 0, 1, 1, 2, 2, 2, 2, 2, 2]
    assert len(values) == 10  # num_heads passed into constructor


def do_expert_search(scenario, searcher_factory, **searcher_kwargs):
    _, defaults = get_scenario_name_and_defaults(scenario)
    es = get_expression_set(defaults["test_exprs"])
    expert = summations.SummationsExpert(num_time_heads_from_config(defaults))
    searcher = searcher_factory({**defaults, **searcher_kwargs}, phase="eval")
    for expr in es.named_exprenvs():
        exprenv = expr.exprenv
        with LogEventsToList() as lst:
            searcher.eval(expert, None, expr)
        rewrite_seq_costs = utils.single_elem(lst.log_items)["best_ep"][1]
        _action, cost = rewrite_seq_costs[-1]
        yield (
            exprenv,
            cost == summations.optimize_by_collecting_terms(exprenv.expr).cost(),
            len(rewrite_seq_costs) - 1
            <= len(expert.get_sequence(exprenv)),  # Skip initial expr
        )


@pytest.mark.notquick
@pytest.mark.parametrize(
    "scenario, cost_per_step, successes",
    [
        ("summations", None, 100),
        # A number of expressions fail due to pruning when search takes useless steps
        # (before reaching the frontier of optimizability) that can't then be reverted
        # (without backing up to a greater time-left). search_batch_size==1 adds 1 success.
        # ("summations20", None, 83),
        ("summations20", 0.03, 100),
    ],
)
def test_expert_beam_search(scenario, cost_per_step, successes):
    success_count = 0
    for e, success, _in_steps in do_expert_search(
        scenario_path(scenario), beam_searcher_from_config, cost_per_step=cost_per_step,
    ):
        if success:
            print("SUCCESS", e)
            success_count += 1
        else:
            print("FAILURE", e)
    if successes is not None:
        assert successes == success_count


@pytest.mark.notquick
@pytest.mark.parametrize(
    "scenario, cost_per_step, successes",
    [
        # ("summations20", None, 0),
        # ("summations20", 0.1, 76),
        ("summations20", 0.01, 100),
        # ("summations", None, 52),
        #    ("summations", 0.3, 83),
        # ("summations", 0.1, 99),
        ("summations", 0.05, 100),
    ],
)
def test_expert_astar_search_cost_per_step(scenario, cost_per_step, successes):
    success_count = 0
    for e, success, in_steps in do_expert_search(
        scenario_path(scenario),
        astar_searcher_from_config,
        cost_per_step=cost_per_step,
    ):
        if success and in_steps:
            print("SUCCESS", e)
            success_count += 1
        else:
            print("FAILURE", e)
    if successes is not None:
        assert successes == success_count
