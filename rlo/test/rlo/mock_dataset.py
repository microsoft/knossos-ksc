import numpy as np
from typing import Tuple

from rlo.dataset import RewriteId, StateValueDataset, PolicyNetDataset
from rlo import rewrites
from testutils import parse_expr_typed


def mock_policy_value_dataset() -> Tuple[PolicyNetDataset, rewrites.RuleSet]:
    rules = rewrites.get_rules("simplify_rules")
    value_dataset = mock_state_value_dataset()
    ds = PolicyNetDataset(value_dataset)
    for time_left, expr, target, (node_id, rule_name) in _loop_over_points():
        action = RewriteId(node_id, rules.id_for_rule(rewrites.rule(rule_name)))
        if time_left > 0:
            ds.add_point(time_left, expr, action, target - np.random.normal())
    return ds, rules


def mock_state_value_dataset() -> StateValueDataset:

    value_dataset = StateValueDataset.build_from_triples(  # type: ignore
        (time_left, expr, target)
        for time_left, expr, target, _action in _loop_over_points()
    )
    return value_dataset


def _loop_over_points():
    for episode in get_episodes():
        final_cost = episode[-1][-1]
        for t, (s, (node_id, rule_name), cost) in enumerate(episode):
            time_left = len(episode) - t - 1
            expr = parse_expr_typed(s)
            rule = rewrites.rule(rule_name)
            assert any(
                rewrite.node_id == node_id for rewrite in rule.get_all_rewrites(expr)
            )
            yield time_left, expr, float(cost - final_cost), (node_id, rule_name)


def get_episodes():
    return [
        [
            ("(div (div 1.0 x) (add (div 1.0 x) 1.0))", (0, "div_of_div"), 7),
            ("(div 1.0 (mul x (add (div 1.0 x) 1.0)))", (2, "mul_by_add"), 7),
            ("(div 1.0 (add (mul x (div 1.0 x)) (mul x 1.0)))", (3, "commute_mul"), 9),
            (
                "(div 1.0 (add (mul (div 1.0 x) x) (mul x 1.0)))",
                (3, "assoc_mul_div"),
                9,
            ),
            ("(div 1.0 (add (div (mul 1.0 x) x) (mul x 1.0)))", (4, "mul_one"), 9),
            ("(div 1.0 (add (div x x) (mul x 1.0)))", (3, "div_by_self"), 7),
            ("(div 1.0 (add 1.0 (mul x 1.0)))", (2, "commute_add"), 5),
        ],
        [
            ("(div (div 1.0 x) (add (div 1.0 x) 1.0))", (0, "assoc_div_div"), 7.0),
            ("(div (div 1.0 (add (div 1.0 x) 1.0)) x)", (0, "div_of_div"), 7.0),
            ("(div 1.0 (mul (add (div 1.0 x) 1.0) x))", (2, "commute_mul"), 7.0),
            ("(div 1.0 (mul x (add (div 1.0 x) 1.0)))", (2, "mul_by_add"), 7.0),
            (
                "(div 1.0 (add (mul x (div 1.0 x)) (mul x 1.0)))",
                (8, "commute_mul"),
                9.0,
            ),
            ("(div 1.0 (add (mul x (div 1.0 x)) (mul 1.0 x)))", (8, "mul_one"), 9.0),
            ("(div 1.0 (add (mul x (div 1.0 x)) x))", (3, "commute_mul"), 7.0),
            ("(div 1.0 (add (mul (div 1.0 x) x) x))", (3, "assoc_mul_div"), 7.0),
            ("(div 1.0 (add (div (mul 1.0 x) x) x))", (4, "mul_one"), 7.0),
            ("(div 1.0 (add (div x x) x))", (3, "div_by_self"), 5.0),
            ("(div 1.0 (add 1.0 x))", (2, "commute_add"), 3.0),
        ],
        [
            ("(mul x (add (div 1.0 x) 1.0))", (0, "mul_by_add"), 5),
            ("(add (mul x (div 1.0 x)) (mul x 1.0))", (1, "commute_mul"), 7),
            ("(add (mul (div 1.0 x) x) (mul x 1.0))", (1, "assoc_mul_div"), 7),
            ("(add (div (mul 1.0 x) x) (mul x 1.0))", (2, "mul_one"), 7),
            ("(add (div x x) (mul x 1.0))", (1, "div_by_self"), 5),
            ("(add 1.0 (mul x 1.0))", (2, "commute_mul"), 3),
        ],
        [
            ("(add (mul (div 1.0 x) x) (mul 1.0 x))", (6, "mul_one"), 7),
            ("(add (mul (div 1.0 x) x) x)", (1, "assoc_mul_div"), 5),
            ("(add (div (mul 1.0 x) x) x)", (2, "mul_one"), 5),
            ("(add (div x x) x)", (1, "div_by_self"), 3),
            ("(add 1.0 x)", (0, "commute_add"), 1),
        ],
    ]
