import pytest
import json

from rlo import costs
from rlo.expression_util import find_users, first_non_let_ancestor, count_ops_along_path
from rlo.expression import Expression
from rlo import expr_sets
from rlo import sparser
from rlo import utils


def test_count_users():
    e = sparser.parse_expr(
        """
(let (x (add 1 2)) (let (y x) (add y (add x (add x (let (x (add x 3)) (add x x)))))))
; users of (x (add 1 2)):  ^              ^      ^              ^
"""
    )
    assert len(list(find_users(e))) == 4
    assert all(e.nodes[i] == Expression.Variable("x") for i in find_users(e))


def check_count_builds(expr, let_var_name, expected_num_builds):
    let_node_id, let_node = utils.single_elem(
        [
            (i, n)
            for i, n in enumerate(expr.nodes)
            if n.op == "let" and n.first.name == let_var_name
        ]
    )
    let_body_id = let_node_id + 1 + len(let_node.second.nodes)
    var_usage_id = utils.single_elem(
        [
            i
            for i, n in enumerate(expr.nodes)
            if n.op == "variable" and n.name == let_var_name and i >= let_body_id
        ]
    )
    count = count_ops_along_path(
        let_node, var_usage_id - let_node_id, ["build", "sumbuild"]
    )
    assert count == expected_num_builds, "Expected {} builds but got {}".format(
        expected_num_builds, count
    )


def test_count_builds():
    _, exprenv = utils.single_elem(
        sparser.parse_defs(
            """
(def
 matrix_matrix_multiply (Vec (Vec Float))
 ((mat_x : (Vec (Vec Float))) (mat_y : (Vec (Vec Float))))
 (let ((l (size mat_x)) (m (size mat_y)) (n (size (index 0 mat_y))))
   (build l (lam (i : Integer)
     (build n (lam (k : Integer)
       (sumbuild m (lam (j : Integer)
         (mul (index j (index i mat_x))
                 (index k (index j mat_y)))))))))))"""
        )
    )
    expected_num_builds = dict(l=1, m=3, n=2)
    for let_var_name, expected_count in expected_num_builds.items():
        check_count_builds(exprenv.expr, let_var_name, expected_count)


def test_count_builds_hard():
    _, exprenv = utils.single_elem(
        sparser.parse_defs(
            """
(def
 matrix_matrix_multiply (Vec (Vec Float))
 ((mat_x : (Vec (Vec Float))) (mat_y : (Vec (Vec Float))))
 (let ((l (size mat_x)) (m (size mat_y)) (n (size (index 0 mat_y))))
   (let ((k (size 
     (build 10 (lam (i1 : Integer)
       (build 10 (lam (i2 : Integer)
         (build 10 (lam (i3 : Integer)
           0)))))))))
     (build l (lam (i : Integer)
       (build n (lam (k : Integer)
         (sumbuild m (lam (j : Integer)
           (mul (index j (index i mat_x))
                (index k (index j mat_y))))))))))))"""
        )
    )
    expected_num_builds = dict(l=1, m=3, n=2)
    for let_var_name, expected_count in expected_num_builds.items():
        check_count_builds(exprenv.expr, let_var_name, expected_count)


def test_first_non_let_ancestor():
    e = sparser.parse_expr(
        """
(size (index 0 (let (m (size x)) (let (n (size y)) (add x y)))))
"""
    )
    add_node_id = next(i for i, n in enumerate(e.nodes) if n.op == "add")
    ancestor_node = first_non_let_ancestor(e, add_node_id)
    assert ancestor_node.op == "index"


def test_gemm_caching():
    # This is a regression test for https://github.com/awf/knossos/issues/1232 (bad caching).
    expr_set = expr_sets.get_expression_set("ksc/blas/blas_test.kso")
    exprs = dict(expr_set.named_exprenvs())

    agent = expr_set.get_expert("ml_rules_no_bind")
    episode = agent.optimize(exprs["gemm_no_inner_prod"])

    agent = expr_set.get_expert("ml_rules_no_bind")
    # Optimize gemm_all_fns_inlined first
    seen_exprs = [s.node.exprenv for s in agent.optimize(exprs["gemm_all_fns_inlined"])]
    episode_with_caching = agent.optimize(exprs["gemm_no_inner_prod"])
    # Sanity check that the two starting expressions hit some common intermediates
    common_exprs = frozenset(seen_exprs).intersection(
        frozenset([s.node.exprenv for s in episode_with_caching])
    )
    assert len(common_exprs) > 0

    # We should reach the same final expression (i.e. cost) in the same #steps.
    assert episode[-1].node.exprenv == episode_with_caching[-1].node.exprenv
    assert len(episode) == len(episode_with_caching)


@pytest.mark.parametrize(
    "file,time_budget",
    [
        ("ksc/blas/blas_combined.kso", 50),
        ("ksc/blas/blas_test.kso", 100),
        ("ksc/blas/blas_train.kso", 100),
    ],
)
@pytest.mark.parametrize("rules_name", ["ml_rules_no_bind"])
def test_expert(file, time_budget, rules_name):
    expr_set = expr_sets.get_expression_set_from_ks(file)
    best_cost = lambda expr: expr_set.best_cost_for_expression(
        expr, rules_name, time_budget
    )
    expert = expr_set.get_expert(rules_name)
    for expr_name, exprenv in expr_set.named_exprenvs():
        print("Optimizing {}".format(expr_name))
        episode = expert.optimize(exprenv, truncate=True)
        print(
            "Found sequence:",
            json.dumps([(e.action.node_id, e.action.rule_name) for e in episode[:-1]]),
        )
        final_expr = episode[-1].node.exprenv
        expert_cost = round(final_expr.cost(), 1)
        expert_ep_length = len(episode)
        print(f"{expert_ep_length} steps to cost {expert_cost} with {final_expr}")
        num_lets = len([n for n in final_expr.expr.nodes if n.op == "let"])
        (best_known_cost, method, seq) = best_cost(expr_name)
        best_ep_length = len(seq) + 1
        best_known_cost = round(best_known_cost, 1)
        if best_known_cost is not None and expert_ep_length <= time_budget:
            if method == "expert":
                # If expert_ep_length is greater than scenario time_budget, we cannot compare expert_cost with
                # best_known_cost, as it is obtained by truncating the expert episode sequence
                assert expert_cost == best_known_cost, (
                    f"For rule-set {rules_name}, "
                    "expected best cost {best_known_cost} for expression {expr_name}, "
                    "but got {expert_cost} from expert"
                )
                assert expert_ep_length == best_ep_length, (
                    "For rule-set {}, expected episode length {} "
                    "for expression {}, "
                    "but got {} from expert".format(
                        rules_name, best_ep_length, expr_name, expert_ep_length
                    )
                )
            else:
                # If expert_ep_length is less or equal to the time_budget, then anything we record, should be
                # at least as good as the expert (maybe better if RLO is working well). If expert_ep_length is greater than
                # time_budget, then we cannot say much - either expert_cost is not achievable in the given time budget,
                # hence RLO-recorded best_known_cost is greater than expert_cost, or RLO is working better than expert and it
                # has found best_known_cost that is lower than expert_cost in a smaller amount of time.
                # Test also that the expert finds all optimization possible except possibly for leaving some hanging lets.
                assert (best_known_cost <= expert_cost) and (
                    expert_cost <= best_known_cost + costs.let_cost * num_lets
                ), (
                    f"For rule-set {rules_name}, "
                    "expected best cost {best_known_cost} for expression {expr_name}, "
                    "but got {expert_cost} from expert"
                )
