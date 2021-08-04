from tabulate import tabulate
import os
import json
import numpy as np

from rlo.agent import Agent
from rlo import expr_sets
from rlo.expression_util import (
    get_node,
    find_users,
    first_non_let_ancestor,
    count_ops_along_path,
)
from rlo import rewrites


class Expert(Agent):
    DONT_APPLY = 0
    DEFAULT_PRIORITY = 1
    UNCONDITIONAL_PRIORITY = 10

    def choose_action(self, node):
        priority_rewrites = [
            (pri, rewrite)
            for rewrite in node.actions
            for pri in [self.get_priority(node.exprenv, rewrite)]
            if pri > Expert.DONT_APPLY
        ]
        if len(priority_rewrites) > 0:
            return max(priority_rewrites, key=lambda x: x[0])[1]
        return None

    def get_priority(self, exprenv, rewrite):
        handler_name = "handle_" + rewrite.rule_name
        if hasattr(self, handler_name):
            handler = getattr(self, handler_name)
            return handler(exprenv, rewrite)
        return Expert.DONT_APPLY

    def get_min_cost_length(self, expr, time_budget=float("inf")):
        episode = self.optimize(expr, time_left=time_budget, truncate=True)
        final_expr = episode[-1].node.exprenv
        return final_expr.cost(), len(episode)


def unconditionally_apply(*rule_names):
    def wrapper(cls):
        for rule_name in rule_names:
            setattr(
                cls, "handle_" + rule_name, lambda *args: Expert.UNCONDITIONAL_PRIORITY
            )
        return cls

    return wrapper


def is_zero_valued(expr):
    return expr.op == "constant" and expr.value == 0.0


@unconditionally_apply(
    "delete_let",
    "select_of_tuple",
    "index_of_build",
    "size_of_build",
    "inline_call",
    "add_of_muls",
    "mul_zero",
    "add_zero",
)
class BlasExpert(Expert):
    @staticmethod
    def handle_lift_bind(exprenv, rewrite):
        parent_node = get_node(exprenv.expr, rewrite._parent_id)
        if parent_node.op == "let":
            return Expert.DONT_APPLY
        return Expert.DEFAULT_PRIORITY

    @staticmethod
    def handle_assoc_add_sub(exprenv, rewrite):
        add_node = get_node(exprenv.expr, rewrite.node_id, "add")
        sub_node = add_node.left
        assert sub_node.op == "sub"
        if is_zero_valued(sub_node.left):
            return Expert.DEFAULT_PRIORITY
        return Expert.DONT_APPLY

    @staticmethod
    def handle_commute_mul(exprenv, rewrite):
        mul_node = get_node(exprenv.expr, rewrite.node_id, "mul")
        left, right = mul_node.children
        if right.op == "constant" and right.value == 0.0:
            return Expert.DEFAULT_PRIORITY
        # Enable (0 - b) * a => a * (0 - b)
        if left.op == "sub" and is_zero_valued(left.left):
            return Expert.DEFAULT_PRIORITY
        return Expert.DONT_APPLY

    @staticmethod
    def handle_mul_by_sub(exprenv, rewrite):
        mul_node = get_node(exprenv.expr, rewrite.node_id, "mul")
        sub_node = mul_node.right
        assert sub_node.op == "sub"
        if is_zero_valued(sub_node.left):
            return Expert.DEFAULT_PRIORITY
        return Expert.DONT_APPLY

    @staticmethod
    def handle_inline_let(exprenv, rewrite):
        expr = exprenv.expr
        let_node_id = rewrite._binder_id
        let_node = get_node(expr, let_node_id, "let")
        val = let_node.second
        if val.op == "select" or val.op == "index":
            return Expert.DONT_APPLY
        if val.op == "size":
            try:
                parent_node = first_non_let_ancestor(expr, let_node_id)
                child_node = let_node.third
                if parent_node.op in ["size", "index"] and child_node.op == "build":
                    return Expert.DEFAULT_PRIORITY
            except StopIteration:
                pass
            num_users = len(list(find_users(let_node)))
            if num_users > 1:
                return Expert.DONT_APPLY
            num_builds = count_ops_along_path(
                let_node, rewrite.node_id - let_node_id, ["build", "sumbuild"]
            )
            if num_builds > 1:
                return Expert.DONT_APPLY

        return Expert.DEFAULT_PRIORITY


class ExpertWrapper:
    def __init__(self, expert, num_time_heads):
        self._expert = expert
        self._num_time_heads = num_time_heads

    def evaluate_all_time_left(self, expr_batch):
        def eval(exprenv):
            episode = self._expert.optimize(
                exprenv, time_left=self._num_time_heads - 1, truncate=False,
            )
            #  pylint: disable=no-member
            episode_costs = np.minimum.accumulate(
                [node_action.node.exprenv.cost() for node_action in episode]
            )
            expr_start_cost = exprenv.cost()
            episode_values = [expr_start_cost - cost for cost in episode_costs]
            episode_values += episode_values[-1:] * (
                self._num_time_heads - len(episode_values)
            )  #  episode_values can be shorter than time_left
            return episode_values

        expr_batch_values = [eval(e) for e in expr_batch]
        expr_batch_values = np.array(expr_batch_values)
        assert expr_batch_values.shape == (len(expr_batch), self._num_time_heads)
        return expr_batch_values


experts = {"blas": BlasExpert}


def get_expert(name, rules):
    if name not in experts:
        raise ValueError("Unknown expert: {}".format(name))
    expert_cls = experts[name]
    return expert_cls(rules)


def main():
    file = "ksc/blas/blas_combined.kso"
    rules_name = "ml_rules_no_bind"
    scenario_config_path = os.path.join(
        os.path.dirname(__file__), "../scenarios/blas_combined.json",
    )
    with open(scenario_config_path) as f:
        scenario_dict = json.load(f)
    scenario_time_budget = scenario_dict["simulation_depth_eval"] + 1
    expr_set = expr_sets.get_expression_set(file)
    best_cost = lambda expr: expr_set.best_cost_for_expression(
        expr, rules_name, scenario_time_budget
    )

    agent_no_bind = BlasExpert(rewrites.get_rules("ml_rules_no_bind"))
    data = []
    for expr_name, exprenv in expr_set.exprenvs:
        print("Optimizing {}".format(expr_name))
        (best_known, _method, _seq) = best_cost(expr_name)
        data.append(
            (expr_name, len(exprenv.expr.nodes), exprenv.cost(), best_known,)
            + agent_no_bind.get_min_cost_length(exprenv)
        )
    print(
        tabulate(
            data,
            headers=[
                "expr",
                "#nodes",
                "original cost",
                "best known",
                "expert cost (no bind)",
                "episode length (no bind)",
            ],
            floatfmt=".1f",
        )
    )


if __name__ == "__main__":
    main()
