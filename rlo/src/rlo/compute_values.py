from typing import Iterable

from rlo import analytics
from rlo.tf_model import ModelWrapper
from rlo.expression_util import NamedExprWithEnv
from rlo.dataset import evaluation_to_list


def compute_values(
    model_wrapper: ModelWrapper, expressions: Iterable[NamedExprWithEnv]
):
    """
        Computes values over range of time budgets for all expressions for a single model.
    """
    names, expressions = zip(*expressions)
    for name, expr, values in zip(
        names, expressions, model_wrapper.evaluate_all_time_left(expressions)
    ):
        values = evaluation_to_list(values)
        analytics.event(
            "expr_value_info", value=values, eval_expr=name, eval_exp_cost=expr.cost()
        )
