from abc import abstractmethod, abstractproperty, ABC
from contextlib import contextmanager
import json
import subprocess
from typing import Callable, Literal, NamedTuple, Optional

from rlo.best_results import TypeBestCost
from rlo.expr_sets import TimeBudget
from rlo.search_ops import SearchSummary


class RemoteRunInfo(NamedTuple):
    source: Literal["azurebatch", "azureml", "local"]
    workspace_name: Optional[str]
    azureml_run_id: Optional[str]


def compute_optimality(
    original_cost: float, best_cost: float, target_cost: Optional[float]
):
    if target_cost is None:
        return float("nan")
    if original_cost == target_cost:
        return 100
    return 100 * (original_cost - best_cost) / (original_cost - target_cost)


@contextmanager
def azure_subscription_context(subscription_name):
    """ Context manager for temporarily changing the default azure subscription
    """
    # check_output returns bytes with quotes and new line. Let json.loads handle the mess
    current_sub = json.loads(
        subprocess.check_output("az account show --query name", shell=True)
    )
    subprocess.check_call(f"az account set -s {subscription_name}", shell=True)
    try:
        yield
    finally:
        subprocess.check_call(f"az account set -s {current_sub}", shell=True)


class SearchMetricFunction(ABC):
    @abstractproperty
    def name(self):
        """ Name of the metric to be reported """

    @abstractmethod
    def __call__(self, search_summary: SearchSummary) -> float:
        """ Computes the metric from SearchSummary """


class SearchOptimalityMetricFunction(SearchMetricFunction):
    name = "optimality"

    def __init__(self, best_cost_func: Callable[[str, TimeBudget], TypeBestCost]):
        self._best_cost_func = best_cost_func

    def __call__(self, search_summary: SearchSummary) -> float:
        target_cost = self._best_cost_func(search_summary.expr_name, float("inf"))
        assert target_cost is not None
        return compute_optimality(
            search_summary.start_cost, search_summary.final_cost, target_cost.cost
        )
