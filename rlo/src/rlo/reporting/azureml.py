from collections import defaultdict
from operator import itemgetter
from tempfile import TemporaryDirectory
from typing import Iterable, Optional, Tuple

import numpy as np


import azureml.core
from azureml.core import Model, Workspace
from azureml.exceptions import AuthenticationException

from rlo.config_utils import config_for_repetition
from rlo.factory import (
    ConfigType,
    framework_from_config,
    model_save_dir_from_config,
    ModelStateFactory,
)
from rlo.model.model import ModelState
from rlo.search_ops import SearchSummary
from rlo import utils
from .utils import RemoteRunInfo, SearchMetricFunction


def get_current_context(prefer_parent=False):
    # Get the current context in a remote Azure ML run
    run = azureml.core.Run.get_context(allow_offline=True)

    if prefer_parent and hasattr(run, "parent") and run.parent is not None:
        # Log only in the parent run
        run = run.parent
    return run


def get_current_run_info() -> RemoteRunInfo:
    run = get_current_context()
    if isinstance(run, azureml.core.Run):
        return RemoteRunInfo("azureml", run.experiment.workspace.name, run.id)
    else:
        return RemoteRunInfo("local", None, None)


def get_default_workspace(
    allow_interactive=True, workspace_config_file="aml_config.json"
) -> Workspace:
    run = get_current_context()
    if isinstance(run, azureml.core.Run):
        # If running in Azure ML
        return run.experiment.workspace
    if not allow_interactive:
        raise AuthenticationException(
            "Cannot authenticate with Azure ML. Try allow_interactive=True"
        )
    # If running locally or in a CI by a service principal
    return Workspace.from_config(workspace_config_file)


def get_secret(secret_name, allow_interactive=True):
    ws = get_default_workspace(allow_interactive)
    kv = ws.get_default_keyvault()
    return kv.get_secret(secret_name)


def aggregate_metrics_over_repetitions(num_repetitions: int):
    """ Aggregate raw metrics possibly logged asynchronously from repetitions by averaging them.

    Each raw metric is stored as a table with three columns, repetition, generation, and value.
    The average is computed for each generation and the result of aggregation is logged as a
    1D list so that Azure ML dashboard can easily visualize.

    Care needs to be taken because we can only compute the average when all repetitions are logged
    and the averaged metric needs to be logged only once per generation in the correct order.
    """
    run = get_current_context(prefer_parent=True)
    if not isinstance(run, azureml.core.Run):
        return

    all_metrics = run.get_metrics()
    raw_keys = [key for key in all_metrics.keys() if key.startswith("raw_")]

    for raw_key in raw_keys:
        metrics = all_metrics[raw_key]
        if not isinstance(metrics["generation"], list):
            # not enough data to aggregate
            print(f"run={run}: {metrics} not a list")
            continue
        agg_key = raw_key[4:]
        existing = all_metrics.get(agg_key, [])
        # The next generation to be aggregated is the number of already logged generations
        # (existing is not a list when exactly one value is logged!)
        next_generation = len(existing) if isinstance(existing, list) else 1
        for gen in range(next_generation, max(metrics["generation"]) + 1):
            values = [
                val
                for rec_gen, val in zip(metrics["generation"], metrics["value"])
                if rec_gen == gen
            ]
            print(
                f"gen={gen}, key={agg_key}: values={values}, len(values)={len(values)}, num_repetitions={num_repetitions}"
            )
            if len(values) == num_repetitions:
                print(f"logging key={agg_key} val={np.mean(values)}")
                run.log(agg_key, np.mean(values))


class AzuremlReporter:
    def __init__(
        self,
        repetition: int,
        num_repetitions: int,
        search_metric_func: SearchMetricFunction,
        train_expr_names: Iterable[str],
    ):
        self._repetition = repetition
        self._num_repetitions = num_repetitions
        self._search_metric_func = search_metric_func
        self._train_expr_names = set(train_expr_names)

    def log_raw_train_test_metrics(
        self, generation: int, search_summaries: Iterable[SearchSummary]
    ):
        metrics = defaultdict(list)
        for search_summary in search_summaries:
            val = self._search_metric_func(search_summary)
            if search_summary.expr_name in self._train_expr_names:
                metrics["train"].append(val)
            else:
                metrics["test"].append(val)
        run = get_current_context(prefer_parent=True)
        for phase in ["train", "test"]:
            run.log_row(
                f"raw_avg_{phase}_{self._search_metric_func.name}",
                repetition=self._repetition,
                generation=generation,
                value=np.mean(metrics[phase]),
            )

        if self._repetition == 0:
            # If this is the first repetition, try aggregation
            # (metrics may not be available due to asynchrony)
            aggregate_metrics_over_repetitions(self._num_repetitions)


def log_all_metrics(config, events):
    # local import to avoid circular dependency
    from rlo.print_expr_solved import output_exprs_solved

    # Log only in parent run to avoid duplication
    run = get_current_context(prefer_parent=True)

    # Log minimal config parameters as tags
    for key in ["extra_scenario_params", "gitlog", "run_id"]:
        run.tag(key, config[key])

    # Tag framework
    run.tag(framework_from_config(config))

    # Compute various metrics to log
    total_train_time = max(
        e["total_train_time"] for e in events if "total_train_time" in e
    )
    total_generations = max(e["generation"] for e in events if "generation" in e)

    df_all, df_avg = output_exprs_solved(config, events)

    # Compute the best number of solved expressions over all reps
    best_num_solved = df_all.groupby("generation").max().num_solved

    num_train_expr = set(
        [
            e["num_train_expr"]
            for e in events
            if e["event"] == "expression_summary" and "num_train_expr" in e
        ]
    )
    num_test_expr = set(
        [
            e["num_test_expr"]
            for e in events
            if e["event"] == "expression_summary" and "num_test_expr" in e
        ]
    )

    run.log("avg_num_solved_in_generation_0", df_avg.num_solved.iloc[0])
    run.log("avg_num_solved_in_last_generation", df_avg.num_solved.iloc[-1])
    run.log("best_num_solved_in_generation_0", best_num_solved.iloc[0])
    run.log("best_num_solved_in_last_generation", best_num_solved.iloc[-1])
    run.log("total_train_time", total_train_time)
    run.log("total_generations", total_generations)
    run.log("num_train_expr", num_train_expr.pop())
    run.log("num_test_expr", num_test_expr.pop())

    # log GNN evaluation throughput
    total_gnn_eval_nodes, total_gnn_eval_time = zip(
        *[
            (e["gnn_eval_nodes"], e["gnn_eval_time"])
            for e in events
            if e["event"] == "rollout_end"
        ]
    )

    run.log(
        "search_gnn_throughput", sum(total_gnn_eval_nodes) / sum(total_gnn_eval_time)
    )

    # Flush outstanding aggregation which may have been pending due to asynchrony
    aggregate_metrics_over_repetitions(config["num_repetitions"])


def upload_locally_saved_models(config: ConfigType, allow_interactive: bool = False):
    """Uploads models saved in local `outputs/Run_{run_id}`.

    Args:
        config: experiment config
        allow_interactive: (optional) allow interactive authentication
    """
    try:
        ws = get_default_workspace(allow_interactive)
    except AuthenticationException:
        print("Nothing to do since not running in Azure ML")
        return
    framework = framework_from_config(config)
    for rep in range(config["num_repetitions"]):
        rep_config = config_for_repetition(config, rep)
        # Take the last generation from the current repetition
        gen, model_path = max(
            utils.get_saved_model_files(model_save_dir_from_config(rep_config)),
            key=itemgetter(0),
        )
        Model.register(
            workspace=ws,
            model_path=model_path,
            model_name=f'{config["scenario"]}_model',
            tags={
                "run_id": config["run_id"],
                "repetition": rep,
                "generation": gen,
                "framework": framework,
            },
        )


def download_model_state(
    run_id: str, repetition: int, allow_interactive: bool = False,
) -> Tuple[Optional[int], ModelState]:
    """Downloads ModelState from run ID

    Args:
        run_id: run ID for the original run
        repetition: repetition to download the model from
        allow_interactive: (optional) allow interactive authentication

    Returns:
        a pair of (generation, ModelState or Weights) where ModelState is
        for a torch model and Weights is for a tensorflow model.
    """
    # Avoid circular import
    from .cosmosdb import get_run_from_db

    original_config, _ = get_run_from_db(run_id)
    ws = get_default_workspace(allow_interactive)
    model = Model(
        workspace=ws,
        name=f"{original_config['scenario']}_model",
        tags=[["run_id", run_id], ["repetition", str(repetition)]],
    )
    generation = model.tags.get("generation")
    with TemporaryDirectory() as tempdir:
        framework = framework_from_config(original_config)
        print(f"Downloading {framework} model {model} into {tempdir}...")
        factory = ModelStateFactory.from_config(original_config)
        return generation, factory.from_path(model.download(tempdir))
