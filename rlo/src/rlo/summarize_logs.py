# mypy: ignore-errors
import traceback
from typing import Optional

from rlo.compare_runs import compare_reps_within_run
from rlo.experiment_result import load_events_from_config
from rlo.expr_sets import ExpressionSet
from rlo.extra_plots import extra_plots
from rlo.plot_clustering_summary import plot_clustering_summary_from_config
from rlo.plot_costs import plot_costs_from_config
from rlo.plot_rollouts import plot_success_from_config
from rlo.plot_dataset_summary import plot_dataset_summary_from_config
from rlo.plot_dataset_overlap import plot_dataset_overlap_from_config
from rlo.plot_empirical_animated import plot_empirical_animated_from_config
from rlo.plot_empirical_predicted_values import (
    plot_empirical_predicted_values_from_config,
)
from rlo.plot_intra_gen_loss import plot_intra_gen_loss_animated_from_config
from rlo.plot_rewrite_sequences import plot_rewrite_sequences_from_config
from rlo.plot_ray_tasks import plot_ray_tasks_from_config
from rlo.plot_train_summary import plot_train_loss_from_config
from rlo.plot_search_summary import plot_search_summary_from_config
from rlo.print_best_episodes import output_best_episodes_from_config
from rlo.plot_merging_across_generations import (
    plot_merging_across_generations_from_config,
)
from rlo.plot_time_lefts_during_search import plot_time_lefts_during_search_from_config
from rlo.plot_state_values_animated import plot_state_values_animated_from_config
from rlo.plot_scatter_fitted import plot_scatter_fitted_from_config
from rlo.reporting import azureml, cosmosdb
from rlo.save_rlo_to_ksc import save_rlo_to_ksc_from_config
from plot_success import plot_success_summary_from_config

import functools


def summarize_logs(
    config, eval_exprs: Optional[ExpressionSet] = None, loglevel=1, ray=False
):

    # Plots that require logs of verbosity >= 1
    verbose_plots = [
        plot_empirical_predicted_values_from_config,
        plot_empirical_animated_from_config,
        plot_intra_gen_loss_animated_from_config,
        plot_dataset_summary_from_config,
        plot_dataset_overlap_from_config,
        plot_scatter_fitted_from_config,
    ] + [extra_plots[plot_name] for plot_name in config["extra_plots"]]
    if config["train_search"] in ["astar", "beam", "hybrid"]:
        verbose_plots.append(
            functools.partial(plot_time_lefts_during_search_from_config, phase="train")
        )
    if config["eval_search"] in ["astar", "beam", "hybrid"]:
        verbose_plots.append(
            functools.partial(plot_time_lefts_during_search_from_config, phase="eval")
        )

    # Plots that only require logs of verbosity == 0
    non_verbose_plots = [
        plot_costs_from_config,
        plot_train_loss_from_config,
        plot_search_summary_from_config,
        plot_state_values_animated_from_config,
        output_best_episodes_from_config,
        plot_success_summary_from_config,
    ]
    # add conditional non-verbose plots
    if config["num_episode_clusters"] > 0:
        non_verbose_plots.append(plot_clustering_summary_from_config)
    if config["eval_search"] == "astar":
        non_verbose_plots.append(plot_rewrite_sequences_from_config)
    if "best_across_generations_refiner" in config["dataset_refiners"]:
        non_verbose_plots.append(plot_merging_across_generations_from_config)
    if ray:
        non_verbose_plots.append(plot_ray_tasks_from_config)
    if config["eval_search"] == "rollout":
        # These plots are about frequency of rollouts defined by softmax policy, so not applicable to A*
        non_verbose_plots.append(plot_success_from_config)
    if config["eval_search"] == "rollout":
        non_verbose_plots.append(
            functools.partial(plot_costs_from_config, frequency_x_scale="generation")
        )

    # verbose events are potentially very large, so this loads them only lazily
    verbose_events = load_events_from_config(config, verbosity=loglevel)

    # Store the exceptions themselves as well as tracebacks so that they're accessible
    # to the debugger
    exception_list = []
    exception_traceback_list = []
    if loglevel >= 1:
        for pl in verbose_plots:
            try:
                pl(config, events=verbose_events)
            except Exception as e:
                exception_traceback_list.append("".join(traceback.format_exc()))
                exception_list.append(e)

    # verbosity=0 events are much smaller
    # load into memory with `list` for faster repeated iteration
    events = list(load_events_from_config(config, verbosity=0))
    for pl in non_verbose_plots:
        try:
            pl(config, events=events)
        except Exception as e:
            exception_traceback_list.append("".join(traceback.format_exc()))
            exception_list.append(e)

    # Log metrics to Azure ML
    azureml.log_all_metrics(config, events)

    if config["template_path"] is not None:
        save_rlo_to_ksc_from_config(
            config, events=events, orig_exprs=dict(eval_exprs.named_exprenvs())
        )

    # Upload the run information to Cosmos DB
    # Don't allow interactive authentication as it will fail in QuickTest
    cosmosdb.upload_run_to_db(config, allow_interactive=False)

    # Upload models to Azure ML workspace if running in Azure ML
    if config["upload_models"]:
        azureml.upload_locally_saved_models(config)

    if config.get("seed_all_reps") is not None:
        compare_reps_within_run(events, sort=ray)

    if len(exception_list) > 0:
        raise Exception(
            f"Some of the plots have not been generated:\n"
            "\n".join(exception_traceback_list),
        )

    return events
