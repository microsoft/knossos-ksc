import argparse
import matplotlib
from typing import Callable, Optional, Sequence

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy
import os
from pandas import DataFrame

from rlo.experiment_result import load_config_events
from rlo import factory
from rlo import plotting
from rlo import sparser
from rlo import utils
from rlo import config_utils


def make_dataframe(events, gnn_limit=None, **kwargs):
    train_exprs = set(e["expr"] for e in events if "expr" in e)

    def final_costs(r, gnn_limit):
        if gnn_limit is None:
            return utils.counter_to_list(r["final_costs"])
        return [cost for _exp, cost, gnn in r["best_traj"] if gnn <= gnn_limit]

    return DataFrame(
        [
            dict(
                expr=expr,
                # Only quite old logs don't have eval_expr_cost so this will be inefficient for those.
                start_cost=r["eval_exp_cost"]
                if "eval_exp_cost" in r
                else sparser.parse_expr(r["eval_expr"]).cost(),
                is_train=expr in train_exprs,
                generation=int(r["generation"]),
                total_train_time=float(r.get("total_train_time", 0)),
                repetition=r["repetition"],
                cost=cost,
                **kwargs,
            )
            for r in events
            if r["event"] == "rollout_end" and "eval_expr" in r
            for expr in [r["eval_expr"]]
            for cost in final_costs(r, gnn_limit)
        ]
    )


def dataframe_with_gnn(logs, gnn_limits: Sequence[int] = (), **kwargs):
    if len(gnn_limits) == 0:
        return make_dataframe(logs, **kwargs)
    if len(gnn_limits) > 1:
        logs = list(logs)
    df = DataFrame()
    run = kwargs.pop("run", "")
    cmap = matplotlib.cm.get_cmap(kwargs.pop("color", "plasma"))
    for i, gnn in enumerate(gnn_limits):
        df = df.append(
            make_dataframe(
                logs,
                gnn_limit=gnn,
                run=f"{run}/{gnn}",
                color=cmap(i / (len(gnn_limits) - 1)),
                **kwargs,
            )
        )
    return df


def compute_optimality(df: DataFrame, best_cost_fn: Callable):
    df = df.assign(
        best_cost=lambda df: [
            best_cost_fn(expr, float("inf")).cost for expr in df.expr.tolist()
        ]
    )
    return df.assign(
        optimality=(df.start_cost - df.cost) / (df.start_cost - df.best_cost)
    )


def num_runs(df):
    return len(set(df.run)) if "run" in df.columns else 1


def average_over_expr(df):
    """ Given a dataset with exactly one cost value per rollout (search), averages over
        all the Expressions to give a dataset with one cost value per evaluation (eval_over_expressions) """
    if len(df) == 0:
        return df
    cols = ["repetition", "generation"] + (["run"] if "run" in df.columns else [])
    # Include all other columns whose values are completely determined by those (that is, columns which are the
    # same when all those above are the same) - should include total_train_{time, cpu}, but exclude cost.
    cols_all_same_per_group = [
        set(
            [
                colname
                for colname in group.columns
                if colname != "cost" and len(set(group[colname])) == 1
            ]
        )
        for _, group in df.groupby(cols)
    ]
    cols_all_same = list(
        cols_all_same_per_group[0].intersection(*cols_all_same_per_group[1:])
    )
    return df.groupby(cols_all_same).aggregate({"cost": "mean"}).reset_index()


def combine_costs(df, func):
    """ Given df: a dataset with multiple cost values for each rollout/search,
              func: a function from series of costs to one value (perhaps e.g. "min"),
        returns a dataset with only one cost for each rollout/search. """
    return (
        df.groupby(
            [
                col
                for col in df.columns
                # we need to exclude columns that may contain nan
                if col not in ["cost", "start_cost", "best_cost", "optimality"]
            ]
        )
        .aggregate({"cost": func})
        .reset_index()
    )


def plot_confidence_most_recent(
    ax, df, caption, x_scale, alpha=1.0, conf_alpha=None, color=None, linewidth=1
):
    """ df here is a subset of values for the same Caption/Expression/etc., differing only in cost, x, and repetition.
        Plots confidence intervals over the most-recent-value-in-each-repetition for all time. """
    if (len(df) == 0) or all(df[x_scale] == 0):
        # Skip rather than crash on missing data from runs before total_train_time or without best_costs
        return
    if color is None:
        if "color" in df.columns:
            color = utils.single_elem(set(df["color"]))
        else:
            color = plotting.random_color_from_string(
                utils.single_elem(set(df["expr"]))
            )
    if conf_alpha is None:
        conf_alpha = 0.3 * alpha
    # Consider repetition 0 does evaluation at time=0 and time=10, repetition 1 does eval at time=0 and time=11
    # all_x_vals is thus [0,10,11], and x_ordinal tells us that time=0 should be element 0, time=10 should be element 1, time=11 should be element 2
    all_x_vals = sorted(set(df[x_scale]))
    x_ordinal = {v: i for i, v in enumerate(all_x_vals)}
    grouped = list(df.groupby("repetition"))
    # Make an empty array; fill each (row==repetition) where we have data (some may be missing)
    data = numpy.full(fill_value=float("nan"), shape=(len(grouped), len(all_x_vals)))
    for (_, rep_rows), rep_data in zip(grouped, data):
        for _idx, row in rep_rows.iterrows():
            rep_data[x_ordinal[row[x_scale]]] = row["cost"]
        # Copy the most recent value into any subsequent empty timeslots
        last = float("nan")
        for i in range(len(rep_data)):
            if numpy.isnan(rep_data[i]):
                rep_data[i] = last
            else:
                last = rep_data[i]
    plotting.plot_with_confidence(
        ax,
        all_x_vals,
        data,
        conf_alpha=conf_alpha,
        col=color,
        label=caption,
        alpha=alpha,
        linewidth=linewidth,
    )


def set_title_labels(ax, title, xlabel, ylabel):
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)


def _cost_ylabel(oracle):
    return (
        "Fraction of starting (cost - oracle)"
        if oracle
        else "Fraction of starting cost"
    )


def plot_frequencies(
    all_experiment_datasets, fname, x_scale=None, oracle=False, suffix=""
):
    # Rollout-oriented plot, analysing the frequency with which rollouts do XYZ.
    # This assumes each element of final_costs is the result of a statistically independent rollout.
    # For A* this is not the case, so the plot is not really meaningful.
    if x_scale is None:
        x_scale = "total_train_time"
    min_cost_found = combine_costs(all_experiment_datasets, "min").cost.min()
    max_cost_redn = int(round((1.0 - min_cost_found) * 100))  # As percentage
    cost_redns = [
        min(x, max_cost_redn) for x in range(10, max_cost_redn + 9, 10)
    ]  # Truncate last to best seen, e.g, 10, 20, 27
    fig, axs = plt.subplots(
        2 + (len(cost_redns) - 1) // 3, 3, figsize=[15, 15], squeeze=False
    )
    assert len(axs[0]) == 3

    def plot_lines(axis, transformed):
        # Test and train on some axis, but test lines emphasized
        for is_train, suffix in [(True, "Train"), (False, "Test")]:
            data = transformed[transformed.is_train == is_train]
            averages = average_over_expr(data)
            if num_runs(transformed) == 1:
                # Single experiment: individual expressions in their own colours, plus averages in black
                for expr, group in data.groupby("expr"):
                    expr = utils.format_expr(expr, 25)
                    plot_confidence_most_recent(
                        axis,
                        group,
                        "{} ({})".format(expr, suffix),
                        x_scale,
                        alpha=0.3 if is_train else 0.4,
                    )
                plot_confidence_most_recent(
                    axis,
                    averages,
                    "Average ({})".format(suffix),
                    x_scale,
                    color="black",
                    linewidth=1 if is_train else 2,
                )
            else:
                # Multiple experiments: plot only averages with each experiment in its own colour
                conf_alpha = 1.0 / num_runs(transformed)
                for run, group in averages.groupby("run"):
                    plot_confidence_most_recent(
                        axis,
                        group,
                        f"{run} ({suffix})",
                        x_scale,
                        conf_alpha=conf_alpha,
                        linewidth=1 if is_train else 2,
                    )

    # Top three plots: the (best) cost we're 50% likely to see if we do 100, 10, 1 rollouts with each model=repetition
    for axis, num_rollouts in zip(axs[0], [100, 10, 1]):
        # Assume that future rollouts sample (with replacement) from the empirical distribution given by the rollouts we've seen.
        # The probability of the _best_ among N rollouts being equal to proportion p of the empirical ones,
        # is equivalent to the probability of all N rollouts being in the remaining (1-p), i.e. we find p
        # satisfying (1-p)^n = 0.5 ===> n*log(p) = log(0.5) so
        p = 1 - numpy.exp(numpy.log(0.5) / num_rollouts)
        print("For {} rollouts, taking proportion {}".format(num_rollouts, p))
        plot_lines(
            axis,
            combine_costs(
                all_experiment_datasets, lambda costs: numpy.percentile(costs, 100 * p)
            ),
        )
        set_title_labels(
            axis,
            "Best in {} rollouts".format(num_rollouts),
            x_scale,
            _cost_ylabel(oracle),
        )

    # Remaining plots: the proportion of rollouts reaching a particular cost (or lower)
    cost_axes = [ax for row in axs[1:] for ax in row]
    assert len(cost_axes) >= len(cost_redns) and len(cost_redns) > (len(cost_axes) - 3)
    for cost_redn, axis in zip(cost_redns, cost_axes):
        target_cost = 1.0 - cost_redn / 100
        plot_lines(
            axis,
            combine_costs(
                all_experiment_datasets,
                lambda costs: numpy.count_nonzero(numpy.array(costs) <= target_cost)
                / len(costs),
            ),
        )
        set_title_labels(
            axis,
            "Reducing {} by at least {}%".format(
                "overhead above oracle" if oracle else "cost", cost_redn
            ),
            x_scale,
            "Frequency",
        )
    plt.xlabel("Generations", fontsize=12)
    plt.suptitle(
        ("Frequency/cost summary" if not oracle else "Frequency/optimization summary")
        + suffix
    )
    fig.tight_layout(rect=[0.0, 0.05, 1.0, 0.95])

    plt.figlegend(
        *plotting.combine_legend_handles_labels(*[ax for row in axs for ax in row]),
        loc="lower center",
        fontsize=8,
        ncol=2,
    )

    plt.savefig(fname)
    plt.close()


def plot_min_costs(data: DataFrame, fname: str, oracle: bool, suffix: str = ""):
    # Min-cost plot. Assume only the minimum among the final_costs is meaningful.
    def plot_test_and_train(axs, x_scale):
        min_costs = combine_costs(data, "min")
        # axs = array of two axes, first for training exprs, second for test exprs
        for ax, is_train in zip(axs, [True, False]):
            tt_data = min_costs[min_costs.is_train == is_train]
            title = "Min cost found ({})".format("Train" if is_train else "Test")
            averages = average_over_expr(tt_data)
            if num_runs(data) == 1:
                # Single experiment: individual expressions in their own colours, plus averages in black
                for expr, group in tt_data.groupby("expr"):
                    expr = utils.format_expr(expr, 25)
                    plot_confidence_most_recent(ax, group, expr, x_scale, alpha=0.4)
                plot_confidence_most_recent(
                    ax, averages, "Average", x_scale, color="black"
                )
            else:
                # Multiple experiments: plot only averages with each experiment in its own colour
                conf_alpha = 1.0 / num_runs(data)
                for run, group in averages.groupby("run"):
                    plot_confidence_most_recent(
                        ax, group, run, x_scale, conf_alpha=conf_alpha
                    )
            set_title_labels(ax, title, x_scale, _cost_ylabel(oracle))

    # Two rows: training set and test set; two columns: x_scale=generation and x_scale=total_train_time
    fig, axss = plt.subplots(2, 2, figsize=(14.3, 10))  # axss elements are rows
    axss = list(zip(*axss))  # Now axss elements are columns
    plot_test_and_train(axss[0], "generation")
    plot_test_and_train(axss[1], "total_train_time")

    plt.suptitle("Min cost summary" + suffix)
    fig.tight_layout(rect=[0.15, 0.05, 0.85, 0.95])  # 14.3 * 0.7 ~= 10

    plt.figlegend(
        *plotting.combine_legend_handles_labels(*axss[0]),
        loc="lower right",
        bbox_to_anchor=(0, 0.05, 1.0, 0.95),
        fontsize=8,
        ncol=1,
    )

    plt.savefig(fname)
    plt.close()


def plot_separate_min_costs(all_experiment_datasets, fname, suffix=""):
    """ No normalization is done here; instead an oracle line is added """
    conf_alpha = 1.0 / num_runs(all_experiment_datasets)
    exprs = list(set(all_experiment_datasets["expr"]))
    fig, axss = plt.subplots(
        len(exprs), 2, figsize=(24, len(exprs) * 4), squeeze=False
    )  # 2 columns
    for (expr, data), axs in zip(all_experiment_datasets.groupby("expr"), axss):
        # The same Expression could possibly be test-only/also-trained in different experiments
        test_train = set(data["is_train"])
        test_train = (
            "Train"
            if test_train == set([True])
            else "Test"
            if test_train == set([False])
            else "Train/Test"
        )
        min_costs = combine_costs(data, "min")
        best_cost = utils.single_elem(set(data["best_cost"]))
        for ax, x_scale in zip(axs.ravel(), ["generation", "total_train_time"]):
            min_costs_by_run = (
                min_costs.groupby("run")
                if "run" in min_costs.columns
                else [(suffix, min_costs)]
            )
            for run, group in min_costs_by_run:
                plot_confidence_most_recent(
                    ax, group, run, x_scale, conf_alpha=conf_alpha
                )
            ax.set_ylabel("Cost")
            ax.set_xlabel(x_scale)
            if best_cost is not None:
                ax.plot(
                    [0, data[x_scale].max()],
                    [best_cost] * 2,
                    linestyle="--",
                    color="Black",
                    linewidth=1,
                )
        axs[0].set_title("{} ({})".format(utils.format_expr(expr, 90), test_train))

    plt.suptitle("Min cost per expression" + suffix)
    fig.tight_layout(rect=[0.0, 0.05, 1.0, 0.95])
    if "run" in all_experiment_datasets.columns:
        plt.figlegend(
            *plotting.combine_legend_handles_labels(
                *[ax for axs in axss for ax in axs]
            ),
            loc="lower center",
            fontsize=8,
        )

    plt.savefig(fname)
    plt.close()


def _plot_costs(
    all_experiment_datasets: DataFrame,
    fname_formatter=lambda fn: fn,
    frequency_x_scale: Optional[str] = None,
    suffix: str = "",
):
    oracle_costs = all_experiment_datasets.assign(
        cost=1.0 - all_experiment_datasets.optimality
    )
    oracle_costs = oracle_costs[oracle_costs.cost.apply(lambda ctr: ctr is not None)]
    norm_costs = all_experiment_datasets.assign(
        cost=all_experiment_datasets["cost"] / all_experiment_datasets["start_cost"]
    )
    if frequency_x_scale:
        plot_frequencies(
            norm_costs,
            fname_formatter("cost_freqs.png"),
            x_scale=frequency_x_scale,
            suffix=suffix,
        )
        if len(oracle_costs) > 0:
            plot_frequencies(
                oracle_costs,
                fname_formatter("optimization_freqs.png"),
                x_scale=frequency_x_scale,
                oracle=True,
                suffix=suffix,
            )
    else:
        plot_separate_min_costs(
            all_experiment_datasets, fname_formatter("min_costs_by_expr.png")
        )
        plot_min_costs(norm_costs, fname_formatter("best_costs.png"), False, suffix)
        if len(oracle_costs) > 0:
            plot_min_costs(oracle_costs, fname_formatter("best_opt.png"), True, suffix)


def plot_costs_from_config(
    config,
    events,
    gnn_limits: Sequence[int] = (),
    frequency_x_scale: Optional[str] = None,
):
    if (frequency_x_scale is not None) and config.get(
        "eval_search", "rollout"
    ) != "rollout":
        raise ValueError("Frequency plot does not make sense except for rollouts")
    df = compute_optimality(
        dataframe_with_gnn(events, gnn_limits), factory.best_cost_from_config(config)
    )
    _plot_costs(
        df,
        lambda fn: plotting.format_figure_filename(config, fn),
        frequency_x_scale,
        plotting.config_suffix(config),
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "experiment_id",
        type=str,
        nargs="+",
        help="id or path to config.json or events.json; may be repeated",
    )
    parser.add_argument(
        "--frequency_x_scale",
        choices=["generation", "total_train_time"],
        default=None,
        help="Do frequency plot (meaningful for rollout-evaluation only) with given x-scale",
    )
    parser.add_argument(
        "--gnn_limit",
        type=int,
        nargs="+",
        default=[],
        help="Plot line for evaluation limited to specified --max_gnn_eval",
    )
    parser.add_argument("--outfile_suffix", type=str, default="")
    args = parser.parse_args()

    if len(args.experiment_id) == 1:
        config, logs = load_config_events(args.experiment_id[0])
        if config is not None:
            plot_costs_from_config(config, logs, args.gnn_limit, args.frequency_x_scale)
        else:
            _plot_costs(
                dataframe_with_gnn(logs, args.gnn_limit),
                lambda fn: os.path.join(args.experiment_id[0], fn),
                args.frequency_x_scale,
            )
    else:
        dataframe = DataFrame()
        configs = []
        for exp_id in args.experiment_id:
            # Separate off any description/colour into 'desc'
            if exp_id.find("=") == -1:
                desc = ""
            else:
                exp_id, desc = exp_id.split("=", 1)
            config, events = load_config_events(exp_id)
            if config is not None:
                suffix = plotting.config_suffix(config)
                configs.append(config)
            else:
                suffix = exp_id
            # Description may include a colour, generate from caption if not
            if desc.find("=") == -1:
                caption = desc + suffix
                col = plotting.random_color_from_string(caption)
            else:
                prefix, col = desc.split("=", 1)
                caption = prefix + suffix
            dataframe = dataframe.append(
                dataframe_with_gnn(events, args.gnn_limit, color=col, run=caption)
            )
        if (args.frequency_x_scale is not None) and any(
            config.get("eval_search", "rollout") != "rollout" for config in configs
        ):
            raise ValueError(
                "Cannot do frequency plot except for rollout-evaluation experiments"
            )
        if len(configs) > 0:
            common_config = config_utils.unify_configs(
                configs,
                [
                    "rules",
                    "train_exprs",
                    "test_exprs",
                    "simulation_depth_train",
                    "simulation_depth_eval",
                ],
                allow_renames={
                    "simulation_depth_train": "simulation_depth",
                    "simulation_depth_eval": "simulation_depth",
                },
            )
            dataframe = compute_optimality(
                dataframe, factory.best_cost_from_config(common_config)
            )

        def add_suffix(fn):
            assert fn.endswith(".png")
            return fn[:-4] + args.outfile_suffix + ".png"

        _plot_costs(dataframe, add_suffix, args.frequency_x_scale)


if __name__ == "__main__":
    main()
