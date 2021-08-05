import argparse
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

from rlo import experiment_result
from rlo import plotting
from rlo import utils


def plot_empirical_predicted_values(
    outfile, title_suffix, events, probabilities=[10, 50, 90]
):

    # import json # determine the size of logs - uncomment if needed
    # print("Size", len(json.dumps([e for e in events if e['event']=='plot_value_comparison'])))

    train_logs = [r for r in events if r["event"] == "plot_value_comparison"]
    by_expr = utils.group_by(train_logs, lambda r: r["expr"])
    fig, axs = plt.subplots(
        len(by_expr), 1, figsize=[15, 4 * len(by_expr)], squeeze=False,
    )
    x_axis_func = lambda r: r["generation"]
    N_GENERATIONS = (
        max([x_axis_func(rec) for rec in train_logs]) + 1
    )  # The first generation is numbered 0
    x_vals = range(N_GENERATIONS)

    N_REPETITIONS = max([int(r["repetition"]) for r in train_logs]) + 1
    for ax, (expr, logs) in zip(axs.ravel(), by_expr.items()):
        expr_cost = logs[0][
            "expr_cost"
        ]  # we just need an initial cost for the starting expression expr
        # compute percentiles separately for each repetition for each generation
        by_generation = utils.group_by(logs, x_axis_func)
        all_percentiles = np.full(
            (N_GENERATIONS, N_REPETITIONS, len(probabilities)), float("nan")
        )

        for generation, generation_logs in by_generation.items():
            for repetition, rep_logs in utils.group_by(
                generation_logs, lambda r: r["repetition"]
            ).items():
                # find percentiles of (predicted - empirical) for repetition
                all_percentiles[int(generation), int(repetition), :] = np.percentile(
                    [
                        p - e
                        for r in rep_logs
                        for p, e in zip(r["predicted_value"], r["empirical_value"])
                    ],
                    probabilities,
                    axis=0,
                )

        # then average across repetitions (ignoring absent values=NaN)
        av_percentiles = np.nanmean(all_percentiles, axis=1)
        # and plot a line against generation for each percentile
        for i in range(len(probabilities)):
            ax.plot(
                x_vals,
                av_percentiles[:, i],
                label=str(probabilities[i]) + "th percentile",
            )

        ax.set_title(
            "Value evaluation for {} with cost {}, {}".format(
                expr, expr_cost, title_suffix
            ),
            fontsize=9,
        )
        ax.axhline(0, color="black", linewidth=1)
        ax.set_ylabel("(predicted - empirical)", fontsize=9)
        ax.set_xlabel("Generations", fontsize=9)

    plt.figlegend(*ax.get_legend_handles_labels(), loc="upper left")
    fig.tight_layout()
    plt.savefig(outfile)


def plot_empirical_predicted_values_from_config(config, events):
    plot_empirical_predicted_values(
        plotting.format_figure_filename(config, "empirical_predicted_values.png"),
        plotting.config_suffix(config),
        events,
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="a run ID (e.g., 2019_01_06_13_15_48_13172) or path to a config.json file",
    )
    args = parser.parse_args()

    config = experiment_result.load_config(args.run_id)
    if "result_save_path" in config:
        logs = experiment_result.load_events_from_config(config, verbosity=1)
        plot_empirical_predicted_values_from_config(config, logs)
    else:
        plot_empirical_predicted_values(
            "empirical_predicted_values.png", "", events=config
        )


if __name__ == "__main__":
    main()
