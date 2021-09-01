import argparse
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

from rlo import experiment_result
from rlo import plotting
from rlo import utils
import numpy as np


def average_of_counter(ctr):
    return sum([float(k) * v for k, v in ctr.items()]) / sum(ctr.values())


def plot_search_summary(outfile, title_suffix, events, alg):
    if alg not in ["rollout", "astar", "beam", "hybrid"]:
        raise ValueError("Unknown search algorithm {}".format(alg))
    joined_events = plotting.join(
        [e for e in events if e["event"] == "rollout_end" and "eval_expr" not in e],
        [e for e in events if e["event"] == "search"],
        lambda r: (r["repetition"], r["generation"], r["expr"]),
    ).values()  # Discard the join keys

    x_axis_func = lambda r: r["generation"]
    by_expr = utils.group_by(joined_events, lambda r: r["expr"])
    # Two plots for each expression, one above the other so x-axes line up
    fig, axs = plt.subplots(
        2, len(by_expr), figsize=[6 * len(by_expr), 6], squeeze=False,
    )
    assert len(axs) == 2 and len(axs[0]) == len(by_expr) and len(axs[1]) == len(by_expr)
    for ax1, ax2, (expr, logs) in zip(axs[0], axs[1], by_expr.items()):
        reps = plotting.pad_into_lists(
            utils.group_by(logs, lambda r: r["repetition"]).values(), x_axis_func
        )
        # An generation may be a fixed number of varying expressions, so allow some exprs to be missing.
        reps = [
            plotting.carry_back_first(plotting.carry_previous_over_none(rep))
            for rep in reps
        ]
        x_vals = range(1, len(reps[0]) + 1)

        def plot(ax, fn, colnum, label):
            plotting.plot_with_confidence(
                ax,
                x_vals,
                [[fn(r) for r in rep] for rep in reps],
                plotting.colors(colnum),
                label=label,
            )

        expr_str = str(expr)
        expr_str = utils.format_expr(expr_str, 50)

        # First subplot: three axes.
        ax1.set_title("{} results".format(expr_str))
        # - Min-cost-found on left-side log axis
        plot(ax1, lambda r: min([float(c) for c in r["final_costs"]]), 1, "best_cost")
        start_costs = [r["expr_cost"] for r in logs if "expr_cost" in r]
        if len(start_costs) > 0:
            ax1.plot(
                [0, max(x_vals)],
                [utils.single_elem(set(start_costs))] * 2,
                linestyle="--",
                color=plotting.colors(1),
                linewidth=1,
            )
        ax1.set_yscale("log")
        ax1.set_ylabel("best_cost", color=plotting.colors(1))
        ax1.tick_params(axis="y", colors=plotting.colors(1), which="both")

        # - Average episode length on linear scale on rhs
        ax1b = ax1.twinx()
        plot(ax1b, lambda r: average_of_counter(r["episode_lengths"]), 2, "avg_ep_len")
        ax1b.set_ylabel("avg_ep_len", color=plotting.colors(2))
        ax1b.tick_params(
            axis="y", colors=plotting.colors(2), which="both", direction="in"
        )

        # - Number of generated episodes on a separate log scale also on rhs
        ax1c = ax1.twinx()
        plot(
            ax1c, lambda r: sum(r["episode_lengths"].values()), 3, "#episodes"
        )  # Unlike final_costs, episode_lengths works for both A* and Rollout.
        ax1c.set_yscale("log")
        ax1c.set_ylabel("#episodes", color=plotting.colors(3))
        ax1c.tick_params(
            axis="y", colors=plotting.colors(3), which="both", direction="out"
        )
        ax1c.spines["right"].set_position(("axes", 1.2))
        ax1c.spines["right"].set_visible(True)

        # Second subplot: two axes
        ax2.set_title("{} details".format(expr_str))
        # - One log scale, with positions evaluated and tl_cache_hits (and others IF A*)
        plot(ax2, lambda r: float(r["posns_evaluated"]), 4, "p_posns_evald")
        plot(ax2, lambda r: float(r["tl_cache_hits"]), 5, "p_tl_cache")
        ax2.set_ylabel("posns p_")  # This is the scale for all things beginning p_
        ax2.set_yscale(
            "log"
        )  # - One linear scale, with batch size (and others IF rollout)
        # - One linear scale, with batch size (and others IF rollout)
        ax2b = ax2.twinx()
        # np.float64 does not produce error with 0 division, for example np.float64(0)/float(0) gives nan, and float(0)/float(0) gives ZeroDivisionError
        plot(
            ax2b,
            lambda r: np.float64(r["posns_evaluated"]) / float(r["batches"]),
            6,
            "evals/batch",
        )

        if alg in ["astar", "beam", "hybrid"]:
            # Only the one line on linear scale, so colour the axis and skip legend
            ax2b.set_ylabel("evals/batch", color=plotting.colors(6))
            ax2b.tick_params(axis="y", colors=plotting.colors(6), which="both")
            legends, labels = [], []
            for col, key in enumerate(
                ["generated", "expanded", "pruned", "unexplored", "merges"],
                7,  # The starting index, i.e. color, after the lines plotted above
            ):
                plot(ax2, lambda r: float(r[key]), col, "p_" + key)
        else:
            assert alg == "rollout"
            # Add success rate and alpha to linear axis
            plot(ax2b, lambda r: float(r["alpha"]), 7, "alpha")
            plot(ax2b, lambda r: float(r["success_rate"]) * 10, 8, "success/10")
            ax2b.set_ylabel("e/b, alpha, succ")
            # Include legends
            legends, labels = ax2b.get_legend_handles_labels()

    plt.xlabel("Generation", fontsize=16)
    plt.suptitle("Search summary" + title_suffix)
    # Add the p_* to the legend
    p_legends, p_labels = ax2.get_legend_handles_labels()
    plt.figlegend(
        legends + p_legends,
        labels + p_labels,
        loc="lower center",
        ncol=len(legends + p_legends),
    )

    fig.tight_layout(rect=[0.0, 0.05, 1.0, 0.95])
    plt.savefig(outfile)


def plot_search_summary_from_config(config, events):
    plot_search_summary(
        plotting.format_figure_filename(config, "search_summary.png"),
        plotting.config_suffix(config),
        events,
        config["train_search"],
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
    logs = experiment_result.load_events_from_config(config)
    plot_search_summary_from_config(config, logs)


if __name__ == "__main__":
    main()
