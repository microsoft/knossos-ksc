import argparse
import matplotlib.pyplot as plt
import numpy as np
from textwrap import wrap

from rlo import experiment_result
from rlo import plotting
from rlo import utils


def plot_clustering_summary(outfile, title_suffix, events, num_clusters=None):
    filtered_events = [e for e in events if e["event"] == "cluster_episodes"]

    def clusters_in_event(e):
        assert len(e["num_members"]) == len(e["min_cost_reduction"])
        return len(e["num_members"])

    clusters_found = max([clusters_in_event(e) for e in filtered_events])
    if num_clusters is not None:
        assert clusters_found <= num_clusters
    num_clusters = clusters_found
    by_expr = utils.group_by(filtered_events, lambda r: r["expr"])
    fig, axs = plt.subplots(
        len(by_expr), 1, figsize=[20, 6 * len(by_expr)], squeeze=False,
    )
    x_axis_func = lambda r: r["generation"]
    for ax, (expr, logs) in zip(axs.ravel(), by_expr.items()):
        reps = plotting.pad_into_lists(
            utils.group_by(logs, lambda r: r["repetition"]).values(), x_axis_func
        )
        # There should be no gaps, as we try every expr in every generation, but some reps may end sooner than others
        # assert all([plotting.check_nones_only_at_end(rep) for rep in reps])
        reps = [plotting.carry_previous_over_none(rep) for rep in reps]

        def sortbycost(r):
            seq = (
                []
                if r is None
                else sorted(
                    zip(r["num_members"], r["min_cost_reduction"]),
                    key=utils.star(lambda _nm, mcr: mcr),
                )
            )
            return seq + [(float("nan"), float("nan"))] * (num_clusters - len(seq))

        clusters = [[sortbycost(r) for r in rep] for rep in reps]
        clusters = np.array(clusters, dtype=np.float64)
        num_members = clusters[:, :, :, 0]
        min_reduction = clusters[:, :, :, 1]

        x_vals = range(1, min_reduction.shape[1] + 1)
        for k in range(min_reduction.shape[2]):
            plotting.plot_with_confidence(
                ax,
                x_vals,
                min_reduction[:, :, k],
                plotting.colors(k),
                linestyle="-",
                label="cluster {}".format(k),
            )
            for j, cluster_size in enumerate(
                np.nanmedian(num_members[:, :, k], axis=0)
            ):
                ax.plot(
                    x_vals[j],
                    np.nanmedian(min_reduction[:, j, k], axis=0),
                    "o",
                    color=plotting.colors(k),
                    markersize=min(3.0 * np.log(cluster_size), 15),
                )
        expr = utils.format_expr(expr, expr_limit=50)
        ax.set_title("{}".format(expr))

    plt.xlabel("Generations", fontsize=16)
    plt.figlegend(*ax.get_legend_handles_labels(), loc="upper left")

    fig.tight_layout(rect=[0, 0, 1, 0.95])
    title_suffix = "\n".join(wrap(title_suffix, 120))
    fig.suptitle("Clustering summary for {}".format(title_suffix), fontsize=16)
    plt.savefig(outfile)


def plot_clustering_summary_from_config(config, events):
    plot_clustering_summary(
        plotting.format_figure_filename(config, "clustering_summary.png"),
        plotting.config_suffix(config),
        events,
        config["num_episode_clusters"],
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
        logs = experiment_result.load_events_from_config(config)
        plot_clustering_summary_from_config(config, logs)
    else:
        plot_clustering_summary("clustering_summary.png", "", events=config)


if __name__ == "__main__":
    main()
