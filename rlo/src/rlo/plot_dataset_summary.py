import argparse
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from collections import Counter
import numpy as np
import os

from rlo.factory import num_time_heads_from_config
from rlo import experiment_result
from rlo import plotting
from rlo import utils
from typing import Dict, List


def plot_dataset_summary(
    events,
    title_suffix,
    outfile,
    time_bin_splits,
    value_bin_splits,
    episode_bin_splits,
    num_time_heads=None,
    distill_net=None,
):

    fig, (ax1, ax2, ax3, ax4) = plt.subplots(4, 1, figsize=[15, 6 * 4])
    search_events, dataset_events = [], []
    for e in events:
        if e["event"] == "distill_fit" and e.get("distill_net") == distill_net:
            dataset_events.append(
                {k: e[k] for k in ["repetition", "generation", "target_and_fitted"]}
            )
        elif e["event"] == "rollout_end" and "eval_expr" not in e:
            # There are many search events, with much data we don't need, so reduce memory usage
            search_events.append(
                {k: e[k] for k in ["repetition", "generation", "episode_lengths"]}
            )

    max_value = max(
        v
        for e in dataset_events
        for vals in e["target_and_fitted"].values()
        for v, _fitted in vals
    )

    def get_labels_num_bins(bins):
        return (
            len(bins) + 1,
            ["<" + str(round(b, 2)) for b in bins] + [">=" + str(round(bins[-1], 2))],
        )

    num_time_bins, time_labels = get_labels_num_bins(time_bin_splits)
    num_ep_bins, episode_labels = get_labels_num_bins(episode_bin_splits)

    # some repetitions might be missing
    N_REPETITIONS = max(r["repetition"] for r in dataset_events) + 1
    # dataset_events contain generations 1, 2, ..., N_GENERATIONS
    N_GENERATIONS = max(r["generation"] for r in dataset_events)

    all_time_bins = np.full((N_GENERATIONS, N_REPETITIONS, num_time_bins), float("nan"))
    all_episode_bins = np.full(
        (N_GENERATIONS, N_REPETITIONS, num_ep_bins), float("nan")
    )

    if not value_bin_splits:
        if max_value <= 1:
            value_bin_splits = [0.2, 1]
        else:
            value_bin_splits = (
                [0.2] + np.logspace(0, np.log10(max_value), num=9).tolist()[:-1]
            )  # split into 10 bins
    num_value_bins, value_labels = get_labels_num_bins(value_bin_splits)
    all_value_bins = np.full(
        (N_GENERATIONS, N_REPETITIONS, num_value_bins), float("nan")
    )
    num_exprs = np.full((N_GENERATIONS, N_REPETITIONS), float("nan"))
    total_num_points = np.full((N_GENERATIONS, N_REPETITIONS), float("nan"))

    def loop_over_generation_rep(events):
        events_by_generation = utils.group_by(events, lambda r: r["generation"])
        # Since we pre-allocate all arrays filled with nan's,
        # the order of iteration does not matter
        for generation, events_generation in events_by_generation.items():
            generation_index = generation - 1
            events_by_rep = utils.group_by(events_generation, lambda r: r["repetition"])
            for rep, events_rep in events_by_rep.items():
                yield generation_index, rep, events_rep

    for generation_index, rep, distill_events_rep in loop_over_generation_rep(
        dataset_events
    ):
        target_and_fitted: Dict[str, List[List[float]]] = utils.single_elem(
            distill_events_rep
        )["target_and_fitted"]
        num_exprs[generation_index, rep] = len(target_and_fitted)
        total_num_points[generation_index, rep] = sum(
            len(vals) for vals in target_and_fitted.values()
        )

        # bin times into groups
        times = [t for vals in target_and_fitted.values() for t in range(len(vals))]
        time_bins, _ = np.histogram(
            times, bins=[0] + time_bin_splits + [np.float("inf")]
        )
        all_time_bins[generation_index, rep, :] = np.cumsum(time_bins) / sum(time_bins)

        # bin values into groups
        values = [
            target for vals in target_and_fitted.values() for target, _fitted in vals
        ]
        value_bins, _ = np.histogram(
            values, bins=[0] + value_bin_splits + [np.float("inf")]
        )
        all_value_bins[generation_index, rep, :] = np.cumsum(value_bins) / sum(
            value_bins
        )

    for generation_index, rep, search_events_rep in loop_over_generation_rep(
        search_events
    ):
        total_episode_lengths_generation_rep = Counter()
        for r in search_events_rep:
            total_episode_lengths_generation_rep += r["episode_lengths"]

        episode_lengths = [int(k) for k in total_episode_lengths_generation_rep.keys()]
        episode_weights = [
            total_episode_lengths_generation_rep[str(e)] for e in episode_lengths
        ]
        episode_bins, _ = np.histogram(
            episode_lengths,
            weights=episode_weights,
            bins=[0] + episode_bin_splits + [np.float("inf")],
        )
        all_episode_bins[generation_index, rep, :] = np.cumsum(episode_bins) / sum(
            episode_bins
        )

    # plot number of points summary
    x_values = list(range(1, N_GENERATIONS + 1))
    plotting.plot_with_confidence(ax1, x_values, total_num_points, axis=1, col="blue")
    ax1b = ax1.twinx()
    plotting.plot_with_confidence(ax1b, x_values, num_exprs, axis=1, col="red")
    ax1.set_ylabel("number of points", fontsize=10, color="blue")
    ax1.tick_params(axis="y", labelcolor="blue")
    ax1b.set_ylabel("number of exprs", fontsize=10, color="red")
    ax1b.tick_params(axis="y", labelcolor="red")
    ax1.set_xlabel("generations", fontsize=10)
    ax1.title.set_text("Size of dataset")
    ax1.grid(True)

    def plot_bin_dist(num_bins, ax, summary_array, labels, title, num_time_heads=None):
        for i in range(num_bins):
            plotting.plot_with_confidence(
                ax,
                x_values,
                summary_array[:, :, i],
                col=plotting.colors(i),
                axis=1,
                label=labels[i],
            )
        ax.set_ylabel("fraction of dataset", fontsize=10)
        ax.set_xlabel("generation", fontsize=10)
        if num_time_heads:
            ax.title.set_text(
                title
                + " distribution in the dataset (num_time_heads="
                + str(num_time_heads)
                + ")"
            )
        else:
            ax.title.set_text(title + " distribution in the dataset")
        ax.grid(True)
        ax.legend()

    # plot value, time, episode distributions
    plot_bin_dist(num_value_bins, ax2, all_value_bins, value_labels, "Value")
    plot_bin_dist(
        num_time_bins, ax3, all_time_bins, time_labels, "Time", num_time_heads
    )
    plot_bin_dist(
        num_ep_bins,
        ax4,
        all_episode_bins,
        episode_labels,
        "Episode lengths",
        num_time_heads,
    )

    fig.suptitle("Dataset summary " + title_suffix, fontsize=18)
    fig.tight_layout(rect=[0, 0, 1, 0.95])
    fig.savefig(outfile)
    fig.clear()


def plot_dataset_summary_from_config(
    config, events, value_bin_splits=None, time_bin_splits=None, episode_bin_splits=None
):

    title_suffix = plotting.config_suffix(config)
    num_time_heads = num_time_heads_from_config(config)
    if value_bin_splits is None:
        value_bin_splits = config.get("value_bin_splits")
    if time_bin_splits is None:
        time_bin_splits = (
            config.get("time_bin_splits")
            if config.get("time_bin_splits")
            else np.arange(5, num_time_heads, num_time_heads // 7).tolist()
        )
    if episode_bin_splits is None:
        episode_bin_splits = (
            config.get("episode_bin_splits")
            if config.get("episode_bin_splits")
            else np.arange(5, num_time_heads, num_time_heads // 7).tolist()
        )
    # if we have two models, we plot the dataset using only one of them.
    # (both models log out the same dataset)
    model_name = "model_1" if config.get("two_value_func") is not None else None
    outfile = plotting.format_figure_filename(config, "dataset_summary.png")
    plot_dataset_summary(
        events,
        title_suffix,
        outfile,
        time_bin_splits,
        value_bin_splits,
        episode_bin_splits,
        num_time_heads_from_config(config),
        model_name,
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="(1) a run ID (e.g., 2019_01_06_13_15_48_13172), (2) path to a config.json file, or (3) path to a directory containing /rep/events.json files",
    )
    parser.add_argument(
        "--time_bin_splits",
        nargs="*",
        type=int,
        default=None,
        help="bin splits to use for time distribution plot",
    )
    parser.add_argument(
        "--episode_bin_splits",
        nargs="*",
        type=int,
        default=None,
        help="bin splits to use for episode lengths distribution plot",
    )
    parser.add_argument(
        "--value_bin_splits",
        nargs="*",
        type=float,
        default=None,
        help="bin splits to use for value distribution plot",
    )
    args = parser.parse_args()

    config, logs = experiment_result.load_config_events(args.run_id, verbosity=1)
    if config is not None:
        plot_dataset_summary_from_config(
            config,
            logs,
            args.value_bin_splits,
            args.time_bin_splits,
            args.episode_bin_splits,
        )
    else:
        plot_dataset_summary(
            logs,
            args.run_id,
            os.path.join(args.run_id, "dataset_summary.png"),
            args.value_bin_splits,
            args.time_bin_splits,
            args.episode_bin_splits,
        )


if __name__ == "__main__":
    main()
