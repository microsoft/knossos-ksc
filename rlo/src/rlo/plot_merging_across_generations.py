import argparse

import matplotlib.pyplot as plt
import numpy as np

from rlo import experiment_result
from rlo import plotting
from rlo import utils


def plot_merging_across_generations(outfile, events, suffix=""):
    # Filter down to only the relevant refiner events.
    events = [
        event
        for event in events
        if event["event"]
        in ["merge_with_cache_across_generations", "update_cache_across_generations"]
    ]

    # Gather starting expressions, and sort for determinism.
    all_starting_expressions = [event["expr"] for event in events]
    all_starting_expressions = sorted(list(set(all_starting_expressions)))

    num_generations = max([event["generation"] for event in events])

    # Group events by repetition.
    events = utils.group_by(events, lambda event: event["repetition"])

    num_repetitions = max(events.keys()) + 1

    # Cache size after every generation; note the cache is shared across expresssions.
    cache_sizes = np.full((num_repetitions, num_generations), fill_value=np.nan)

    # Sum of dataset sizes, which is an upper bound on cache size.
    cache_sizes_upper_bound = np.full(
        (num_repetitions, num_generations), fill_value=np.nan
    )

    # Fraction of changes after every generation, grouped by expression and then repetition.
    fraction_of_expr_changes_per_rep = {
        expr: np.full((num_repetitions, num_generations), fill_value=np.nan)
        for expr in all_starting_expressions
    }

    for repetition_id, repetition_events in events.items():
        # A fixed repetition could have performer less generations than `num_generations`.
        repetition_num_generations = max(
            [event["generation"] for event in repetition_events]
        )

        # Group further by starting expression.
        repetition_events = utils.group_by(
            repetition_events, lambda event: event["expr"]
        )

        sum_dataset_sizes = np.full(repetition_num_generations, fill_value=0.0)

        for starting_expr, repetition_expr_events in repetition_events.items():
            dataset_update_events = [
                event
                for event in repetition_expr_events
                if event["event"] == "merge_with_cache_across_generations"
            ]

            cache_update_events = [
                event
                for event in repetition_expr_events
                if event["event"] == "update_cache_across_generations"
            ]

            dataset_sizes = np.full(num_generations, fill_value=np.nan)
            dataset_changes = np.full(num_generations, fill_value=np.nan)

            for event in dataset_update_events:
                dataset_changes[event["generation"] - 1] = event[
                    "num_expressions_changed"
                ]

            for event in cache_update_events:
                # these events have generations 1, 2, ..., NUM_GENERATIONS
                generation = event["generation"] - 1

                # In each generation, the cache is updated by each expression. The `max` here
                # ensures we store the cache size as it was at the end of the generation.
                cache_sizes[repetition_id][generation] = np.nanmax(
                    [
                        event["num_expressions_in_cache_after"],
                        cache_sizes[repetition_id][generation],
                    ]
                )

                dataset_sizes[generation] = event["num_expressions_in_dataset"]

            sum_dataset_sizes += np.where(np.isnan(dataset_sizes), 0.0, dataset_sizes)[
                :repetition_num_generations
            ]

            # Store the fraction of expressions that changed.
            fraction_of_expr_changes_per_rep[starting_expr][repetition_id] = (
                dataset_changes / dataset_sizes
            )

        # Store sum of dataset sizes, making sure we keep nan's at the end.
        cache_sizes_upper_bound[repetition_id][:repetition_num_generations] = np.cumsum(
            sum_dataset_sizes
        )

    _, axs = plt.subplots(2, 1, figsize=(12, 8))

    plotting.plot_with_confidence(
        axs[0], range(num_generations), cache_sizes, col="red", label="cache size",
    )

    plotting.plot_with_confidence(
        axs[0],
        range(num_generations),
        cache_sizes_upper_bound,
        col="black",
        linestyle=":",
        label="sum of dataset sizes (cache size upper bound)",
    )

    axs[0].legend()

    for starting_expr in all_starting_expressions:
        fraction_of_changes = fraction_of_expr_changes_per_rep[starting_expr]

        # Some expressions may be missing from some generations, creating all-nan columns.
        is_all_nan_generation = np.all(np.isnan(fraction_of_changes), axis=0)
        (generations_to_plot,) = np.where(
            ~is_all_nan_generation
        )  # np.where returns a 1-tuple

        plotting.plot_with_confidence(
            axs[1],
            generations_to_plot,
            fraction_of_changes[:, generations_to_plot],
            col=plotting.random_color_from_string(starting_expr),
            label=utils.format_expr(starting_expr, expr_limit=22),
        )

    axs[0].set_title("BestAcrossGenerationsRefiner cache size\n" + suffix)
    axs[0].set_ylabel("Number of expressions")

    axs[1].set_title(
        "Fraction of expressions in the dataset for which higher values were found in "
        "the cache (one color per starting expression)"
    )
    axs[1].set_ylim((0.0, 1.0))
    axs[1].set_xlabel("Generation")
    axs[1].set_ylabel("Fraction of expressions")
    legend = axs[1].legend(loc="center right", bbox_to_anchor=(1.4, 1.1))

    plt.savefig(outfile, bbox_extra_artists=(legend,), bbox_inches="tight")
    plt.close()


def plot_merging_across_generations_from_config(config, events):
    outfile = plotting.format_figure_filename(
        config, "merging_across_generations_summary.png"
    )

    suffix = plotting.config_suffix(config)
    plot_merging_across_generations(outfile, events=events, suffix=suffix)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="a run ID (e.g., 2019_01_06_13_15_48_13172) or path to a config.json file",
    )
    args = parser.parse_args()

    config = experiment_result.load_config(args.run_id)
    events = experiment_result.load_events_from_config(config)
    plot_merging_across_generations_from_config(config, events)


if __name__ == "__main__":
    main()
