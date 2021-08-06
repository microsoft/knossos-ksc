import argparse
import json
import math
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
import numpy as np

from rlo.utils import zip_values


# Dictionary of (time_budget, expression): value
DatasetDict = Dict[Tuple[int, str], float]


def load_dataset(path: str) -> DatasetDict:
    with open(path) as f:
        dataset = json.load(f)

    return {
        (time_left, expression): value for (time_left, expression, value) in dataset
    }


def show_dataset_sequence_diff(datasets: List[DatasetDict], output_file: str):
    """Plot differences between adjacent datasets in a sequence."""
    all_intersections_over_unions = []
    all_errors = []

    for (dataset, dataset_next) in zip(datasets, datasets[1:]):
        intersection = zip_values(dataset, dataset_next)

        intersection_size = len(intersection)
        union_size = len(set(dataset.keys()) | set(dataset_next.keys()))

        all_intersections_over_unions.append(float(intersection_size) / union_size)
        all_errors.append(
            np.array(
                [value_2 - value_1 for (value_1, value_2) in intersection.values()]
            )
        )

    _, axs = plt.subplots(2, figsize=(10, 10))

    axs[0].plot(all_intersections_over_unions, label="intersection over union")
    axs[0].set_ylim((0.0, 1.0))

    axs[1].plot([np.mean(errors) for errors in all_errors], label="mean difference")
    axs[1].plot([np.max(errors) for errors in all_errors], label="max increase")
    axs[1].plot([-np.min(errors) for errors in all_errors], label="max decrease")
    axs[1].plot(
        [np.mean(np.abs(errors)) for errors in all_errors],
        label="mean absolute difference",
    )

    axs[1].set_yscale("symlog")

    for ax in axs:
        ax.legend(loc=0)
        ax.grid(True)
        ax.set_xticks(range(0, len(datasets), 2))

    plt.savefig(output_file)
    plt.close()


def print_examples(examples: List[Tuple[Tuple[int, str], float, float]]):
    for (key, value_before, value_after) in examples:
        print("Input", key)
        print("Value before: {}, value after: {}".format(value_before, value_after))

    print("")


def get_top_k_elements(elements: List, k: int, key) -> List:
    """Get top `k` of `elements` by `key`."""
    return sorted(elements, key=key, reverse=True)[:k]


def show_dataset_pair_diff(
    dataset_before: DatasetDict, dataset_after: DatasetDict, print_num_examples: int
):
    """Shows statistics for how two datasets differ."""
    print(
        "Dataset size before: {}, after: {}".format(
            len(dataset_before), len(dataset_after)
        )
    )

    intersection = zip_values(dataset_before, dataset_after)
    print("Intersection size: {}\n".format(len(intersection)))

    intersection_gain: List[Tuple] = []
    intersection_drop: List[Tuple] = []

    for (key, (value_before, value_after)) in intersection.items():
        if math.isclose(value_before, value_after):
            continue

        target = intersection_gain if value_after > value_before else intersection_drop
        target.append((key, value_before, value_after))

    print("Number of inputs with value gain:", len(intersection_gain))
    print("Inputs with largest gain")

    print_examples(
        get_top_k_elements(intersection_gain, print_num_examples, lambda e: e[2] - e[1])
    )

    print("Number of inputs with value drop:", len(intersection_drop))
    print("Inputs with largest drop")

    print_examples(
        get_top_k_elements(intersection_drop, print_num_examples, lambda e: e[1] - e[2])
    )


def show_expression_progress(
    datasets: List[DatasetDict],
    target_expression: str,
    success_threshold: float,
    output_file: str,
):
    """Show progress on a single expression, over a sequence of datasets."""

    # Indices of datasets where there was a "success" on the target expression,
    # along with the minimum time budget required.
    first_success_indices = []
    first_success_times = []

    # Indices of datasets where the target expression was present in the dataset.
    in_dataset_indices = []

    for i in range(len(datasets)):
        # All occurrences of the target expression.
        target_occurrences = [
            (time_left, value)
            for ((time_left, expression), value) in datasets[i].items()
            if expression == target_expression
        ]

        if target_occurrences:
            in_dataset_indices.append(i)

        # Only the occurrences were the value exceeded the success threshold.
        successes = sorted(
            [
                time_left
                for (time_left, value) in target_occurrences
                if value >= success_threshold
            ]
        )

        if successes:
            # Values should be not-decreasing when the time budget increases.
            assert successes == list(range(successes[0], successes[-1] + 1))

            first_success_indices.append(i)
            first_success_times.append(successes[0])

    plt.plot(
        first_success_indices,
        first_success_times,
        label="First success (value >= {})".format(success_threshold),
    )

    plt.plot(in_dataset_indices, [0] * len(in_dataset_indices), label="In dataset")

    plt.xticks(range(len(datasets)))
    plt.legend(loc=1)

    plt.savefig(output_file)
    plt.close()


def add_arguments_for_sequence_command(parser):
    parser.add_argument(
        "paths", type=str, nargs="+", help="list of paths to datasets",
    )

    parser.add_argument(
        "--output_file",
        type=str,
        default="dataset_sequence_diff.png",
        help="path to save the output plot",
    )


def add_arguments_for_pair_command(parser):
    parser.add_argument(
        "path_1", type=str, help="first dataset for the comparison",
    )

    parser.add_argument(
        "path_2", type=str, help="second dataset for the comparison",
    )

    parser.add_argument(
        "--num_examples",
        type=int,
        default=1,
        help="how many extreme examples to print",
    )


def add_arguments_for_expression_command(parser):
    parser.add_argument(
        "paths", type=str, nargs="+", help="list of paths to datasets",
    )

    parser.add_argument(
        "--expression", type=str, help="expression to look at",
    )

    parser.add_argument(
        "--success_threshold",
        type=float,
        help="threshold (minimum value) that defines success",
    )

    parser.add_argument(
        "--output_file",
        type=str,
        default="first_success.png",
        help="path to save the output plot",
    )


def main():
    parser = argparse.ArgumentParser(
        """Show differences between datasets at different levels of granularity.

To plot high-level difference of a sequence of datasets (typically datasets from several consecutive generations)
$ python src/dataset_diff.py sequence ./path/to/dataset/from_generation_*

To print some statistics and most differing expressions for a pair of datasets (typically from two consecutive generations)
$ python src/dataset_diff.py pair ./path/to/dataset/from_generation_05.json ./path/to/dataset/from_generation_06.json

To plot progress on shortening the optimization path for a single expression over a sequence of datasets
$ python src/dataset_diff.py expression ./path/to/dataset/from_generation_* --expression '(mul 1.0 x)' --success_threshold 0.5
"""
    )

    subparsers = parser.add_subparsers(
        dest="command", help="possible commands to execute"
    )

    parser_sequence = subparsers.add_parser(
        "sequence", help="plot high-level differences in a sequence of datasets"
    )

    parser_pair = subparsers.add_parser(
        "pair", help="show most differing datapoints for a pair of datasets"
    )

    parser_expression = subparsers.add_parser(
        "expression", help="analyze a single expression in a sequence of datasets",
    )

    add_arguments_for_sequence_command(parser_sequence)
    add_arguments_for_pair_command(parser_pair)
    add_arguments_for_expression_command(parser_expression)

    args = parser.parse_args()

    if args.command == "sequence":
        print("Analyzing datasets:", args.paths, "\n")

        show_dataset_sequence_diff(
            datasets=[load_dataset(path) for path in args.paths],
            output_file=args.output_file,
        )
    elif args.command == "pair":
        print("Analyzing dataset ", args.path_1, "vs", args.path_2, "\n")

        show_dataset_pair_diff(
            dataset_before=load_dataset(args.path_1),
            dataset_after=load_dataset(args.path_2),
            print_num_examples=args.num_examples,
        )
    elif args.command == "expression":
        print("Analyzing datasets:", args.paths, "\n")
        print("Target expression:", args.expression, "\n")

        show_expression_progress(
            datasets=[load_dataset(path) for path in args.paths],
            target_expression=args.expression,
            success_threshold=args.success_threshold,
            output_file=args.output_file,
        )


if __name__ == "__main__":
    main()
