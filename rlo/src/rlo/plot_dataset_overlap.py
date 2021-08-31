# mypy: ignore-errors
import argparse
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from textwrap import wrap
from typing import Mapping, Sequence

from rlo import experiment_result
from rlo import plotting
from rlo import sparser


def plot_dataset_overlap(
    events, outfile: str, title_suffix: str, repetition: int, hash_exprs: bool = True,
):
    gens_by_expr = {}
    intersection_counts: Mapping[
        int, Sequence[int]
    ] = {}  # gen -> [gen' < gen] -> number of common expressions
    expr_counts: Mapping[int, int] = {}
    for e in events:
        if e["event"] == "distill_fit" and e["repetition"] == repetition:
            gen = e["generation"]
            exprs = e["target_and_fitted"].keys()
            if hash_exprs:
                exprs = [sparser.parse_expr(expr) for expr in exprs]
            print(
                f"Generation {gen} with {len(exprs)} expressions (prev total {len(gens_by_expr)})"
            )
            if gen in expr_counts:
                # Every distill_fit event should be for a different generation, except for two-value-function,
                # where we get one event per model...
                assert "distill_net" in e
                # Check both models have (roughly) the same Expressions, and ignore the 2nd.
                assert all(gen in gens_by_expr[expr] for expr in exprs)
                continue
            assert gen not in expr_counts
            expr_counts[gen] = len(exprs)
            assert gen not in intersection_counts
            # In fact this relies on seeing the generations in increasing order.
            # If logs come more out of order than that, this will need adapting.
            intersections = [0] * gen
            intersection_counts[gen] = intersections
            for expr in exprs:
                previously_seen_in_gens = gens_by_expr.setdefault(expr, [])
                for prev_gen in previously_seen_in_gens:
                    intersections[prev_gen] += 1
                previously_seen_in_gens.append(gen)
    generations = sorted(expr_counts.keys())
    num_generations = max(generations) + 1
    expression_overlap = np.zeros((num_generations, num_generations))
    for gen1 in generations:
        intersections = intersection_counts.get(gen1, [])
        for gen2 in range(1, len(intersections)):
            # Each point is the proportion of the expressions in the X (first) axis generation
            # that were also seen in the Y (second) axis generation
            expression_overlap[gen1][gen2] = intersections[gen2] / expr_counts[gen1]
            expression_overlap[gen2][gen1] = intersections[gen2] / expr_counts[gen2]
        expression_overlap[gen1, gen1] = 1.0
    plt.imshow(
        expression_overlap, interpolation=None, vmin=0.0, vmax=1.0, cmap="nipy_spectral"
    )
    plt.colorbar()
    plt.title(
        "\n".join(
            wrap(
                f"Overlap of expressions in dataset (rep {repetition}) {title_suffix}",
                60,
            )
        )
    )
    plt.savefig(outfile)


def plot_dataset_overlap_from_config(config, events, repetition=0, hash_exprs=True):
    plot_dataset_overlap(
        events,
        plotting.format_figure_filename(config, "dataset_overlap.png"),
        plotting.config_suffix(config),
        repetition,
        hash_exprs,
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("run_id", type=str)
    parser.add_argument("--repetition", type=int, default=0)
    parser.add_argument("--no_hash_exprs", action="store_false", dest="hash_exprs")
    args = parser.parse_args()
    config, events = experiment_result.load_config_events(args.run_id, verbosity=1)
    plot_dataset_overlap_from_config(config, events, args.repetition, args.hash_exprs)


if __name__ == "__main__":
    main()
