import argparse
import pandas as pd

from rlo import experiment_result
from rlo import factory
from rlo.plot_costs import make_dataframe, compute_optimality


def output_exprs_solved(config, events):
    df = compute_optimality(
        make_dataframe(events), factory.best_cost_from_config(config)
    )

    # Take max over episodes in search (for astar-like search, it is alaready a singleton)
    optimality = df.groupby(["generation", "expr", "repetition"])["optimality"].max()

    grouped = (optimality >= 1.0).astype(int).groupby(["generation", "repetition"])
    avg_opt = optimality.groupby(["generation", "repetition"]).mean().rename("avg_opt")
    num_solved = grouped.sum().rename("num_solved")
    num_exprs = grouped.size().rename("num_exprs")
    num_solved = pd.concat([avg_opt, num_solved, num_exprs], axis=1)

    return num_solved, num_solved.groupby("generation").mean()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="(1) a run ID (e.g., 2019_01_06_13_15_48_13172), (2) path to a config.json file, or (3) path to a directory containing /rep/events.json files",
    )

    args = parser.parse_args()
    config, logs = experiment_result.load_config_events(args.run_id)
    print(output_exprs_solved(config, logs))


if __name__ == "__main__":
    main()
