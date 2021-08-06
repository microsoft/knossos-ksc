import argparse
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy

from rlo.best_results import best_cost_for_exp
from rlo.costs import compare_epsilon
from rlo import experiment_result
from rlo import plotting
from rlo import utils


def success_rate_from_final_costs(counter, target_cost):
    return sum(
        [count for cost, count in counter.items() if float(cost) <= target_cost]
    ) / sum(counter.values())


def plot_success_from_config(config, events):
    by_expr = utils.group_by(
        [e for e in events if "eval_expr" in e and e["event"] == "rollout_end"],
        lambda r: r["eval_expr"],
    )
    fig, axs = plt.subplots(2, 1, figsize=[12, 10])

    # First trained generation is labelled 1; 0 is before any training
    x_axis_func = lambda r: int(r["generation"])
    N_GENERATIONS = (
        max([x_axis_func(rec) for b_e in by_expr.values() for rec in b_e]) + 1
    )  # Includes the pre-training "generation"
    x_vals = range(N_GENERATIONS)

    max_num_episodes = utils.single_elem(
        set(
            [
                sum(rec["final_costs"].values())
                for b_e in by_expr.values()
                for rec in b_e
            ]
        )
    )
    N_REPETITIONS = (
        max([int(rec["repetition"]) for b_e in by_expr.values() for rec in b_e]) + 1
    )  # First is repetition=0

    for (expr, logs) in by_expr.items():
        reps = plotting.pad_into_lists(
            utils.group_by(logs, lambda r: r["repetition"]).values(), x_axis_func
        )
        # Repetitions may skip generations if they didn't train.
        reps = [plotting.carry_previous_over_none(rep) for rep in reps]

        col = plotting.random_color_from_string(expr)

        all_final_costs = numpy.array(
            [[utils.counter_to_list(r["final_costs"]) for r in rep] for rep in reps]
        )
        assert all_final_costs.shape == (N_REPETITIONS, N_GENERATIONS, max_num_episodes)
        all_final_costs = all_final_costs.transpose(1, 0, 2).reshape(
            (N_GENERATIONS, N_REPETITIONS * max_num_episodes)
        )

        # Min cost graph at top
        plotting.plot_with_confidence(
            axs[0], x_vals, all_final_costs, col, label=expr, probs=[0, 50], axis=1
        )

        # Add on the best cost seen or possible
        best_cost = all_final_costs.min()
        linestyle_best = ":"  # If best_cost is only best achieved, make it dotted

        if config["oracle"]:
            best_oracle_cost = best_cost_for_exp(expr, config["rules"]).cost
        else:
            best_oracle_cost = None

        if best_oracle_cost is not None:
            assert best_cost >= best_oracle_cost - compare_epsilon
            best_cost = best_oracle_cost
            linestyle_best = "-"  # Solid line

        axs[0].plot(
            [0, N_GENERATIONS], [best_cost] * 2, linestyle_best, color=col, linewidth=1
        )

        success = [
            [success_rate_from_final_costs(r["final_costs"], best_cost) for r in rep]
            for rep in reps
        ]

        # Success rates beneath. Note this is equivalent to the 100%-oracle-cost subplot in the plot_costs frequency plot.
        plotting.plot_with_confidence(
            axs[1],
            x_vals,
            success,
            col,
            label=expr,
            linestyle=linestyle_best,
            probs=[20, 50, 80],
        )

    plt.xlabel("Generations", fontsize=12)
    suffix = plotting.config_suffix(config)
    axs[0].set_title("Min, median and mean absolute costs.{}".format(suffix))
    axs[0].set_ylabel("Cost", fontsize=12)
    axs[0].grid(True)
    axs[1].set_title("Success rates vs oracle.{}".format(suffix))
    axs[1].set_ylabel("Success rate", fontsize=12)
    axs[1].grid(True)

    fig.tight_layout(rect=[0.0, 0.0, 0.8, 1.0])

    plt.figlegend(
        *axs[1].get_legend_handles_labels(),
        loc="center right",
        fontsize=8,
        bbox_to_anchor=(0.9, 0.5)
    )
    plt.savefig(plotting.format_figure_filename(config, "cost_success.png"))
    plt.close()


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
    plot_success_from_config(config, logs)


if __name__ == "__main__":
    main()
