import argparse
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
import numpy as np
from textwrap import wrap

from rlo import experiment_result
from rlo import expr_sets
from rlo import plotting
from rlo import utils


def check_expansions(expansions):
    min_time_left = None
    for _budget, time_left, _cost in expansions:
        assert min_time_left is None or time_left >= min_time_left - 1
        min_time_left = min(
            time_left, float("inf") if min_time_left is None else min_time_left
        )


def plot_time_lefts_during_search_from_config(
    config, events, phase="train", repetition=0, cmap="plasma"
):
    if phase == "train":
        expr_set = expr_sets.get_expression_set(config["train_exprs"])
        search_alg = config["train_search"]
        expr_key = "expr"
        max_gnn = config["max_gnn_train"]
    else:
        expr_set = expr_sets.get_expression_set(config["train_exprs"]).union(
            expr_sets.get_expression_set(
                config["test_exprs"], take_defs=config.get("test_on_defs")
            )
        )
        search_alg = config["eval_search"]
        expr_key = "eval_expr"
        max_gnn = config["max_gnn_eval"]

    expressions = dict(expr_set.named_exprenvs())
    try:
        expert = expr_set.get_expert(config["rules"])

        def sort_order(expr_name):
            expr = expressions[expr_name]
            return len(list(expert.optimize(expr)))

    except (ValueError, NotImplementedError):
        # cannot find an expert
        sort_order = lambda expr_name: expr_name

    events = [
        e
        for e in events
        if e["event"] == "rollout_end_verbose"
        and expr_key in e
        and e["repetition"] == repetition
    ]
    generations = sorted(set([e["generation"] for e in events]))
    exprs = sorted(set([e[expr_key] for e in events]), key=sort_order)
    if len(generations) * len(exprs) > 1000:
        gen_every = int(np.ceil((len(generations) * len(exprs)) / 1000))
        generations = [g for i, g in enumerate(generations) if (i % gen_every) == 0]
    print(generations)
    print(exprs)

    fig_width = 8 * len(generations)
    fig_height = 4 if len(exprs) == 1 else 2 * len(exprs)
    # matplotlib has a maximum figure size of 2^16 dots in each direction
    dpi = min(100, 0.999 * 2 ** 16 / max(fig_width, fig_height))
    fig, axs = plt.subplots(
        len(exprs),
        len(generations),
        figsize=(fig_width, fig_height),
        dpi=dpi,
        squeeze=False,
    )
    # Generations may be 0, 1, ..., NUM_GENERATIONS or 1, 2, ..., NUM_GENERATIONS
    axs = [dict(zip(generations, axrs)) for axrs in axs]
    for i, expr_name in enumerate(exprs):
        original_cost = expressions[expr_name].cost()
        best_cost = expr_set.best_cost_for_expression(
            expr_name, config["rules"], config[f"simulation_depth_{phase}"] + 1
        ).cost
        for generation in generations:
            ax = axs[i][generation]
            if generation == generations[0]:
                ax.set_ylabel("{}\ntime_left".format(utils.format_expr(expr_name, 25)))
            if i == 0:
                ax.set_title("Generation {}".format(generation))
            if i == len(exprs) - 1:
                ax.set_xlabel("Remaining GNN budget")
            expansions = [
                e
                for e in events
                if e[expr_key] == expr_name and e["generation"] == generation
            ]
            if len(expansions) == 0:
                # skip if expression did not appear in the current generation
                continue
            expansions = utils.single_elem(expansions)["expansions"]
            check_expansions(expansions)
            gnn_evals, time_left, cost = zip(*expansions)
            norm = Normalize(0.0, original_cost - best_cost, clip=True)
            ax.scatter(
                max_gnn - np.array(gnn_evals),
                time_left,
                c=original_cost - np.array(cost),
                marker="x",
                cmap=plt.get_cmap(cmap),
                norm=norm,
                linewidth=2,
            )
            ax.invert_xaxis()
            ax.grid(True)
            ax.set_ylim((0, config[f"simulation_depth_{phase}"]))
    plt.tight_layout(rect=[0, 0, 1, (fig_height - 1.0) / fig_height])
    fig.suptitle(
        "\n".join(
            wrap(
                "Time left vs. remaining GNN budget, repetition={}, search={}, {} (cmap={})".format(
                    repetition, search_alg, plotting.config_suffix(config), cmap
                ),
                80 * len(generations),
            )
        ),
        y=(fig_height - 0.5) / fig_height,
    )
    output_file = plotting.format_figure_filename(
        config, "time_lefts_{}.png".format(phase)
    )
    plt.savefig(output_file, dpi="figure")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("run_id", type=str, help="A run ID")
    parser.add_argument("--phase", choices=["train", "eval"], default="train")
    parser.add_argument("--repetition", type=int, default=0, help="Repetition")
    parser.add_argument("--cmap", type=str, default="plasma")
    args = parser.parse_args()
    config, events = experiment_result.load_config_events(args.run_id, verbosity=1)
    plot_time_lefts_during_search_from_config(
        config, events, args.phase, args.repetition, args.cmap
    )


if __name__ == "__main__":
    main()
