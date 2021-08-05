import os
from tabulate import tabulate

from rlo import analytics
from rlo.factory import (
    searcher_from_config,
    num_time_heads_from_config,
    rng_factory_from_config,
)
from rlo.expr_sets import get_expression_set
from rlo import rewrites
from rlo.experts import get_expert, ExpertWrapper
from rlo.flags import (
    make_config_for_scenario,
    run_arguments,
    make_parser,
)
from rlo import utils
from rlo.utils import FunctionArgs as Args
from rlo.best_results import TypeBestCost
from rlo.experiment_result import (
    load_config_events,
    save_config,
)
from rlo.print_best_episodes import (
    rewrite_sequence_summary,
    make_summary,
)
from rlo.plot_time_lefts_during_search import plot_time_lefts_during_search_from_config


def do_expert_search(expr_set, expert, searcher, random_source):
    """
        Performs search using expert values.
    """

    for expr in expr_set.named_exprenvs():
        print(f"Processing {expr.name}...")
        with analytics.Scope(
            generation=1, repetition=0
        ):  # generation and repetition are required for plotting
            searcher.eval(expert, random_source, expr)


def compute_expert_best_costs(expert, expr_set, num_time_heads):
    best_expert_costs = {}
    for name, exprenv in expr_set.named_exprenvs():
        expert_best_cost, _ = expert.get_min_cost_length(
            exprenv, time_budget=num_time_heads - 1,
        )
        best_expert_costs[name] = TypeBestCost(expert_best_cost, "expert", None)

    def get_best_cost(expr, _time):
        return best_expert_costs[expr]

    return get_best_cost


def compute_success(output_path, summary, num_time_heads, optimality_threshold):
    """
        Computes success and saves it to .txt file. It is reported in the table format
        as in the example below:

        Success rate: 97.96 
        -------  -----------------------  ---------  -----------  ---------------------------  ----------
        outcome  expr                     init_cost  search_cost  expert_cost_time_budget_100  optimality
        success  float_matrix_multiply    1061.5     1061.2       1061.4                       3.0
        ...

    """
    success_count = 0

    titles = [
        [
            "outcome",
            "expr",
            "init_cost",
            "search_cost",
            f"expert_cost_time_budget_{num_time_heads}",
            "optimality",
        ]
    ]
    rows = []

    for cnt, e_name in enumerate(summary["eval_expr"]):
        df_for_expr = summary[summary["eval_expr"] == e_name].iloc[0]
        optimality = df_for_expr.optimality
        start_cost, search_cost, expert_cost = (
            round(df_for_expr.original_cost, 2),
            round(df_for_expr.best_cost, 2),
            round(df_for_expr.target_cost, 2),
        )
        if optimality >= optimality_threshold:
            rows.append(
                ["success", e_name, start_cost, search_cost, expert_cost, optimality]
            )
            success_count += 1
        else:
            rows.append(
                ["failure", e_name, start_cost, search_cost, expert_cost, optimality]
            )

    success_rate = round(success_count * 100 / (cnt + 1), 2)
    with open(
        os.path.join(output_path, f"success_report_{optimality_threshold}.txt"), "w"
    ) as f:
        f.write(f"Success rate: {success_rate} \n")
        f.write(tabulate(titles + rows))


def get_time_expr_expert_from_config(config):
    num_time_heads = num_time_heads_from_config(config)
    expr_set = get_expression_set(
        config["test_exprs"], take_defs=config.get("test_on_exprs")
    )
    expert_name = config["expert_name"]
    rules = rewrites.get_rules(config["rules"])
    expert = get_expert(expert_name, rules)
    return num_time_heads, expr_set, expert


def get_random_source_from_config(config):
    if config["eval_search"] in ["beam", "astar"]:
        random_source = None
    else:
        generation_rng = rng_factory_from_config(config)
        random_source = utils.seed(utils.rng(generation_rng(1)))
    return random_source


def search_from_config(config):
    (num_time_heads, expr_set, expert) = get_time_expr_expert_from_config(config)
    random_source = get_random_source_from_config(config)
    searcher = searcher_from_config(config, phase="eval")
    expert_wrapper = ExpertWrapper(expert, num_time_heads)
    do_expert_search(expr_set, expert_wrapper, searcher, random_source)


def plot_and_summarise_results_from_config(config, events, optimality_threshold=[0.95]):
    (num_time_heads, expr_set, expert) = get_time_expr_expert_from_config(config)

    summary = make_summary(
        events,
        generations=[1],
        start_exprenvs=dict(expr_set.named_exprenvs()),
        best_cost_func=compute_expert_best_costs(expert, expr_set, num_time_heads),
    )
    expr_names = [name for name, _ in expr_set.named_exprenvs()]
    ks_out = rewrite_sequence_summary(expr_names, summary, [])
    with open(os.path.join(config["result_save_path"], f"best_episodes.kso"), "w") as f:
        f.write(ks_out)

    for threshold in optimality_threshold:
        compute_success(config["result_save_path"], summary, num_time_heads, threshold)

    if config["eval_search"] in ["astar", "beam", "hybrid"]:
        plot_time_lefts_during_search_from_config(config, events, phase="eval")


def main():
    """
        Search with expert on a set of expressions from a scenario.

        To search:
        $ python src/search_with_expert.py scenario_name

        To search on a subset of expressions use --test_on_exprs flag:
        $ python src/search_with_expert.py scenario_name --test_on_exprs gemm gemv

        You can also override hyperparameters using flags available in train_over_expressions.py. For example,
        $ python src/search_with_expert.py scenario_name --eval_search rollout

        To load and plot search results from a previosuly ran search with expert experiment:
        $ python src/search_with_expert.py scenario_name --run_id experiment_id

    """
    search_with_expert_arguments = [
        Args(
            "--optimality_threshold",
            type=int,
            nargs="+",
            default=[0.95],
            help="thresholds for evaluating expert optimality results",
        ),
        Args(
            "--test_on_exprs",
            type=str,
            nargs="+",
            default=None,
            help="specify which expressions from scenario's expression set to evaluate the expert on",
        ),
    ]
    parser = make_parser(run_arguments + search_with_expert_arguments)
    args, _ = parser.parse_known_args()

    assert args.scenario in [
        "blas",
        "blas_combined",
    ], f"Scenario {args.scenario} is invalid! In experts.py we only have expert agent implemented for blas_combined and blas scenarios."
    expert_name = "blas"
    expert_rules = "ml_rules_no_bind"

    if args.run_id:
        run_id = args.run_id
    else:
        config = make_config_for_scenario(
            args.scenario, run_args=run_arguments + search_with_expert_arguments,
        )
        config["expert_name"] = expert_name
        assert (
            config["rules"] == expert_rules
        ), f"Ruleset {config['rules']} is invalid! In experts.py we only have expert agent implemented for ml_rules_no_bind."
        if config["repetition"] is None:
            config["repetition"] = 0
        run_id = config["run_id"]

        save_config(config)
        with analytics.log_events_to_files(
            os.path.join(config["result_save_path"], "search_with_expert_events/")
        ):
            search_from_config(config)

    config, events = load_config_events(run_id, verbosity=1)
    plot_and_summarise_results_from_config(
        config, events, optimality_threshold=args.optimality_threshold
    )


if __name__ == "__main__":
    main()
