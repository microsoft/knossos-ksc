import json
import matplotlib.pyplot as plt
import os
from functools import reduce
from pyrsistent import pmap
from pyrsistent.typing import PMap
from typing import FrozenSet

from rlo import analytics
from rlo.dataset import StateValueDataset
from rlo.expression_util import SymtabAndDefs
from rlo import expr_sets
from rlo import sparser
from rlo.flags import (
    run_arguments,
    make_parser,
    make_config_for_scenario,
    check_save_config,
)
from rlo import utils
from rlo.utils import FunctionArgs as Args
from rlo import factory
from rlo.experiment_result import load_events_from_config
from rlo.plot_scatter_fitted import plot_scatter_fitted_from_config
from rlo.plot_intra_gen_loss import plot_intra_gen_loss_from_config
from ksc.type import Type


def train(config, dataset_list):

    model_wrapper = factory.regressor_from_config(config)

    symtab_defs = get_symtab_and_defs_from_expr_set(
        expr_sets.get_expression_set(config["train_exprs"])
    )

    dataset = StateValueDataset.build_from_triples(
        (t, symtab_defs.make_toplevel(sparser.parse_expr(expr_str)), v)
        for t, expr_str, v in dataset_list
    )

    distillator = factory.distillator_from_config(config)
    # Note this seed-construction algorithm is different from the training curriculum,
    #  so we cannot replay distillation exactly the same.
    rng = utils.rng(factory.seed_from_config(config))
    with analytics.log_events_to_files(
        os.path.join(config["result_save_path"], "train_on_dataset_events/")
    ), analytics.Scope(generation=1, repetition=config["repetition"]):
        weights = distillator(
            model_wrapper=model_wrapper, seed=utils.seed(rng), dataset=dataset
        )
    weights.save(os.path.join(config["result_save_path"], "model_best.npz"))


def get_symtab_and_defs_from_expr_set(expr_set: expr_sets.ExpressionSet):
    # The format of expressions in (datasets in) the logs does not record the types of free variables
    # or prelude functions (specifically cost$<f>'s). Try to recover these from the original ExpressionSet.
    # Since we don't know from which starting expression each datapoint comes, we require that the
    # free variables in all starting exprs have matching types.

    def combine_pmaps(m, m2):
        # The 2*frozenset allows use of pyrsistent's PMap whose keys() are a PVector
        # (not a set - see https://github.com/tobgu/pyrsistent/issues/178).
        common_keys = frozenset(m.keys()) & frozenset(m2.keys())
        assert all(m[k] == m2[k] for k in common_keys)
        return m.update(m2)

    start_exprenvs = [e for _name, e in expr_set.named_exprenvs()]
    defs = reduce(combine_pmaps, [pmap(e.env.defs) for e in start_exprenvs])
    symtab = reduce(combine_pmaps, [pmap(e.env.symtab) for e in start_exprenvs])

    # Gather the types of all free variables in the expr, as these may not be in the symtab
    # (e.g. Expressions defined in best_results.py using infix / EF syntax).
    def get_free_var_types(e, fvars_needed: FrozenSet[str]) -> PMap[str, Type]:
        if len(fvars_needed) == 0:
            return pmap()
        if e.op == "variable" and e.name in fvars_needed:
            return pmap({e.name: e.type})
        return reduce(
            combine_pmaps,
            [
                get_free_var_types(
                    ch,
                    fvars_needed.difference([e.bound_var.name])
                    if e.is_binder and e.binds_in_child(i)
                    else fvars_needed,
                )
                for i, ch in enumerate(e.children)
            ],
            pmap(),
        )

    symtab = reduce(
        combine_pmaps,
        [get_free_var_types(e.expr, e.expr.free_var_names) for e in start_exprenvs],
        symtab,
    )
    return SymtabAndDefs(symtab=symtab, defs=defs)


def plot(config, dataset):
    if config["dist_plots"]:
        plot_initial_distribution_summary(config, dataset, config["result_save_path"])

    verbose_events = load_events_from_config(config, verbosity=1)
    plot_intra_gen_loss_from_config(
        config, verbose_events, generation=1,
    )
    plot_scatter_fitted_from_config(config, verbose_events)


def plot_initial_distribution_summary(config, dataset_list, output_path):

    if "costs" in config["dist_plots"]:
        costs = [sparser.parse_expr(e).cost() for (_, e, _) in dataset_list]
        plot_initial_distribution(
            costs, output_path, "costs", "train", config["cost_bins"],
        )
        print("Costs range between ", min(costs), " and ", max(costs))

    if "values" in config["dist_plots"]:
        values = [v for (_, _, v) in dataset_list]
        plot_initial_distribution(
            values, output_path, "values", "train", config["value_bins"],
        )
        print("Values range between ", min(values), " and ", max(values))

    if "nodes" in config["dist_plots"]:
        nodes = [sparser.parse_expr(e).num_nodes for (_, e, _) in dataset_list]
        plot_initial_distribution(
            nodes, output_path, "nodes", "train", config["node_bins"],
        )
        print("Number of nodes varies between ", min(nodes), " and ", max(nodes))


def plot_initial_distribution(
    inputs, output_path, field_name, test_train, bins=10, name_suffix=None,
):
    plt.hist([int(inp) for inp in inputs], bins=bins)
    plt.xlabel(field_name, fontsize=16)
    plt.ylabel("counts", fontsize=16)
    plt.title(f"{field_name} distribution for {test_train} expressions", fontsize=12)
    name_suffix = f"_{name_suffix}" if name_suffix else ""
    save_path = os.path.join(
        output_path, f"{test_train}_{field_name}_distribution{name_suffix}.png",
    )
    plt.savefig(save_path)
    plt.close()


def main():
    """
        Train a model on a dataset.

        To train a model with configurations specified by default scenario config (input scenario name) or
        arbitrary scenario config (input scenario config):
        $ python src/train_on_dataset.py scenario_name_or_config --dataset_path /path/to/dataset.json

        You can also override hyperparameters using flags available in train_over_expressions.py. For example,
        $ python src/train_on_dataset.py scenario_name_or_config --dataset_path /path/to/dataset.json --output_keep_prob 1.0

        To plot distributions of the dataset, use --dist_plots flag with the plotting choices preferred. Note every plotting has adjustable
        `bins` parameter flags: --cost_bins, --value_bins and --node_bins.
        $ python src/train_on_dataset.py ... --dist_plots costs values
    """

    train_on_dataset_arguments = [
        Args("--dataset_path", type=str, help="path to generated dataset"),
        Args(
            "--dist_plots",
            nargs="+",
            type=str,
            choices=["costs", "values", "nodes"],
            help="plotting initial distributions",
        ),
        Args(
            "--cost_bins",
            type=int,
            default=10,
            help="number of bins for the cost histogram plot",
        ),
        Args(
            "--value_bins",
            type=int,
            default=10,
            help="number of bins for the value histogram plot",
        ),
        Args(
            "--node_bins",
            type=int,
            default=10,
            help="number of bins for the node histogram plot",
        ),
        Args("--quiet", action="store_true", help="not verbose"),
    ]

    parser = make_parser(train_on_dataset_arguments + run_arguments)
    args, _ = parser.parse_known_args()
    config = make_config_for_scenario(
        args.scenario, run_args=run_arguments + train_on_dataset_arguments
    )

    with open(args.dataset_path) as f:
        dataset = json.load(f)
    assert "run_id" in dataset
    assert "gitlog" in dataset
    dataset = dataset["data_points"]

    config["verbose"] = not config.pop("quiet")
    config["simulation_depth_train"] = max(t for t, _e, _v in dataset)

    if config["num_repetitions"] != 1:
        print(
            "Warning: setting num_repetitions to 1. "
            "train_on_dataset only supports training one repetition."
        )
        config["num_repetitions"] = 1

    if config["repetition"] is None:
        config["repetition"] = 0
    check_save_config(config, train_exprs=None, eval_exprs=None)

    train(config, dataset)
    plot(config, dataset)


if __name__ == "__main__":
    main()
