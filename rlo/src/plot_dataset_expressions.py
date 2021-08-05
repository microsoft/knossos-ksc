import argparse
import json
from collections import defaultdict
import matplotlib.pyplot as plt
from textwrap import wrap
from tqdm import tqdm
import os

from rlo.utils import single_elem
from rlo.experts import get_expert, ExpertWrapper
from rlo.summations import SummationsExpert
from rlo.rewrites import get_rules
from rlo.factory import num_time_heads_from_config
from rlo.sparser import parse_expr
from rlo.expression_util import SymtabAndDefs
from train_on_dataset import get_symtab_and_defs_from_expr_set
from rlo.expr_sets import get_expression_set


def plot_exprs(exprs_dict, save_path, expert, parse_func):
    for i, (expr, data) in enumerate(tqdm(exprs_dict.items(), desc="Progress")):
        times = [tl for (tl, _, _) in data]
        fitted = [f for (_, _, f) in data]
        expert_targs = single_elem(
            expert.evaluate_all_time_left([parse_func(expr)]).tolist()
        )
        expert_targs = expert_targs[: len(fitted)]
        model_targs = [t for (_, t, _) in data]

        _fig = plt.figure(figsize=[30, 15])
        plt.plot(times, fitted, color="red", label="fitted")
        plt.plot(times, expert_targs, color="black", label="expert targs")
        plt.scatter(times, model_targs, color="green", label="empirical targs", s=30)

        plt.xlabel("time left", fontsize=15)
        plt.ylabel("values", fontsize=15)
        plt.title("\n".join(wrap(expr, width=230)), fontsize=15)
        plt.legend()
        plt.savefig(os.path.join(save_path, f"{len(times)}_budget_{i}_expr"))
        plt.close()


def main():
    parser = argparse.ArgumentParser(
        """
            Creates fitted vs target (can be either dataset target or expert target) 
            plots for each expression in the dataset. To extract the dataset use src/extract_dataset.py
            script.

            Plots will be saved in the same directory as the dataset.

        """
    )
    parser.add_argument("--path_to_dataset", type=str, help="path to dataset json file")
    parser.add_argument(
        "--path_to_dataset_config", type=str, help="path to dataset config json file",
    )
    parser.add_argument(
        "--output_folder",
        type=str,
        default="dataset_expr_plots",
        help="name of the folder where to save the plots",
    )
    args = parser.parse_args()

    # get dataset
    with open(args.path_to_dataset) as f:
        dataset_file = json.load(f)
    data_points = dataset_file["data_points"]

    with open(args.path_to_dataset_config) as f:
        dataset_config_file = json.load(f)

    # get expert
    scenario = dataset_config_file["scenario"]
    rules = get_rules(dataset_config_file["rules"])
    train_exprs = dataset_config_file["train_exprs"]
    num_time_heads = num_time_heads_from_config(dataset_config_file)

    if scenario.startswith("summations"):
        expert = SummationsExpert(num_time_heads)
        symtab_defs = get_symtab_and_defs_from_expr_set(
            get_expression_set(train_exprs).expressions
        )
    else:
        expert = ExpertWrapper(get_expert(scenario, rules), num_time_heads)
        symtab_defs = SymtabAndDefs()

    parse_func = lambda e: symtab_defs.make_toplevel(parse_expr(e))
    dir_path = os.path.dirname(args.path_to_dataset)
    save_path = os.path.join(dir_path, args.output_folder)
    if not os.path.isdir(save_path):
        os.mkdir(save_path)

    exprs_dict = defaultdict(list)
    for ind, (tl, expr, targ, fitted) in enumerate(data_points):
        exprs_dict[expr].append((tl, targ, fitted))

    print(f"Dataset contains {len(exprs_dict)} unique expressions and {ind} points.")
    plot_exprs(exprs_dict, save_path, expert, parse_func)


if __name__ == "__main__":
    main()
