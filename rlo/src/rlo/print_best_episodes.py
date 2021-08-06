import argparse
import os

import pandas as pd
from collections import defaultdict
import json
import subprocess
from typing import Any, Callable, Dict, List, Optional, Tuple


from rlo.best_results import TypeBestCost
from rlo.expression_util import ExprWithEnv
from rlo import experiment_result
from rlo import factory
from rlo.rewrites import rewrite_seq_to_exprenvs
from rlo.expr_sets import get_expression_set, TimeBudget
from rlo.factory import get_train_and_eval_exprs
from rlo import plotting
from rlo.reporting import compute_optimality
from rlo import sparser
from rlo import utils


def convert_best_episode(best_ep, start_exprenv: ExprWithEnv):
    if (
        len(best_ep) == 2
        and isinstance(best_ep[0], str)
        and isinstance(best_ep[1], list)
    ):
        # new format: starting expr and sequence of actions
        logs_expr, rewrite_seq_costs = best_ep
        logs_expr = sparser.parse_expr(logs_expr)
        assert start_exprenv.expr == logs_expr
        actions, costs = zip(*rewrite_seq_costs)

        # This reflects the current log format in (best_)episode_to_log. It may not be the case for legacy logs,
        # which record whatever action came after after the lowest-cost expression was reached.
        assert tuple(actions[-1]) == (None, None)

        exprenvs = rewrite_seq_to_exprenvs(
            start_exprenv, actions[:-1]
        )  # last action is (None, None)
        assert len(actions) == len(exprenvs)
        return list(zip([str(exprenv.expr) for exprenv in exprenvs], actions, costs))
    # old format: all exprs and costs already present, so no need to parse, or call cost(), or for symtab.
    return best_ep


def convert_event(r, start_exprenvs):
    assert r["event"] == "rollout_end"
    return {
        **r,
        "best_ep": convert_best_episode(r["best_ep"], start_exprenvs[r["eval_expr"]]),
    }


def best_ep_key(event):
    # sort key - take the least cost, shortest, earliest
    return (event["best_ep"][-1][2], len(event["best_ep"]), event["generation"])


def best_ep_actions(event):
    # last action is (None, None)
    return [action for (_expr, action, _cost) in event["best_ep"]][:-1]


def read_ks_file(file_path: str) -> Dict[Tuple[str, str], List[str]]:
    """ Function to process all lines, which start with ; BEST
        in a scenario .kso file. Returns a dictionary that maps
        (rules_name, func_name) to a corresponding line. """

    return utils.group_snds_by_fst(
        [
            (
                (best_ep_entries.rules_name, best_ep_entries.func_name),
                best_ep_entries.raw_line,
            )
            for best_ep_entries in utils.parse_best_episodes(file_path)
        ]
    )


def make_summary(
    events: List,
    generations: List,
    best_cost_func: Optional[Callable[[str, TimeBudget], TypeBestCost]],
    start_exprenvs: Dict[str, ExprWithEnv],
):
    """ Compute summary statistics for each eval_expr,
        for the best generation+repetition, and each generation in generations for the same repetition.
        generations may contain -1 to indicate the last generation (in the same repetition).

        events: list containing experiment events
        generation: list of generations for which to produce a summary
        best_cost_func: function that takes in eval_expr and time, and returns a tuple (best_cost, source, sequence)
    """
    eval_events = [
        convert_event(e, start_exprenvs)
        for e in events
        if "eval_expr" in e and e["event"] == "rollout_end" and "generation" in e
    ]
    by_expr = utils.group_by(eval_events, lambda r: r["eval_expr"])

    # Sort results lexicographically by expression.
    results_by_expr_sorted = sorted(by_expr.items(), key=lambda item: item[0])

    summary = []
    for eval_expr, events in results_by_expr_sorted:
        best_in_all_generations = min(events, key=best_ep_key)
        rep_events = [
            e
            for e in events
            if e["repetition"] == best_in_all_generations["repetition"]
        ]
        last_generation = max([int(e["generation"]) for e in rep_events])
        rep_generations = [(last_generation if ep == -1 else ep) for ep in generations]
        by_generation = utils.group_by(
            [e for e in rep_events if e["generation"] in rep_generations],
            lambda e: e["generation"],
        )
        assert (
            len(by_generation) > 0
        ), "No events for generations {} found for {}".format(generations, eval_expr)
        by_generation.pop(
            best_in_all_generations["generation"], None
        )  # Avoid outputting same row twice if best generation was the last
        summary_events = [best_in_all_generations] + [
            min(events, key=best_ep_key) for _, events in sorted(by_generation.items())
        ]
        for best_event in summary_events:
            best_expr, _, best_cost = best_event["best_ep"][-1]
            best_expr = sparser.parse_expr(best_expr)
            original_cost = best_event["eval_exp_cost"]

            target_cost, target_source, target_seq = (
                (None, None, None)
                if best_cost_func is None
                else best_cost_func(eval_expr, float("inf"))
            )
            summary.append(
                {
                    "eval_expr": eval_expr,
                    "generation": best_event["generation"],
                    "repetition": best_event["repetition"],
                    "original_cost": original_cost,
                    "highest_hill": max(
                        [cost for _expr, _action, cost in best_event["best_ep"]]
                    ),
                    "best_cost": best_cost,
                    "target_cost": target_cost,
                    "target_reduction": compute_optimality(
                        original_cost, 0.0, target_cost
                    ),
                    "target_source": target_source,
                    "target_ep_length": None
                    if target_seq is None
                    else len(target_seq) + 1,
                    "optimality": compute_optimality(
                        original_cost, best_cost, target_cost
                    ),
                    "best_episode_length": len(best_event["best_ep"]),
                    "trivial_lets": len(
                        [
                            n
                            for n in best_expr.nodes
                            if n.op == "let" and n.second.op == "var"
                        ]
                    ),
                    "best_ep": best_event["best_ep"],
                }
            )
    return pd.DataFrame(
        summary,
        columns=[
            "eval_expr",
            "generation",
            "repetition",
            "original_cost",
            "highest_hill",
            "best_cost",
            "target_cost",
            "target_reduction",
            "target_source",
            "target_ep_length",
            "optimality",
            "best_episode_length",
            "trivial_lets",
            "best_ep",
        ],
    )


def best_episodes_to_ks(
    events: List,
    generations: List,
    source: str,
    start_exprenvs: Dict[str, ExprWithEnv],
    best_cost_func: Optional[Callable[[str, TimeBudget], TypeBestCost]] = None,
    ks_source_files: List[str] = [],
    run_id: str = None,
    rules_name: str = None,
    exprs_to_print: List = None,
):

    """ events:           list containing experiment events
        generations:      list of generations used to produce a summary
        start_exprenvs:    Dictionary from name (key used in logs) to starting ExprWithEnv. Usually, dict(expr_set.named_exprenvs()).
        best_cost_func:   function that takes in eval_expr and time, and returns a tuple (best_cost, source, sequence)
        ks_source_files:  str, list of ks source files used for training / test, only for ".kso" scenarios
        run_id:           str id of the experiment
        rules_name:       str name of the rule set
        exprs_to_print:   list of the expressions to output, or None for all of them
        source:           str, indicates whether the best_cost came from expert/rlo/user
    """

    summary = make_summary(events, generations, best_cost_func, start_exprenvs)
    summary_str = "; " + "\n; ".join(
        summary.to_string(
            columns=[col for col in summary.columns if col != "best_ep"],
            index=False,
            float_format="%.1f",
        ).split("\n")
    )
    ks_out = [summary_str]

    def output_ks_info(row: Dict[str, Any]) -> str:
        """ Function that converts dataframe row into a string with the format: 
            ; BEST rule_names func_name source experiment_id ; rewrite sequence """
        eval_expr = row["eval_expr"]
        run_id_or_commit = (
            run_id
            if run_id is not None
            else subprocess.check_output(["git", "describe", "--always"])
            .strip()
            .decode()
            if source == "expert"
            else ""
        )  # Expected if source == "user"
        rewrite_seq = json.dumps(best_ep_actions(row))
        return f"; BEST {rules_name} {eval_expr} {source} {run_id_or_commit} ; {rewrite_seq}"

    def new_best_cost_file(path_exprs: str) -> List[str]:
        """ Function that checks if the experiment has achieved new best_costs. If the new best_costs are found, 
            function returns a list containing information on the expressions that achieved new best_costs."""
        # dictionaries contain best_cost recorded in .kso files for some (or all) train and test expressions.
        file_dict = read_ks_file(
            os.path.join(os.path.dirname(os.path.abspath(__file__)), path_exprs)
        )

        exprs_in_this_set = [
            name for (name, _) in get_expression_set(path_exprs).named_exprenvs()
        ]

        def find_updates():
            for _, row in summary.iterrows():
                expr = row["eval_expr"]
                expr_rewrite_costs = [cost for (_expr, _action, cost) in row["best_ep"]]

                def cost_improved(time, new_rewrite_cost):
                    return (
                        best_cost_func is None
                        or new_rewrite_cost < best_cost_func(expr, time).cost
                    )

                we_care_about_this_expr = expr in exprs_in_this_set
                cost_improved_at_some_time = any(
                    cost_improved(time, new_rewrite_cost)
                    for time, new_rewrite_cost in enumerate(expr_rewrite_costs[1:], 1)
                )

                if we_care_about_this_expr and cost_improved_at_some_time:
                    yield ((rules_name, expr), output_ks_info(row))

        updates = list(find_updates())
        if len(updates) == 0:
            return []

        for k, vs in utils.group_snds_by_fst(updates).items():
            # Remove duplicates (same sequence found in multiple generations or repetitions).
            # (TODO: would be better to do this as we went along in find_updates(): that would mean we would output
            # only the best among *multiple* new sequences; this way means we will output all new sequences that
            # are better than the existing ones, even if some new sequences are strictly worse than other new sequences.)
            vs_set = frozenset(vs)

            # None of the sequences can have been present before, because they improve on the existing best costs
            assert (k not in file_dict) or vs_set.isdisjoint(file_dict[k])
            file_dict.setdefault(k, []).extend(vs_set)

        return [
            f"\nPlease overwrite the information on best costs in the scenario file ({path_exprs}) with the details below:\n"
        ] + [v for vs in file_dict.values() for v in vs]

    # check for improvements against the best known costs
    if len(ks_source_files) != 0:
        best_cost_updates = []
        for ks_file in ks_source_files:
            best_cost_updates += new_best_cost_file(ks_file)
        ks_out += best_cost_updates

        if source is None and len(best_cost_updates) > 0:
            ks_out.append(
                "\n; New best costs were achieved by RLO! Please add experiment config to ksc/configs/ folder and rename accordingly!\n"
            )
        else:
            ks_out.append("\n; NO new best costs were found.\n")

    else:
        # df_improved is obtained only from those entries where we know target_cost (i.e. target_cost is not None)
        summary_not_none = summary[summary["target_cost"].notnull()]
        df_improved = summary_not_none[
            summary_not_none["best_cost"] < summary_not_none["target_cost"]
        ]

        if len(df_improved) > 0:
            ks_out.extend(
                [";", "; following expressions had better than previously known costs:"]
            )
            for _, row in df_improved.iterrows():
                ks_out.append(output_ks_info(row))
            ks_out.extend(
                [
                    ";",
                    "; Please check the rewrite sequences and make a PR to the relevant input file!",
                    "; If the new best cost was achieved by RLO, also add a corresponding config file to the PR!",
                ]
            )

    return rewrite_sequence_summary(exprs_to_print, summary, ks_out)


def rewrite_sequence_summary(exprs_to_print, summary, ks_out):
    if exprs_to_print is None:
        # all expressions
        exprs_to_print = summary["eval_expr"]

    for eval_expr in exprs_to_print:
        df_for_expr = summary[summary["eval_expr"] == eval_expr]
        for generation in df_for_expr["generation"]:
            df = df_for_expr[df_for_expr["generation"] == generation]
            assert len(df) == 1
            best_event = df.iloc[0]
            ks_out.append(
                ";--------------------------------------------------------------------------------------"
            )
            ks_out.append(
                "; {} generation={} (repetition={})".format(
                    eval_expr, generation, best_event.repetition
                )
            )
            ks_out.append(
                ";--------------------------------------------------------------------------------------"
            )
            rewrite_rule_counts = defaultdict(int)
            for i, (expr, action, cost) in enumerate(best_event.best_ep):
                best_cost = best_event.target_cost
                if best_cost is not None:
                    if best_event.original_cost != best_cost:
                        optimality = ", opt={:.2f}%".format(
                            100
                            * (best_event.original_cost - cost)
                            / (best_event.original_cost - best_cost)
                        )
                    else:
                        optimality = ", opt={:.2f}%".format(100)
                else:
                    optimality = ""
                ks_out.append(
                    "; step {} (cost={:.1f}{});"
                    " next action={}".format(i, cost, optimality, tuple(action))
                )
                ks_out.append(expr)
                _, rule = action
                if rule is not None:
                    rewrite_rule_counts[rule] += 1
            ks_out.append(
                "\n; rewrite rule counts: {}\n".format(
                    sorted(rewrite_rule_counts.items())
                )
            )
    return "\n".join(ks_out)


def _best_episodes_from_config(config, events, generations, exprs_to_print=None):
    _, eval_expr_set = get_train_and_eval_exprs(config)
    ks_source_files = [
        ks_source_file
        for ks_source_file in [config["train_exprs"], config["test_exprs"]]
        if ks_source_file.endswith(".kso")
    ]
    return best_episodes_to_ks(
        events,
        generations,
        start_exprenvs=dict(eval_expr_set.named_exprenvs()),
        best_cost_func=factory.best_cost_from_config(config),
        ks_source_files=ks_source_files,
        run_id=config["run_id"],
        rules_name=config["rules"],
        exprs_to_print=exprs_to_print,
        source="rlo",
    )


def output_best_episodes_from_config(config, events, generations=[-1]):
    # generations = [-1] takes the last generation
    output_file = plotting.format_figure_filename(config, "best_episodes.kso")
    ks_out = _best_episodes_from_config(config, events, generations)
    with open(output_file, "w") as f:
        f.write(ks_out)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="(1) a run ID (e.g., 2019_01_06_13_15_48_13172), (2) path to a config.json file, or (3) path to a directory containing /rep/events.json files",
    )
    parser.add_argument(
        "--generations",
        type=int,
        nargs="+",
        default=[-1],
        help="Generation number to output (as well as best over all generations)",
    )
    # The default=None here means that if run without --exprs, we'll pass exprs=None, meaning all expressions.
    # python ...print_best_episodes.py config --exprs A B means we'll get a list of all A/B following (may be an empty list)
    parser.add_argument(
        "--exprs",
        type=str,
        nargs="*",
        default=None,
        help="Expressions to output (default: all; --exprs <EOL> for just summary)",
    )
    args = parser.parse_args()
    config, logs = experiment_result.load_config_events(args.run_id)
    print(
        _best_episodes_from_config(
            config, logs, args.generations, exprs_to_print=args.exprs
        )
    )


if __name__ == "__main__":
    main()
