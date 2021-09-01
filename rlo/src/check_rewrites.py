import argparse
import re
from typing import Callable, Mapping, Sequence, TypeVar, Optional

from rlo.agent import Agent
from rlo.expression_util import SymtabAndDefs, NamedExprWithEnv
from rlo import experts
from rlo import expr_sets
from rlo import prettyprint
from rlo import rewrites
from rlo import sparser
from rlo.print_best_episodes import best_episodes_to_ks
from rlo.search_ops import episode_to_log
from ksc.type import Type
from rlo import utils

T = TypeVar("T")


def user_input_loop(choices_fn: Callable[[str], Mapping[str, T]]) -> Optional[T]:
    """ Gets the user to select an element using the console.
        choices_fn is a function that
           - takes the string last entered by the user (or "" if the user has not entered anything),
           - prints out any appropriate instructions, perhaps according to said last string entered
           - returns a Mapping from strings that the user may enter, to the value thereby selected.
        If the user enters a string in said mapping, user_input_loops returns the corresponding value.
        If the user enters a string *not* in said mapping, we feed that back into input_to_result.
    """
    last_input = ""
    while True:
        choices = choices_fn(last_input)
        try:
            last_input = input("?")
        except EOFError:
            return None
        if last_input in choices:
            return choices[last_input]


def choose_expr(named_exprs: Sequence[NamedExprWithEnv]):
    options = {str(i): n_ex for i, n_ex in enumerate(named_exprs)}
    options.update({n_ex.name: n_ex for n_ex in named_exprs})

    def choice_fn(str_from_prompt: str) -> Mapping[str, NamedExprWithEnv]:
        if str_from_prompt == "":
            for i, (n, _e) in enumerate(named_exprs):
                print(f"[{i}, {n}]")
        elif str_from_prompt.startswith("/"):
            for i, (n, e) in enumerate(named_exprs):
                if re.search(str_from_prompt[1:], n):
                    print(f"[{i}, {n}]:", prettyprint.pformat(e))
        else:
            print(f"Please enter integer 0-{len(named_exprs)-1} or name of expression")
        return options

    return choice_fn


def input_expr(str_from_prompt: str) -> Mapping[str, NamedExprWithEnv]:
    def parse():
        try:
            return sparser.parse_defs(str_from_prompt)[-1]
        except Exception as e:
            print(e)
        try:
            expr = sparser.parse_expr(str_from_prompt)
            env = SymtabAndDefs(
                # We have to give some type to allow the user to enter expressions with free variables!
                # Float seems as good as any.
                symtab={varname: Type.Float for varname in expr.free_var_names},
            )
            return NamedExprWithEnv("standalone_expression", env.make_toplevel(expr))
        except Exception as e:
            print(e)
        return None

    n_ex = parse()
    if n_ex is None:
        print("Enter expression or (def...)")
        return {}
    prettyprint.cpprint(n_ex.exprenv, width=80)
    print("<return> to accept or enter new Expression")
    return {"": n_ex}


class UserInputAgent(Agent):
    def __init__(self, rules, input_func):
        super().__init__(rules)
        self._input_func = input_func
        self._rule_name_filter = ""

    def choose_action(self, node):
        expr = node.exprenv.expr
        print("")
        print(
            f"====== Expression (cost {node.exprenv.cost()}; {self._steps} rules applied) ======"
        )

        # opt: a string that can be entered by the user, e.g. "0" or "inline_let@6"
        all_opts_by_rewrite = [
            (rewrite, [str(i), f"{rewrite.rule_name}@{rewrite.node_id}"])
            for i, rewrite in enumerate(node.actions)
        ]
        rewrite_dict = {opt: rw for rw, opts in all_opts_by_rewrite for opt in opts}
        # prompt: lists the opts for the same rewrite, e.g. "[0, inline_let@6]"
        prompt_by_rewrite = {
            rw: "[{}]".format(",".join(opts)) for rw, opts in all_opts_by_rewrite
        }
        expr_nodes = expr.nodes

        def print_expr_with_rules():
            prompts_by_node_id = {
                node_id: "".join(prompts)
                for node_id, prompts in utils.group_snds_by_fst(
                    [
                        (rw.node_id, prompt)
                        for rw, prompt in prompt_by_rewrite.items()
                        if re.search(self._rule_name_filter, rw.rule_name)
                    ]
                ).items()
            }
            prettyprint.cpprint(
                prettyprint.ExprWithComments(expr, prompts_by_node_id), width=80,
            )
            print(
                f'Showing rules ":{self._rule_name_filter}". /[<filter>] to list actions. 0-{len(node.actions)-1} or rule_name@node_id.'
            )

        def choices_fn(str_from_prompt: str):
            if str_from_prompt.startswith("/"):
                for rewrite, prompt in prompt_by_rewrite.items():
                    disp = f"{prompt} {expr_nodes[rewrite.node_id]}"
                    if re.search(str_from_prompt[1:], disp):
                        print(disp)
            elif str_from_prompt.startswith(":"):
                self._rule_name_filter = str_from_prompt[1:]
                print_expr_with_rules()
            elif str_from_prompt == "":
                print_expr_with_rules()
            else:
                print(
                    f"""Please enter integer 0-{len(node.actions)-1}, or rule_name@node_id, to perform action;
/ or /<filter> to list actions;
<enter> to print program with current rule_name_filter \"{self._rule_name_filter}\";
or :<rule_name_filter> to change."""
                )
            return rewrite_dict

        return self._input_func(choices_fn)


def get_episode_as_event(agent, expr_name, exprenv, truncate=False, **kwargs):
    episode = agent.optimize(exprenv, truncate=truncate, **kwargs)
    best_ep = episode_to_log(episode)
    assert best_ep.rewrite_seq_costs[0][1] == exprenv.cost()

    # Create a fake event to use print_best_episodes. Note that it is a slight
    # abuse because print_best_episodes assumes that the last element in best_ep
    # has the best cost (last element in best_ep).
    return dict(
        event="rollout_end",
        repetition=0,
        generation=0,
        eval_expr=expr_name,
        best_ep=best_ep,
        eval_exp_cost=exprenv.cost(),
    )


def check_rewrites(
    file,
    expert_name,
    rules_name,
    input_func,
    output_file,
    truncate=False,
    time_left=float("inf"),
):
    """ Check rewrites on a file or user-provided stand-alone expression manually or
    using an expert.

    Args:
        file: input file path (relative to src). If None, takes an expression
            from console input.
        expert_name: name of the expert. If None (default), uses UserInputAgent.
        rules_name: name of the rule set.
        input_func: a wrapper for Python's input for testing. See user_input_func.
        output_file: output file name.
        truncate: if True, returns a truncated sequence up to the minimum cost expression.
        time_left: time budget for the expert and for the best cost calculation.

    """
    rules = rewrites.get_rules(rules_name)
    if expert_name is None:
        agent = UserInputAgent(rules, input_func)
        source = "user"
    else:
        agent = experts.get_expert(expert_name, rules)
        source = "expert"

    expr_name, exprenv = input_func(
        input_expr
        if file is None
        else choose_expr(expr_sets.get_expression_set(file).named_exprenvs())
    )

    event = get_episode_as_event(
        agent, expr_name, exprenv, truncate=truncate, time_left=time_left
    )

    if file is None:
        output_content = best_episodes_to_ks(
            [event],
            generations=[-1],
            start_exprenvs={expr_name: exprenv},
            rules_name=rules_name,
            source=source,
        )
    else:
        expr_set = expr_sets.get_expression_set(file)
        best_cost_func = lambda expr, time_budget: expr_set.best_cost_for_expression(
            expr, rules_name, time_budget
        )
        output_content = best_episodes_to_ks(
            [event],
            generations=[-1],
            start_exprenvs=dict(expr_set.named_exprenvs()),
            best_cost_func=best_cost_func,
            ks_source_files=[file],
            rules_name=rules_name,
            source=source,
        )

    with open(output_file, "w") as f:
        f.write(output_content)
    print("\nSummary of rewrites written to {}".format(output_file))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--rules", required=True, choices=rewrites.available_rules())
    parser.add_argument(
        "--file",
        help="Path to scenario expression file, e.g. ./ksc/blas/blas_combined.kso",
    )
    parser.add_argument(
        "--expert",
        default=None,
        choices=experts.experts.keys(),
        help="Use an expert in experts.py (default: console input)",
    )
    parser.add_argument(
        "--time_left",
        default=float("inf"),
        type=int,
        help="Time budget limit for the expert",
    )
    parser.add_argument(
        "--truncate",
        action="store_true",
        help="if specified, returns a truncated sequence up to the minimum cost expression",
    )

    args = parser.parse_args()
    check_rewrites(
        args.file,
        args.expert,
        args.rules,
        user_input_loop,
        "check_rewrite_summary.kso",
        args.truncate,
        time_left=args.time_left,
    )


if __name__ == "__main__":
    main()
