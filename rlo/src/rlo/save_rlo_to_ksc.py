# fmt: off
from typing import Iterable, Mapping, Any
import os
import shutil

from rlo.expression import Expression, EF
from rlo.expression_util import ExprWithEnv
from rlo import plotting
from rlo import utils
from rlo import sparser


def save_rlo_to_ksc(outdir, events: Iterable[Mapping[str, Any]], orig_exprs: Mapping[str, ExprWithEnv], template_path) -> None:

    if not os.path.isdir(outdir):
        os.mkdir(outdir)

    eval_event = [e for e in events if 'eval_expr' in e and e['event']=='rollout_end' and 'generation' in e] #not specifying (and "generation" in e) includes random generation
    by_expr_eval = utils.group_by(eval_event, lambda r: r["eval_expr"])

    for func_name, eval_logs in by_expr_eval.items():
        orig_expr_with_symtab = orig_exprs[func_name]
        eval_logs_by_rep = utils.group_by(eval_logs, lambda r: r["repetition"])
        for rep, rep_logs in eval_logs_by_rep.items(): # iterate over repetitions

            #we want expr with the smallest cost (in any generation)
            best_expressions = min([r for log in rep_logs for r in log['best_traj']], key = lambda s : s[1])
            expr, _, _ = best_expressions
            expr = sparser.parse_expr(expr)

            # Compute types using original symbol table.
            typed_expr = orig_expr_with_symtab.env.make_toplevel(expr).expr
            # And check type has not been changed by optimization
            expr_lam = utils.get_func_body(func_name, typed_expr)
            assert expr_lam.type == utils.get_func_body(func_name, orig_expr_with_symtab.expr).type

            # Finally output just the named function
            expr_name = Expression.Variable(func_name)
            expr_func = EF.Let(expr_name, expr_lam, expr_name)  # type: ignore[attr-defined]

            original_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), template_path)
            new_path = os.path.join(outdir, 'rep_' + str(rep) + '.kso')
            shutil.copy(original_path, new_path)
            with open(new_path, 'a') as f:
                f.write(expr_func.ksc_str() + '\n\n')

def save_rlo_to_ksc_from_config(config, events, orig_exprs):
    save_rlo_to_ksc(
        plotting.format_figure_filename(config, "rlo_to_ksc"),
        events,
        orig_exprs,
        config["template_path"]
    )
