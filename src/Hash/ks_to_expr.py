import argparse
import os
import sys
from typing import List, NamedTuple, Union


# This file lives in src/Hash, but wants to peek into src/python, so add that to path manually.
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../python"))

from ksc.parse_ks import parse_ks_file
import ksc.expr as ksc


Term = Union["Var", "Lam", "App"]


class Var(NamedTuple):
    name: str


class Lam(NamedTuple):
    var_name: str
    body: Term


class App(NamedTuple):
    left: Term
    right: Term


def tuple_args(args: List[ksc.Expr]) -> Term:
    if len(args) == 0:
        return Var("emptyArgsList")

    terms = [convert_expr(arg) for arg in args]

    if len(args) == 1:
        return terms[0]

    full_term = Var("makeTuple")

    for term in terms:
        full_term = App(left=full_term, right=term)

    return full_term


def convert_call(name: str, args: List[ksc.Expr]) -> Term:
    return App(left=Var(name=name), right=tuple_args(args))


def convert_expr(expr: ksc.Expr) -> Term:
    if isinstance(expr, ksc.Const):
        return Var(name=f"const[{expr.value}]")
    elif isinstance(expr, ksc.Var):
        return Var(name=f"var[{expr.name}]")
    elif isinstance(expr, ksc.Call):
        return convert_call(name=f"func[{expr.name}]", args=expr.args)
    elif isinstance(expr, ksc.Lam):
        return Lam(var_name=convert_expr(expr.arg), body=convert_expr(expr.body))
    elif isinstance(expr, ksc.Let):
        return App(
            Lam(var_name=convert_expr(expr.var).name, body=convert_expr(expr.body)),
            right=convert_expr(expr.rhs),
        )
    elif isinstance(expr, ksc.If):
        return convert_call(name="if", args=[expr.cond, expr.t_body, expr.f_body])
    elif isinstance(expr, ksc.Assert):
        return convert_call(name="assert", args=[expr.cond, expr.body])
    else:
        assert False, f"Unknown expression type: {expr}"


def convert_expr_list(exprs: List[ksc.Expr]) -> Term:
    full_term = None

    for expr in exprs[::-1]:
        if not isinstance(expr, ksc.Def):
            continue

        print("Reading definition for", expr.name)
        term = convert_expr(expr.body)

        if full_term is None:
            full_term = term
        else:
            full_term = App(left=Lam(var_name=expr.name, body=full_term), right=term)

    return full_term


def term_to_string(term: Term) -> (str, int):
    if isinstance(term, Var):
        return f'Var () "{term.name}"', 1
    elif isinstance(term, Lam):
        body_str, body_size = term_to_string(term.body)
        return f'Lam () "{term.var_name}" ({body_str})', 1 + body_size
    elif isinstance(term, App):
        left_str, left_size = term_to_string(term.left)
        right_str, right_size = term_to_string(term.right)

        return f'App () ({left_str}) ({right_str})', 1 + left_size + right_size
    else:
        assert False, f"Unknown term type: {term}"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("KS_FILE", type=str)
    parser.add_argument("--output_dir", default="./exprs")

    args = parser.parse_args()
    in_path = args.KS_FILE

    with open(in_path) as f:
        content = f.read()
        exprs = list(parse_ks_file(content))

    term = convert_expr_list(exprs)

    out_path = os.path.join(args.output_dir, os.path.basename(in_path)[:-3]) + ".expr"

    term_str, num_nodes = term_to_string(term)
    print(f"Combined lambda term has {num_nodes} nodes")

    with open(out_path, "w") as f_out:
        f_out.write(term_str)


if __name__ == "__main__":
    main()
