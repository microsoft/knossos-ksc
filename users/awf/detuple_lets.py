from ksc.type import Type

from ksc.expr import Expr, Def, EDef, Rule, Const, Var, Lam, Call, Let, If, Assert
from ksc.expr import pystr

# Pretty printing
# Importing prettyprint to get the decorated printers for Expression and Type
import ksc.prettyprint  # pylint: disable=unused-import

# Import the prettyprinter routines we use explicitly in this file
from prettyprinter import cpprint, pprint, pformat

var_number = 1000


def detuple_lets(expr: Expr) -> Expr:
    if isinstance(expr, Def):
        return Def(expr.name, expr.type, expr.args, detuple_lets(expr.body))

    if isinstance(expr, EDef):
        return expr

    if isinstance(expr, Let):
        if isinstance(expr.vars, Var):
            return Let(expr.vars, detuple_lets(expr.rhs), detuple_lets(expr.body))
        global var_number
        var_number += 1
        tvar = Var(f"detuple_lets${var_number}")
        body = detuple_lets(expr.body)
        n = len(expr.vars)
        for k, var in enumerate(expr.vars):
            body = Let(var, Call(f"get${k+1}${n}", [tvar]), body)
        out = Let(tvar, detuple_lets(expr.rhs), body)
        return out

    if isinstance(expr, Call):
        return Call(name=expr.name, args=[detuple_lets(arg) for arg in expr.args])

    if isinstance(expr, Lam):
        return Lam(arg=expr.arg, body=detuple_lets(expr.body))

    if isinstance(expr, If):
        return If(
            cond=detuple_lets(expr.cond),
            t_body=detuple_lets(expr.t_body),
            f_body=detuple_lets(expr.f_body),
        )

    if isinstance(expr, Assert):
        return Assert(cond=detuple_lets(expr.cond), body=detuple_lets(expr.body))

    if isinstance(expr, (Var, Const)):
        return expr

    assert False, f"Unknown expression type: {expr}"


if __name__ == "__main__":
    from ksc.parse_ks import parse_ks_string

    exprs = parse_ks_string(
        """
    (def f Float (a : Float)
        (let ((b c) (tuple a a))
          (let ((d e) (tuple 2 3))
            d)))
    """
    )
    expr = detuple_lets(list(exprs)[0])
    cpprint(expr)
