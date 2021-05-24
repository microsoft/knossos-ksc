from ksc.visitors import ExprTransformer
from ksc.expr import StructuredName, Expr, Let, Call, Var
from ksc.cav_subst import make_nonfree_var
from ksc.utils import singleton


def is_literal_tuple(e: Expr):
    return isinstance(e, Call) and e.name.mangle_without_type() == "tuple"


def make_tuple_get(idx: int, size: int, tuple_val: Expr) -> Expr:
    assert 1 <= idx <= size
    return Call(StructuredName(f"get${idx}${size}"), [tuple_val])


@singleton
class _UntupleLets(ExprTransformer):
    def visit_let(self, l: Let) -> Expr:
        return untuple_one_let(Let(l.vars, self.visit(l.rhs), self.visit(l.body)))


def untuple_one_let(l: Let) -> Expr:
    if isinstance(l.vars, Var):
        return l
    # So, tupled let, binding multiple variables. Convert to nested single lets.
    body = l.body

    # Optional special case, otherwise we could inline the tuple (uphill) and then use
    # the select-of-tuple rules, once for each element, then delete the outermost let.
    if is_literal_tuple(l.rhs):
        assert len(l.rhs.args) == len(l.vars)
        for arg, val in reversed(list(zip(l.vars, l.rhs.args))):
            body = Let(arg, val, body)
        return body
    else:
        # A tuple of variables, assigned to a single value - e.g. (let ((x y z) val) ...).
        # If the val is not a variable, we should compute it once only into a fresh name.
        # Then each variable to a get$n$m of that.
        if isinstance(l.rhs, Var):
            temp_var = l.rhs
        else:
            temp_var = make_nonfree_var("temp", [l.rhs, body])
        for posn, var in reversed(list(enumerate(l.vars, 1))):
            body = Let(var, make_tuple_get(posn, len(l.vars), temp_var), body)
        return body if isinstance(l.rhs, Var) else Let(temp_var, l.rhs, body)


def untuple_lets(e: Expr):
    """ Converts any 'Let's within e that bind tuples, into chains of 'Let's each binding only a single Var. """
    return _UntupleLets.visit(e)
