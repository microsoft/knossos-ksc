from ksc.expr_transformer import ExprTransformer
from ksc.expr import StructuredName, Expr, Let, Call, Var
from ksc.cav_subst import make_nonfree_var

def is_literal_tuple(e: Expr):
    return isinstance(e, Call) and e.name.mangle_without_type() == "tuple"

def make_tuple_get(idx: int, size: int, tuple_val: Expr) -> Expr:
    assert 1 <= idx  <= size
    return Call(StructuredName(f"get${idx}${size}"), [tuple_val])

class _UntupleLets(ExprTransformer):
    def __init__(self):
        self.transform_let = self.transform.register(self.transform_let)

    # One cannot write `@transform.register` here - transform is not recognized.
    # One can write `@ExprTransformer.transform.register` - but that overrides the superclass case (for all ExprTransformers)!
    def transform_let(self, l : Let) -> Expr:
        rhs = self.transform(l.rhs)
        body = self.transform(l.body)
        if isinstance(l.vars, Var):
            return Let(l.vars, rhs, body)
        # Tupled let, binding multiple variables. Convert to nested single lets.

        # Optional special case, otherwise we could inline the tuple (uphill) and then use
        # the select-of-tuple rules, once for each element, then delete the outermost let.
        if is_literal_tuple(l.rhs):
            assert len(l.rhs.args) == len(l.vars)
            for arg, val in reversed(list(zip(l.vars, l.rhs.args))):
                body = Let(arg, val, body)
            return body
        else:
            # A tuple of variables, assigned to a single value - e.g. (let ((x y z) val) ...).
            # In this case we must assign the value to a fresh name, then each variable to a get$n$m of that.
            temp_var = make_nonfree_var("temp", [rhs, body])
            for posn, var in reversed(list(enumerate(l.vars, 1))):
                body = Let(var, make_tuple_get(posn, len(l.vars), temp_var), body)
            return Let(temp_var, l.rhs, body)

untuple_lets = _UntupleLets().transform
