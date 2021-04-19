from functools import singledispatch
from ksc.expr import StructuredName, Expr, Let, Call, Var, If, Assert, Const, Lam
from ksc.cav_subst import make_nonfree_var

@singledispatch
def untuple_lets(e: Expr) -> Expr:
    raise AssertionError("Must override for all Expr subclasses")

@untuple_lets.register
def untuple_lets_var(v : Var) -> Expr:
    return v

@untuple_lets.register
def untuple_lets_const(c : Const) -> Expr:
    return c

@untuple_lets.register
def untuple_lets_if(e : If) -> Expr:
    return If(untuple_lets(e.cond), untuple_lets(e.t_body), untuple_lets(e.f_body))

@untuple_lets.register
def untuple_lets_call(c : Call) -> Expr:
    return Call(c.name, [untuple_lets(a) for a in c.args])

@untuple_lets.register
def untuple_lets_lam(l : Lam) -> Expr:
    return Lam(l.arg, untuple_lets(l.body))

@untuple_lets.register
def untuple_lets_assert(a : Assert) -> Expr:
    return Assert(untuple_lets(l.cond), untuple_lets(l.body))

def is_literal_tuple(e: Expr):
    return isinstance(e, Call) and e.name.mangle_without_type() == "tuple"

def make_tuple_get(tuple_val: Expr, idx: int, size: int) -> Expr:
    assert 1 <= idx  <= size
    return Call(StructuredName(f"get${idx}${size}"), [tuple_val])

@untuple_lets.register
def untuple_let(l : Let) -> Expr:
    rhs = untuple_lets(l.rhs)
    body = untuple_lets(l.body)
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
            body = Let(var, make_tuple_get(temp_var, posn, len(l.vars)), body)
        return Let(temp_var, l.rhs, body)
