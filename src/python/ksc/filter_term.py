from functools import singledispatch
from typing import Union

from ksc.expr import ConstantType, StructuredName, Expr, Const, Call, Var
from ksc.type import Type

# Note: filter_term
# A term that allows a quick-rejection test of whether an expression matches a template or another expression.
# That is: get_filter_term computes a FilterTerm from an Expr in time O(1) in the size of the Expr, such that
# if get_filter_term(expr1) == get_filter_term(expr2) ---> they might match
#    get_filter_term(expr1) != get_filter_term(expr2) ---> they definitely don't match
# Moreover, the design aims to optimize the frequency of detecting non-matches.
FilterTerm = Union[Type, ConstantType, StructuredName]

@singledispatch
def get_filter_term(e : Expr) -> FilterTerm:
    return e.__class__

@get_filter_term.register
def get_filter_term_const(e : Const) -> ConstantType:
    return e.value

@get_filter_term.register
def get_filter_term_call(e : Call):
    return e.name
