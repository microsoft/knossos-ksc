from functools import singledispatch
from typing import Union

from ksc.expr import ConstantType, StructuredName, Expr, Const, Call
from ksc.type import Type

# A term that describes the topmost node of an expression, excluding its "children"
FilterTerm = Union[Type, ConstantType, StructuredName]

@singledispatch
def get_filter_term(e : Expr) -> FilterTerm:
    # Allows to quick-reject rules that can never match a particular expr. See RuleMatcher.possible_expr_filter.
    return e.__class__

@get_filter_term.register
def get_filter_term_const(e : Const) -> ConstantType:
    return e.value

@get_filter_term.register
def get_filter_term_call(e : Call):
    return e.name
