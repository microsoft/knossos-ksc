from functools import singledispatch
from typing import Union, Type
from ksc.expr import ConstantType, StructuredName, Expr, Const, Call

MatchFilterType = Union[Type, ConstantType, StructuredName]

@singledispatch
def match_filter(e : Expr) -> MatchFilterType:
    """ Checks whether two Expr's are equal ignoring their children (and names of Variables) """
    # Default.
    return e.__class__

@match_filter.register
def match_filter_const(e : Const) -> ConstantType:
    return e.value

@match_filter.register
def match_filter_call(e : Call):
    return e.name
