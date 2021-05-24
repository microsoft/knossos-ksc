from operator import add, iadd, sub, mul, ifloordiv, truediv, eq, gt, ge
import numpy
from typing import Callable, Mapping

from ksc.expr import StructuredName, make_structured_name
from ksc.type import Type


# Largely a placeholder for an interpreter, which in the future may be capable of interpreting any Expr
# with no free variables. TODO: investigate combining with abstract_value.py and backends/ directory.

native_impls: Mapping[StructuredName, Callable] = {
    make_structured_name(("add", Type.Tuple(Type.Integer, Type.Integer))): iadd,
    make_structured_name(("add", Type.Tuple(Type.Float, Type.Float))): add,
    make_structured_name(("sub", Type.Tuple(Type.Integer, Type.Integer))): sub,
    make_structured_name(("sub", Type.Tuple(Type.Float, Type.Float))): sub,
    make_structured_name(("mul", Type.Tuple(Type.Integer, Type.Integer))): mul,
    make_structured_name(("mul", Type.Tuple(Type.Float, Type.Float))): mul,
    make_structured_name(("div", Type.Tuple(Type.Integer, Type.Integer))): ifloordiv,
    make_structured_name(("div", Type.Tuple(Type.Float, Type.Float))): truediv,
    make_structured_name(("exp", Type.Float)): numpy.exp,
    make_structured_name(("log", Type.Float)): numpy.log,
    make_structured_name(("eq", Type.Tuple(Type.Integer, Type.Integer))): eq,
    make_structured_name(("eq", Type.Tuple(Type.Float, Type.Float))): eq,
    make_structured_name(("gt", Type.Tuple(Type.Integer, Type.Integer))): gt,
    make_structured_name(("gt", Type.Tuple(Type.Float, Type.Float))): gt,
    make_structured_name(("gte", Type.Tuple(Type.Integer, Type.Integer))): gt,
    make_structured_name(("gte", Type.Tuple(Type.Float, Type.Float))): ge,
    make_structured_name(("to_float", Type.Integer)): float,
}
