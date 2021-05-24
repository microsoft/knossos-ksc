import numpy
from typing import Callable, Mapping

from ksc.expr import StructuredName, make_structured_name
from ksc.type import Type


# Largely a placeholder for an interpreter, which in the future may be capable of interpreting any Expr
# with no free variables. TODO: investigate combining with abstract_value.py and backends/ directory.

mksn = make_structured_name

native_impls: Mapping[StructuredName, Callable] = {
    mksn(("add", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a + b,
    mksn(("add", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a + b,
    mksn(("sub", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a - b,
    mksn(("sub", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a - b,
    mksn(("mul", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a * b,
    mksn(("mul", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a * b,
    mksn(("div", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a // b,
    mksn(("div", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a / b,
    mksn(("exp", Type.Float)): lambda a: numpy.exp(a),
    mksn(("log", Type.Float)): lambda a: numpy.log(a),
    mksn(("eq", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a == b,
    mksn(("eq", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a == b,
    mksn(("gt", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a > b,
    mksn(("gt", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a > b,
    mksn(("gte", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a >= b,
    mksn(("gte", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a >= b,
    mksn(("to_float", Type.Integer)): float,
}
