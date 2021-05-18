import numpy
from typing import Callable, Mapping

from ksc.expr import StructuredName, make_structured_name
from ksc.type import Type


# Largely a placeholder for an interpreter, which in the future may be capable of interpreting any Expr
# with no free variables. TODO: investigate combining with abstract_value.py and backends/ directory.


native_impls: Mapping[StructuredName, Callable] = {
    make_structured_name(
        ("add", Type.Tuple(Type.Integer, Type.Integer))
    ): lambda a, b: a
    + b,
    make_structured_name(("add", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a
    + b,
    make_structured_name(
        ("sub", Type.Tuple(Type.Integer, Type.Integer))
    ): lambda a, b: a
    - b,
    make_structured_name(("sub", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a
    - b,
    make_structured_name(
        ("mul", Type.Tuple(Type.Integer, Type.Integer))
    ): lambda a, b: a
    * b,
    make_structured_name(("mul", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a
    * b,
    make_structured_name(
        ("div", Type.Tuple(Type.Integer, Type.Integer))
    ): lambda a, b: a
    // b,
    make_structured_name(("div", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a
    / b,
    make_structured_name(("exp", Type.Float)): lambda a: numpy.exp(a),
    make_structured_name(("log", Type.Float)): lambda a: numpy.log(a),
    make_structured_name(("eq", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a
    == b,
    make_structured_name(("eq", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a
    == b,
    make_structured_name(("gt", Type.Tuple(Type.Integer, Type.Integer))): lambda a, b: a
    > b,
    make_structured_name(("gt", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a
    > b,
    make_structured_name(
        ("gte", Type.Tuple(Type.Integer, Type.Integer))
    ): lambda a, b: a
    >= b,
    make_structured_name(("gte", Type.Tuple(Type.Float, Type.Float))): lambda a, b: a
    >= b,
    make_structured_name(("to_float", Type.Integer)): float,
}
