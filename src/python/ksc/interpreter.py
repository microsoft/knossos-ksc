import numpy
from typing import Callable, Mapping

from ksc.expr import StructuredName, make_structured_name
from ksc.type import Type


# Largely a placeholder for an interpreter, which in the future may be capable of interpreting any Expr
# with no free variables. TODO: investigate combining with abstract_value.py and backends/ directory.


def _mksn(s: str, t: Type) -> StructuredName:
    return make_structured_name((s, t))


native_impls: Mapping[StructuredName, Callable] = {
    _mksn("add", Type.Tuple(Type.Integer, Type.Integer)): lambda a, b: a + b,
    _mksn("add", Type.Tuple(Type.Float, Type.Float)): lambda a, b: a + b,
    _mksn("sub", Type.Tuple(Type.Integer, Type.Integer)): lambda a, b: a - b,
    _mksn("sub", Type.Tuple(Type.Float, Type.Float)): lambda a, b: a - b,
    _mksn("mul", Type.Tuple(Type.Integer, Type.Integer)): lambda a, b: a * b,
    _mksn("mul", Type.Tuple(Type.Float, Type.Float)): lambda a, b: a * b,
    _mksn("div", Type.Tuple(Type.Integer, Type.Integer)): lambda a, b: a // b,
    _mksn("div", Type.Tuple(Type.Float, Type.Float)): lambda a, b: a / b,
    _mksn("exp", Type.Float): lambda a: numpy.exp(a),
    _mksn("log", Type.Float): lambda a: numpy.log(a),
    _mksn("eq", Type.Tuple(Type.Integer, Type.Integer)): lambda a, b: a == b,
    _mksn("eq", Type.Tuple(Type.Float, Type.Float)): lambda a, b: a == b,
    _mksn("gt", Type.Tuple(Type.Integer, Type.Integer)): lambda a, b: a > b,
    _mksn("gt", Type.Tuple(Type.Float, Type.Float)): lambda a, b: a > b,
    _mksn("gte", Type.Tuple(Type.Integer, Type.Integer)): lambda a, b: a >= b,
    _mksn("gte", Type.Tuple(Type.Float, Type.Float)): lambda a, b: a >= b,
    _mksn("to_float", Type.Integer): float,
}
