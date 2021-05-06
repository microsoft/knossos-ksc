import numpy
from typing import Callable, Mapping

from ksc.expr import StructuredName
from ksc.type import Type

def _mksn(s: str, t: Type) -> StructuredName:
    sn = StructuredName.from_str(s)
    sn2, old_ty = sn.add_type(t)
    assert old_ty == None
    return sn2

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
