import numpy
from typing import Callable, Mapping

from ksc.expr import StructuredName, make_structured_name
from ksc.type import Type


# Largely a placeholder for an interpreter, which in the future may be capable of interpreting any Expr
# with no free variables. TODO: investigate combining with abstract_value.py and backends/ directory.


tuple_int_int = Type.Tuple(Type.Integer, Type.Integer)
tuple_float_float = Type.Tuple(Type.Float, Type.Float)

native_impls: Mapping[StructuredName, Callable] = {
    make_structured_name(("add", tuple_int_int)): lambda a, b: a + b,
    make_structured_name(("add", tuple_float_float)): lambda a, b: a + b,
    make_structured_name(("sub", tuple_int_int)): lambda a, b: a - b,
    make_structured_name(("sub", tuple_float_float)): lambda a, b: a - b,
    make_structured_name(("mul", tuple_int_int)): lambda a, b: a * b,
    make_structured_name(("mul", tuple_float_float)): lambda a, b: a * b,
    make_structured_name(("div", tuple_int_int)): lambda a, b: a // b,
    make_structured_name(("div", tuple_float_float)): lambda a, b: a / b,
    make_structured_name(("exp", Type.Float)): lambda a: numpy.exp(a),
    make_structured_name(("log", Type.Float)): lambda a: numpy.log(a),
    make_structured_name(("eq", tuple_int_int)): lambda a, b: a == b,
    make_structured_name(("eq", tuple_float_float)): lambda a, b: a == b,
    make_structured_name(("gt", tuple_int_int)): lambda a, b: a > b,
    make_structured_name(("gt", tuple_float_float)): lambda a, b: a > b,
    make_structured_name(("gte", tuple_int_int)): lambda a, b: a >= b,
    make_structured_name(("gte", tuple_float_float)): lambda a, b: a >= b,
    make_structured_name(("to_float", Type.Integer)): float,
}
