import numpy
from typing import Callable, Mapping
from rlo.sparser import default_symtab


def check_types(*pairs):
    for val, typ in pairs:
        assert isinstance(val, typ)
    return True


native_impls: Mapping[str, Callable] = {
    "add$ii": lambda a, b: check_types((a, int), (b, int)) and a + b,
    "add$ff": lambda a, b: check_types((a, float), (b, float)) and a + b,
    "sub$ii": lambda a, b: check_types((a, int), (b, int)) and a - b,
    "sub$ff": lambda a, b: check_types((a, float), (b, float)) and a - b,
    "mul$ii": lambda a, b: check_types((a, int), (b, int)) and a * b,
    "mul$ff": lambda a, b: check_types((a, float), (b, float)) and a * b,
    "div$ii": lambda a, b: check_types((a, int), (b, int)) and a // b,
    "div$ff": lambda a, b: check_types((a, float), (b, float)) and a / b,
    "exp$f": lambda a: check_types((a, float)) and numpy.exp(a),
    "log$f": lambda a: check_types((a, float)) and numpy.log(a),
    "eq$ii": lambda a, b: check_types((a, int), (b, int)) and a == b,
    "eq$ff": lambda a, b: check_types((a, float), (b, float)) and a == b,
    "gt$ii": lambda a, b: check_types((a, int), (b, int)) and a > b,
    "gt$ff": lambda a, b: check_types((a, float), (b, float)) and a > b,
    "gte$ii": lambda a, b: check_types((a, int), (b, int)) and a >= b,
    "gte$ff": lambda a, b: check_types((a, float), (b, float)) and a >= b,
    "to_float": lambda a: check_types((a, int)) and float(a),
}

assert frozenset(default_symtab.keys()).issuperset(native_impls.keys())
