

import pytest

from ksc.type import Type

from ksc.parse_ks import parse_structured_name, s_exps_from_string
from ksc.expr import StructuredName
from ksc.type import Type

def test_structured_names():
    sname = parse_structured_name(s_exps_from_string("[Dt [my_log Float]]")[0]) 
    assert sname.is_derivation()
    assert sname.is_derived("Dt")
    assert sname.has_type()
    _sn, ty = sname.add_type(Type.Float)
    assert ty == Type.Float

