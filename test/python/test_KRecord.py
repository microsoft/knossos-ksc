import pytest

from typing import List
from ksc.utils import KRecord

class MyClass(KRecord):
    cost: float
    names: List[str]

    def __init__(self, cost, names):
        super().__init__(cost=cost, names=names)

def test_KRecord_init():
    a = MyClass(1.3, ["fred", "conor", "una"])
    assert a.cost == 1.3
    assert len(a.names) == 3

def test_KRecord_equal():
    a = MyClass(1.3, ["fred", "conor", "una"])
    b = MyClass(1.3, ["fred", "conor"])
    b.names.append("una")
    assert a == b

# Check construction through baseclass
class MyABC(KRecord):
    a: int

class MyDerivedClass(MyABC):
    b: int

    def __init__(self, a, b):
        super().__init__(a=a,b=b)

def test_KRecord_abc_init():
    a = MyDerivedClass(17,43)
    assert a.a == 17
    assert a.b == 43

# Check that all attrs must be declared -- Can't make this work for derived chains as above

# class MyClass2(KRecord):
#     cost: float

#     def __init__(self, cost):
#         super().__init__(xcost=cost)

# def test_KRecord_unknown_attr_fail():

#     with pytest.raises(AssertionError) as excinfo:
#         a = MyClass2(17)
 