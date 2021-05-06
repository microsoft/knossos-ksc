import pytest

from ksc.type import Type
from ksc.expr import StructuredName, make_structured_name, Let, Lam, If, Call, Const, Var

from ksc.parse_ks import s_exps_from_string, parse_structured_name


def parse(s: str) -> StructuredName:
    ses = s_exps_from_string(s)
    assert len(ses) == 1
    return parse_structured_name(ses[0])


def test_StructuredName():
    sn = make_structured_name(("rev", ("f", Type.Integer)))
    assert sn.mangled() == "rev$f@i"

    assert parse("[rev [f Integer]]") == sn


def test_StructuredName_manglers():
    assert parse("[rev foo]").mangle_without_type() == "rev$foo"
    assert parse("[foo (Tuple Float Float)]").mangle_without_type() == "foo"
    assert parse("[rev [fwd [foo (Tuple Float Float)]]]").mangle_without_type() == "rev$fwd$foo"

    assert parse("[rev foo]").mangled() == "rev$foo"
    assert parse("[foo (Tuple Float Float)]").mangled() == "foo@<ff>"
    assert parse("[rev [fwd [foo (Tuple Float Float)]]]").mangled() == "rev$fwd$foo@<ff>"


def test_free_vars():
    assert Const(11.3).free_vars_ == frozenset()

    x = Var("x")
    assert x.free_vars_ == frozenset(["x"])

    # Binding
    y = Var("y")
    assert Let(x, If(y, Const(0), Const(1)), x).free_vars_ == frozenset(["y"])  # x is not free
    assert Lam(x, y).free_vars_ == frozenset(["y"])
    assert Lam(x, x).free_vars_ == frozenset()

    # Rebinding
    assert Let(x, If(y, x, Const(1)), x).free_vars_ == frozenset(["x", "y"])  # different x is free

    # Call targets not included
    assert Let(x, Call("add", [x, Const(1)]), Call("mul", [y, y])).free_vars_ == frozenset(["x", "y"])
