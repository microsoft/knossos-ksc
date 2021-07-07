import pytest

from ksc.type import Type
from ksc.expr import (
    StructuredName,
    make_structured_name,
    Let,
    Lam,
    If,
    Call,
    Const,
    Var,
    Def,
)

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
    assert (
        parse("[rev [fwd [foo (Tuple Float Float)]]]").mangle_without_type()
        == "rev$fwd$foo"
    )

    assert parse("[rev foo]").mangled() == "rev$foo"
    assert parse("[foo (Tuple Float Float)]").mangled() == "foo@ff"
    assert parse("[foo (Tuple (Tuple Float Float) Float)]").mangled() == "foo@<ff>f"
    assert parse("[rev [fwd [foo (Tuple Float Float)]]]").mangled() == "rev$fwd$foo@ff"


def test_free_vars():
    assert Const(11.3).free_vars_ == frozenset()

    p = Var("p", Type.Bool)
    x = Var("x", Type.Float)
    y = Var("y", Type.Float)
    assert x.free_vars_ == frozenset(["x"])

    # Binding
    assert Let(x, If(p, Const(0.0), Const(1.1)), x).free_vars_ == frozenset(
        ["p"]
    )  # x is not free

    # Rebinding
    assert Let(x, If(p, x, Const(1.1)), x).free_vars_ == frozenset(
        ["x", "p"]
    )  # different x is free

    # Call targets not included
    assert Let(
        x, Call("add", [x, Const(1.1)]), Call("mul", [y, y])
    ).free_vars_ == frozenset(["x", "y"])


def test_free_vars_lam():
    x = Var("x", Type.Float)
    y = Var("y", Type.Float)

    assert Lam(x, y).free_vars_ == frozenset(["y"])
    assert Lam(x, x).free_vars_ == frozenset()


def test_eq():
    # This documents current behaviour and is not intended as a statement of desired behaviour.
    assert Var("x", Type.Float) == Var("x")

    # For Lam's, we *do* require different types, as these are different functions
    assert Lam(Var("x", Type.Float), Var("x")) != Lam(Var("x", Type.Integer), Var("x"))
    foo = StructuredName.from_str("foo")
    assert Def(foo, Type.Float, [Var("x", Type.Float)], Const(3.0)) != Def(
        foo, Type.Float, [Var("x", Type.Integer)], Const(3.0)
    )

    # Also check Def's distinguish on return type
    assert Const(3.0) == Const(3)  # python! TODO do we want to fix this?
    bar = StructuredName.from_str("bar")
    assert Def(bar, Type.Float, [], Const(3.0)) != Def(bar, Type.Integer, [], Const(3))

    # ...even tho Lam's don't
    assert Lam(Var("x", Type.Float), Const(3.0)) == Lam(Var("x", Type.Float), Const(3))
