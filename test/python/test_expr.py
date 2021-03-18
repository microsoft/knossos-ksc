import pytest

from ksc.type import Type
from ksc.expr import StructuredName, make_structured_name, Let, Lam, If, Call, Const, Var

from ksc.parse_ks import s_exps_from_string, parse_structured_name

def parse(s : str) -> StructuredName:
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

def test_num_nodes():
  assert Var("x").num_nodes_ == 1

  # Variables bound by "Let" are not counted
  three_nodes = Let(Var("x"), Const(3), Var("x"))
  assert three_nodes.num_nodes_ == 3

  # Neither is the target of a "Call"
  assert Call("add", [Var("x"), three_nodes]).num_nodes_ == 5

  assert Lam(Var("x"), Call("add", [Var("x"), Const(1)])).num_nodes_ == 4 # Lam, Call, x, 1

  x,y = Var("x"), Var("y")
  e = Let([x,y], Call("tuple", [Const(1), Const(2)]), Call("add", [x, y]))
  #from ksc.type_propagate import type_propagate
  #type_propagate(e, {StructuredName(("add", Type.Tuple(Type.Integer, Type.Integer))): Type.Integer})
  assert e.num_nodes_ == 7 # Let, tuple, 1, 2, Call, x, y

def test_free_vars():
  assert Const(11.3).free_vars_ == frozenset()

  x = Var("x")
  x_name = StructuredName.from_str("x")
  assert x.free_vars_ == frozenset([x_name])

  # Binding
  y = Var("y")
  y_name = StructuredName.from_str("y")
  assert Let(x, Call("add", [y, Const(1)]), Call("add", [x, x])).free_vars_ == frozenset(
    [y_name, StructuredName.from_str("add")]) # x not free

  # Rebinding
  assert Let(x, Call("add", [x, Const(1)]), Call("mul", [y, y])).free_vars_ == frozenset(
    [x_name, y_name, StructuredName.from_str("add"), StructuredName.from_str("mul")])

  assert Lam(x, x).free_vars_ == frozenset()
  assert Lam(x, y).free_vars_ == frozenset([y_name])
  assert If(x, y, Const(0)).free_vars_ == frozenset([x_name, y_name])
