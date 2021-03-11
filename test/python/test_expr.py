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
  assert Var("x").num_nodes == 1

  # Variables bound by "Let" are not counted
  three_nodes = Let(Var("x"), Const(3), Var("x"))
  assert three_nodes.num_nodes == 3

  # Neither is the target of a "Call"
  assert Call("add", [Var("x"), three_nodes]).num_nodes == 5

  assert Lam(Var("x"), Call("add", [Var("x"), Const(1)])).num_nodes == 4 # Lam, Call, x, 1

  x,y = Var("x"), Var("y")
  e = Let([x,y], Call("tuple", [Const(1), Const(2)]), Call("add", [x, y]))
  #from ksc.type_propagate import type_propagate
  #type_propagate(e, {StructuredName(("add", Type.Tuple(Type.Integer, Type.Integer))): Type.Integer})
  assert e.num_nodes == 7 # Let, tuple, 1, 2, Call, x, y
