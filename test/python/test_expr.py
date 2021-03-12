import pytest

from ksc.type import Type
from ksc.expr import StructuredName, make_structured_name, Expr, Let, Lam, If, Call, Const, Var, Assert

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

def test_free_vars():
  assert Const(11.3).free_vars == frozenset()

  x = Var("x")
  x_name = StructuredName.from_str("x")
  assert x.free_vars == frozenset([x_name])

  # Binding
  y = Var("y")
  y_name = StructuredName.from_str("y")
  assert Let(x, Call("add", [y, Const(1)]), Call("add", [x, x])).free_vars == frozenset(
    [y_name, StructuredName.from_str("add")]) # x not free

  # Rebinding
  assert Let(x, Call("add", [x, Const(1)]), Call("mul", [y, y])).free_vars == frozenset(
    [x_name, y_name, StructuredName.from_str("add"), StructuredName.from_str("mul")])

  assert Lam(x, x).free_vars == frozenset()
  assert Lam(x, y).free_vars == frozenset([y_name])
  assert If(x, y, Const(0)).free_vars == frozenset([x_name, y_name])

def test_next_unused_var_num():
  # Testing a private/protected member; there is no need to expose it to clients.
  assert Var("x")._next_unused_var_num == 0
  assert Var("_1")._next_unused_var_num == 2
  assert Var("_2x")._next_unused_var_num == 0
  assert Var("__1")._next_unused_var_num == 0
  assert If(Var("_0"), Var("_3"), Var("x"))._next_unused_var_num == 4
  assert Let(Var("_1"), Var("x"), Var("_1"))._next_unused_var_num == 2 # Conservative but safe

def test_replace_free_vars():
  # Replaces Var
  e = Let(Var("x"), Call("add", [Var("y"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  replaced = e.replace_free_vars({"y": Var("z")})
  expected = Let(Var("x"), Call("add", [Var("z"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  assert replaced == expected

  # Replaces target of Call
  replaced2 = e.replace_free_vars({"add": Var("foo")})
  expected2 = Let(Var("x"), Call("foo", [Var("y"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  assert replaced2 == expected2

def test_replace_free_vars_shadowing():
  e = Let(Var("x"), Call("add", [Var("x"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  replaced = e.replace_free_vars({"x": Var("z")})
  expected = Let(Var("x"), Call("add", [Var("z"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  assert replaced == expected

def _get_node(expr, idx):
  # Note, this somewhat repeats the logic in Expr._cav_children.
  # TODO: Consider exposing via Expr.children_with_indices(): Generator[Tuple[Expr, int]] ?
  if idx==0: return expr
  idx -= 1
  for ch in expr.children():
    if idx < ch.num_nodes:
      return _get_node(ch, idx)
    idx -= ch.num_nodes

def test_replace_subtrees():
  e = Call("foo", [Var("x"), Var("y"), Var("z")])
  replaced = e.replace_subtrees([
    Expr.ReplaceLocationRequest(3, Var("w")),
    Expr.ReplaceLocationRequest(2, Var("v"))])
  expected = Call("foo", [Var("x"), Var("v"), Var("w")])
  assert replaced == expected

def test_replace_subtrees_inner_first():
  e = Assert(Const(True), Call("foo", [Var("x"), Var("y"), Var("z")]))
  inner_replaced = Call("foo", [Var("x"), Var("y"), Var("w")])
  def replacer(e1, e2):
    assert e1 == Const(0)
    assert e2 == inner_replaced
    return Var("success")
  assert _get_node(e, 5) == Var("z")
  replaced = e.replace_subtrees([
    Expr.ReplaceLocationRequest(5, Var("w")),
    Expr.ReplaceLocationRequest(2, Const(0), replacer)
  ])
  assert replaced == Assert(Const(True), Var("success"))

def test_replace_subtree_avoids_capture():
  e = Let(Var("x"), If(Var("p"), Var("a"), Var("b")), Call("add", [Var("x"), Var("y")]))
  new_subtree = Call("mul", [Var("x"), Const(2)])
  assert _get_node(e, 7) == Var("y")
  replaced = e.replace_subtree(7, new_subtree)
  # Must rename the "x".
  # Since we don't have alpha-equivalence yet, this must second-guess the new name
  expected = Let(Var("_0"), If(Var("p"), Var("a"), Var("b")), Call("add", [Var("_0"), new_subtree]))
  assert replaced == expected

def test_replace_subtree_applicator_allows_capture():
  e = Let(Var("x"), If(Var("p"), Var("a"), Var("b")), Call("add", [Var("x"), Var("y")]))
  new_subtree = Call("mul", [Var("x"), Const(2)])
  assert _get_node(e, 7) == Var("y")
  replaced = e.replace_subtree(7, Const(0), lambda e1,e2: new_subtree)
  expected = Let(Var("x"), If(Var("p"), Var("a"), Var("b")), Call("add", [Var("x"), new_subtree]))
  assert replaced == expected

def test_replace_subtree_allows_inlining_call():
  # In the future this will become part of the inline-call rule,
  # but for now we merely test that replace_subtree supports it.
  e = Let(Var("x"), If(Var("p"), Var("a"), Var("b")), Call("foo", [Var("x")]))
  foo_impl = Lam(Var("foo_arg"), Call("add", [Var("x"), Var("foo_arg")]))
  assert type(_get_node(e, 5)) == Call
  def apply_to_argument(func, call):
    assert type(func) == Lam and type(call) == Call
    assert len(call.args) == 1  # More would require making a Tuple
    return Let(func.arg, call.args[0], func.body)
  replaced = e.replace_subtree(5, foo_impl, apply_to_argument)
  expected = Let(Var("_0"), # Bound variable renamed to avoid capturing "x" in foo_impl
    If(Var("p"), Var("a"), Var("b")),
    # The call:
    Let(Var("foo_arg"), Var("_0"), # Renaming applied to argument
      Call("add", [Var("x"), Var("foo_arg")]) # Renaming not applied to the "x" in foo_impl
  ))
  assert replaced == expected
