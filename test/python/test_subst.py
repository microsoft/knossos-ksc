import pytest

from ksc.expr import Var, Const, Let, Lam, Call
from ksc.cav_subst import replace_free_vars, replace_subtree, replace_subtrees, ReplaceLocationRequest, _make_nonfree_var, _get_indexed_children, _get_subtree_size
from ksc.parse_ks import parse_expr_string

def test_subtree_size():
  assert _get_subtree_size(Var("x")) == 1

  # Variables bound by "Let" are not counted
  three_nodes = Let(Var("x"), Const(3), Var("x"))
  assert _get_subtree_size(three_nodes) == 3

  # Neither is the target of a "Call"
  assert _get_subtree_size(Call("add", [Var("x"), three_nodes])) == 5

  assert _get_subtree_size(Lam(Var("x"), Call("add", [Var("x"), Const(1)]))) == 4 # Lam, Call, x, 1

  x,y = Var("x"), Var("y")
  e = Let([x,y], Call("tuple", [Const(1), Const(2)]), Call("add", [x, y]))
  #from ksc.type_propagate import type_propagate
  #type_propagate(e, {StructuredName(("add", Type.Tuple(Type.Integer, Type.Integer))): Type.Integer})
  assert _get_subtree_size(e) == 7 # Let, tuple, 1, 2, Call, x, y


def test_make_nonfree_var():
  assert _make_nonfree_var("x", []) == Var("x_0")
  assert _make_nonfree_var("x", [Var("x")]) == Var("x_0")
  assert _make_nonfree_var("x", [Var("x_1")]) == Var("x_0")
  assert _make_nonfree_var("x", [Var("x_0")]) == Var("x_1")
  assert _make_nonfree_var("x_0", []) == Var("x_0_0")
  assert _make_nonfree_var("x_1", [Var("x_1_0"), Var("x_1_1")]) == Var("x_1_2")
  assert _make_nonfree_var("", [parse_expr_string("(if _0 _3 x)")]) == Var("_1")

def test_replace_free_vars():
  # Replaces Var
  e = parse_expr_string("(let (x (add y 1)) (mul x 2))")
  replaced = replace_free_vars(e, {"y": Var("z")})
  expected = parse_expr_string("(let (x (add z 1)) (mul x 2))")
  assert replaced == expected

def test_replace_free_vars_shadowing():
  e = parse_expr_string("(let (x (add x 1)) (mul x 2))")
  replaced = replace_free_vars(e, {"x": Var("z")})
  expected = parse_expr_string("(let (x (add z 1)) (mul x 2))")
  assert replaced == expected

def _get_node(expr, idx):
  if idx==0:
    return expr
  for start_idx, ch, end_idx in _get_indexed_children(expr):
    if idx < end_idx:
      return _get_node(ch, idx - start_idx)
  raise IndexError(f"{idx} out of bounds in expr of size {expr.subtree_size_}")

def test_replace_subtrees():
  e = parse_expr_string("(foo x y z)")
  replaced = replace_subtrees(e, [
    ReplaceLocationRequest(3, Var("w")),
    ReplaceLocationRequest(2, Var("v"))])
  expected = parse_expr_string("(foo x v w)")
  assert replaced == expected

def test_replace_subtrees_nested():
  e = parse_expr_string("(assert true (foo x y z))")
  inner_replaced = parse_expr_string("(foo x y w)")
  path_to_call = 2
  path_to_z = 5
  assert isinstance(_get_node(e, path_to_call), Call)
  assert _get_node(e, path_to_z) == Var("z")
  with pytest.raises(ValueError, match="nested"):
    replace_subtrees(e, [
      ReplaceLocationRequest(path_to_z, Var("w")),
      ReplaceLocationRequest(path_to_call, Const(0))
    ])

def test_replace_subtree_avoids_capture():
  e = parse_expr_string("(let (x (if p a b)) (add x y))")
  # Replace the y with a subtree mentioning a free "x".
  # The "x" in the new subtree should not be captured by the bound "x"
  # (that is, it should have the same value as in "e"'s scope).
  new_subtree = parse_expr_string("(mul x 2)")
  assert _get_node(e, 7) == Var("y")
  replaced = replace_subtree(e, 7, new_subtree)
  # Must rename the "x".
  new_var = _make_nonfree_var("x", [e]) # No alpha-equivalence, so this is the name used.
  expected = parse_expr_string(f"(let ({new_var} (if p a b)) (add {new_var} (mul x 2)))")
  assert replaced == expected

def test_replace_subtree_avoids_capturing_another():
  new_subtree = parse_expr_string("(mul x 2)")
  conflicting_var = _make_nonfree_var("x", [new_subtree])  # But, this already exists
  e = parse_expr_string(f"(lam (x : Integer) (foo {conflicting_var} x y))")
  assert _get_node(e, 4) == Var("y")
  replaced = replace_subtree(e, 4, new_subtree)
  new_var = replaced.arg  # No alpha-equivalence, so this is the name used; this has decl=True
  expected = parse_expr_string(f"(lam ({new_var}) (foo {conflicting_var} {new_var.name} (mul x 2)))")
  assert new_var != conflicting_var
  assert replaced == expected

def test_replace_subtree_applicator_allows_capture():
  e = parse_expr_string("(let (x (if p a b)) (add x y))")
  # Replace the y with a subtree mentioning "x". But, here we want the new "x" to be the
  # x present in the same scope of y. This can be achieved by passing a lambda function:
  new_subtree = parse_expr_string("(mul x 2)")
  assert _get_node(e, 7) == Var("y")
  replaced = replace_subtree(e, 7, Const(0), lambda _e1, _e2: new_subtree)
  expected = parse_expr_string("(let (x (if p a b)) (add x (mul x 2)))")
  assert replaced == expected

def test_replace_subtree_allows_inlining_call():
  # In the future this will become part of the inline-call rule,
  # but for now we merely test that replace_subtree supports it.
  e = parse_expr_string("(let (x (if p a b)) (foo x))")
  foo_impl = parse_expr_string("(lam (foo_arg : Float) (add x foo_arg))")
  assert type(_get_node(e, 5)) == Call
  def apply_to_argument(func, call):
    assert type(func) == Lam and type(call) == Call
    assert len(call.args) == 1  # More would require making a Tuple
    return Let(Var(func.arg.name), # drop the type
              call.args[0],
              func.body)
  replaced = replace_subtree(e, 5, foo_impl, apply_to_argument)
  new_var = _make_nonfree_var("x", [e])
  expected = parse_expr_string(f"""(let ({new_var} (if p a b))  ; Bound variable renamed to avoid capturing "x" in foo_impl
    ; The call:
    (let (foo_arg {new_var}) ; Renaming applied to argument
      (add x foo_arg))) ; Renaming not applied to the "x" in foo_impl
   """)
  assert replaced == expected
