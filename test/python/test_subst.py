import pytest

from ksc.expr import Var, Const, Let, Lam, Call
from ksc.cav_subst import replace_free_vars, replace_subtree, replace_subtrees, ReplaceLocationRequest, _make_nonfree_var
from ksc.parse_ks import parse_expr_string

def test_make_nonfree_var():
  assert _make_nonfree_var("x", [Var("x")]) == Var("x_0")
  assert _make_nonfree_var("x", [Var("x_0")]) == Var("x_1")
  assert _make_nonfree_var("x_0", []) == Var("x_1")
  assert _make_nonfree_var("x_0", [Var("x_1")]) == Var("x_2")
  assert _make_nonfree_var("x_2", [Var("x_0")]) == Var("x_3")
  assert _make_nonfree_var("", [parse_expr_string("(if _0 _3 x)")]) == Var("_1")

def test_replace_free_vars():
  # Replaces Var
  e = parse_expr_string("(let (x (add y 1)) (mul x 2))")
  replaced = replace_free_vars(e, {"y": Var("z")})
  expected = parse_expr_string("(let (x (add z 1)) (mul x 2))")
  assert replaced == expected

  # Replaces target of Call
  replaced2 = replace_free_vars(e, {"add": Var("foo")})
  expected2 = parse_expr_string("(let (x (foo y 1)) (mul x 2))")
  assert replaced2 == expected2

def test_replace_free_vars_shadowing():
  e = parse_expr_string("(let (x (add x 1)) (mul x 2))")
  replaced = replace_free_vars(e, {"x": Var("z")})
  expected = parse_expr_string("(let (x (add z 1)) (mul x 2))")
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
  e = parse_expr_string("(foo x y z)")
  replaced = replace_subtrees(e, [
    ReplaceLocationRequest(3, Var("w")),
    ReplaceLocationRequest(2, Var("v"))])
  expected = parse_expr_string("(foo x v w)")
  assert replaced == expected

def test_replace_subtrees_nested():
  e = parse_expr_string("(assert true (foo x y z))")
  inner_replaced = parse_expr_string("(foo x y w)")
  def replacer(e1, e2):
    assert e1 == Const(0)
    assert e2 == inner_replaced
    return Var("success")
  assert isinstance(_get_node(e, 2), Call)
  assert _get_node(e, 5) == Var("z")
  with pytest.raises(ValueError):
    replace_subtrees(e, [
      ReplaceLocationRequest(5, Var("w")),
      ReplaceLocationRequest(2, Const(0), replacer)
    ])

def test_replace_subtree_avoids_capture():
  e = parse_expr_string("(let (x (if p a b)) (add x y))")
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
  e = parse_expr_string(f"(let (x (if p a b)) (foo {conflicting_var} x y))")
  assert _get_node(e, 8) == Var("y")
  replaced = replace_subtree(e, 8, new_subtree)
  new_var = replaced.vars  # No alpha-equivalence, so this is the name used.
  expected = parse_expr_string(f"(let ({new_var} (if p a b)) (foo {conflicting_var} {new_var} (mul x 2)))")
  assert new_var != conflicting_var
  assert replaced == expected

def test_replace_subtree_applicator_allows_capture():
  e = parse_expr_string("(let (x (if p a b)) (add x y))")
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
