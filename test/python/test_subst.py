from ksc.expr import Var, Const, Let, Lam, Call, If, Assert
from ksc.cav_subst import replace_free_vars, replace_subtree, replace_subtrees, ReplaceLocationRequest, _make_nonfree_var

def test_make_nonfree_var():
  assert _make_nonfree_var(Var("x")) == Var("_0")
  assert _make_nonfree_var(Var("_0")) == Var("_1")
  assert _make_nonfree_var(Var("_0x")) == Var("_0")
  assert _make_nonfree_var(Var("_1")) == Var("_0")
  assert _make_nonfree_var(If(Var("_0"), Var("_3"), Var("x"))) in [Var("_1"), Var("_2"), Var("_4")]

def test_replace_free_vars():
  # Replaces Var
  e = Let(Var("x"), Call("add", [Var("y"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  replaced = replace_free_vars(e, {"y": Var("z")})
  expected = Let(Var("x"), Call("add", [Var("z"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  assert replaced == expected

  # Replaces target of Call
  replaced2 = replace_free_vars(e, {"add": Var("foo")})
  expected2 = Let(Var("x"), Call("foo", [Var("y"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  assert replaced2 == expected2

def test_replace_free_vars_shadowing():
  e = Let(Var("x"), Call("add", [Var("x"), Const(1)]), Call("mul", [Var("x"), Const(2)]))
  replaced = replace_free_vars(e, {"x": Var("z")})
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
  replaced = replace_subtrees(e, [
    ReplaceLocationRequest(3, Var("w")),
    ReplaceLocationRequest(2, Var("v"))])
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
  replaced = replace_subtrees(e, [
    ReplaceLocationRequest(5, Var("w")),
    ReplaceLocationRequest(2, Const(0), replacer)
  ])
  assert replaced == Assert(Const(True), Var("success"))

def test_replace_subtree_avoids_capture():
  e = Let(Var("x"), If(Var("p"), Var("a"), Var("b")), Call("add", [Var("x"), Var("y")]))
  new_subtree = Call("mul", [Var("x"), Const(2)])
  assert _get_node(e, 7) == Var("y")
  replaced = replace_subtree(e, 7, new_subtree)
  # Must rename the "x".
  new_var = _make_nonfree_var(e) # No alpha-equivalence, so this is the name used.
  expected = Let(new_var, If(Var("p"), Var("a"), Var("b")), Call("add", [new_var, new_subtree]))
  assert replaced == expected

def test_replace_subtree_avoids_capturing_another():
  e = Let(Var("x"), If(Var("p"), Var("a"), Var("b")), Call("foo", [Var("_0"), Var("x"), Var("y")]))
  new_subtree = Call("mul", [Var("x"), Const(2)])
  assert _get_node(e, 8) == Var("y")
  replaced = replace_subtree(e, 8, new_subtree)
  # Must rename the "x".
  new_var = _make_nonfree_var(e) # No alpha-equivalence, so this is the name used.
  expected = Let(new_var, If(Var("p"), Var("a"), Var("b")), Call("foo", [Var("_0"), new_var, new_subtree]))
  assert new_var != Var("_0")
  assert replaced == expected

def test_replace_subtree_applicator_allows_capture():
  e = Let(Var("x"), If(Var("p"), Var("a"), Var("b")), Call("add", [Var("x"), Var("y")]))
  new_subtree = Call("mul", [Var("x"), Const(2)])
  assert _get_node(e, 7) == Var("y")
  replaced = replace_subtree(e, 7, Const(0), lambda e1,e2: new_subtree)
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
  replaced = replace_subtree(e, 5, foo_impl, apply_to_argument)
  new_var = _make_nonfree_var(e)
  expected = Let(new_var, # Bound variable renamed to avoid capturing "x" in foo_impl
    If(Var("p"), Var("a"), Var("b")),
    # The call:
    Let(Var("foo_arg"), new_var, # Renaming applied to argument
      Call("add", [Var("x"), Var("foo_arg")]) # Renaming not applied to the "x" in foo_impl
  ))
  assert replaced == expected
