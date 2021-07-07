import pytest

from ksc.expr import Var, Const, Let, Lam, Call
from ksc.cav_subst import (
    replace_free_vars,
    replace_subtree,
    replace_subtrees,
    ReplaceLocationRequest,
    make_nonfree_var,
)
from ksc.parse_ks import parse_expr_string
from ksc import path
from ksc.path import Path, ExprWithPath


def get_node_at_location(e, p: Path):
    return ExprWithPath.from_expr(e, p).expr


def test_make_nonfree_var():
    assert make_nonfree_var("x", []) == Var("x_0")
    assert make_nonfree_var("x", [Var("x")]) == Var("x_0")
    assert make_nonfree_var("x", [Var("x_1")]) == Var("x_0")
    assert make_nonfree_var("x", [Var("x_0")]) == Var("x_1")
    assert make_nonfree_var("x_0", []) == Var("x_0_0")
    assert make_nonfree_var("x_1", [Var("x_1_0"), Var("x_1_1")]) == Var("x_1_2")
    assert make_nonfree_var("", [parse_expr_string("(if _0 _3 x)")]) == Var("_1")


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


def test_replace_subtrees():
    e = parse_expr_string("(foo x y z)")
    replaced = replace_subtrees(
        e,
        [
            ReplaceLocationRequest((path.call_args[2],), Var("w")),
            ReplaceLocationRequest((path.call_args[1],), Var("v")),
        ],
    )
    expected = parse_expr_string("(foo x v w)")
    assert replaced == expected


def test_replace_subtrees_nested():
    e = parse_expr_string("(assert true (foo x y z))")
    path_to_call = (path.assert_body,)
    path_to_z = (path.assert_body, path.call_args[2])
    assert isinstance(get_node_at_location(e, path_to_call), Call)
    assert get_node_at_location(e, path_to_z) == Var("z")
    with pytest.raises(ValueError, match="nested"):
        replace_subtrees(
            e,
            [
                ReplaceLocationRequest(path_to_z, Var("w")),
                ReplaceLocationRequest(path_to_call, Const(0)),
            ],
        )


def test_replace_subtree_avoids_capture():
    e = parse_expr_string("(let (x (if p a b)) (add x y))")
    # Replace the y with a subtree mentioning a free "x".
    # The "x" in the new subtree should not be captured by the bound "x"
    # (that is, it should have the same value as in "e"'s scope).
    new_subtree = parse_expr_string("(mul x 2)")
    path_to_y = (path.let_body, path.call_args[1])
    assert get_node_at_location(e, path_to_y) == Var("y")
    replaced = replace_subtree(e, path_to_y, new_subtree)
    # Must rename the "x".
    new_var = make_nonfree_var(
        "x", [e]
    )  # No alpha-equivalence, so this is the name used.
    expected = parse_expr_string(
        f"(let ({new_var} (if p a b)) (add {new_var} (mul x 2)))"
    )
    assert replaced == expected


def test_replace_subtree_avoids_capturing_another():
    new_subtree = parse_expr_string("(mul x 2)")
    conflicting_var = make_nonfree_var("x", [new_subtree])  # But, this already exists
    assert conflicting_var.name == "x_0"
    e = parse_expr_string(f"(lam (x : Integer) (foo x_0 x y))")
    path_to_y = (path.lam_body, path.call_args[2])
    assert get_node_at_location(e, path_to_y) == Var("y")
    replaced = replace_subtree(e, path_to_y, new_subtree)
    # Test exact equality without alpha-equivalence, using the name generated
    # (this has decl=True):
    new_var = replaced.arg
    expected = parse_expr_string(f"(lam {new_var} (foo x_0 {new_var.name} (mul x 2)))")
    assert new_var != conflicting_var
    assert replaced == expected


def test_replace_subtree_applicator_allows_capture():
    e = parse_expr_string("(let (x (if p a b)) (add x y))")
    # Replace the y with a subtree mentioning "x". But, here we want the new "x" to be the
    # x present in the same scope of y. This can be achieved by passing a lambda function:
    new_subtree = parse_expr_string("(mul x 2)")
    path_to_y = (path.let_body, path.call_args[1])
    assert get_node_at_location(e, path_to_y) == Var("y")
    replaced = replace_subtree(e, path_to_y, Const(0), lambda _e1, _e2: new_subtree)
    expected = parse_expr_string("(let (x (if p a b)) (add x (mul x 2)))")
    assert replaced == expected


def test_replace_subtree_allows_inlining_call():
    # In the future this will become part of the inline-call rule,
    # but for now we merely test that replace_subtree supports it.
    e = parse_expr_string("(let (x (if p a b)) (foo x))")
    foo_impl = parse_expr_string("(lam (foo_arg : Float) (add x foo_arg))")
    path_to_call = (path.let_body,)
    assert type(get_node_at_location(e, path_to_call)) == Call

    def apply_to_argument(func, call):
        assert type(func) == Lam and type(call) == Call
        assert len(call.args) == 1  # More would require making a Tuple
        return Let(Var(func.arg.name), call.args[0], func.body)  # drop the type

    replaced = replace_subtree(e, path_to_call, foo_impl, apply_to_argument)
    new_var = make_nonfree_var("x", [e])
    expected = parse_expr_string(
        f"""(let ({new_var} (if p a b))  ; Bound variable renamed to avoid capturing "x" in foo_impl
    ; The call:
    (let (foo_arg {new_var}) ; Renaming applied to argument
      (add x foo_arg))) ; Renaming not applied to the "x" in foo_impl
   """
    )
    assert replaced == expected
