from ksc.expr_utils import list_binders, binder_sets_per_free_var
from ksc.parse_ks import parse_expr_string


def test_list_binders():
    e = parse_expr_string("(add x y)")
    assert list_binders(e) == []

    e = parse_expr_string("(build 5 (lam (i : Integer) (add (to_float i) 5.0)))")
    assert list_binders(e) == ["i"]

    e = parse_expr_string("(add (let (x e) x) (let (y e2) (let (x e3) (add x y))))")
    assert list_binders(e) == ["x", "y", "x"]


def test_binder_sets_per_free_var():
    e = parse_expr_string(
        """
        (foo
            a
            (let (x e) a)
            (let (y e2) (let (z e) (add z a)))
        )"""
    )
    actual = binder_sets_per_free_var(e)
    assert actual == {
        "a": [frozenset(), frozenset(["x"]), frozenset(["y", "z"])],
        "e": [frozenset(), frozenset(["y"])],
        "e2": [frozenset()],
    }
