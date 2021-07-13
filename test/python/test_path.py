import json
import pytest

from ksc.parse_ks import parse_expr_string
from ksc.path import Path, ExprWithPath, serialize_path, deserialize_path
from ksc import path


def test_path():
    e = parse_expr_string("(let (x (add y 2)) (mul x 3))")
    assert ExprWithPath.from_expr(
        e, [path.let_rhs, path.call_args[0]]
    ).expr == parse_expr_string("y")


def test_subexprs():
    e = parse_expr_string("(let (x (add y 2)) (mul x 3))")

    subexprs = ExprWithPath.from_expr(e).all_subexprs_with_paths()
    assert subexprs == [
        ExprWithPath.from_expr(e, [path.let_rhs]),
        ExprWithPath.from_expr(e, [path.let_body]),
    ]
    assert subexprs[0].expr == parse_expr_string("(add y 2)")
    assert subexprs[1].expr == parse_expr_string("(mul x 3)")


@pytest.mark.parametrize(
    "expr, path, res",
    [
        ("(let (x (add y 2)) (mul x 3))", (path.let_rhs, path.call_args[0]), "y"),
        (
            "(build 5 (lam (i : Integer) (let (j 3) (if p (add i j) (assert true (add i i))))))",
            (
                path.call_args[1],
                path.lam_body,
                path.let_body,
                path.if_f_body,
                path.assert_body,
            ),
            "(add i i)",
        ),
    ],
)
def test_json(expr: str, path: Path, res: str):
    e = parse_expr_string(expr)
    json_str = json.dumps(serialize_path(path))
    roundtrip = deserialize_path(json.loads(json_str))
    assert path == roundtrip
    # Additional sanity check that they behave the same way
    assert (
        ExprWithPath.from_expr(e, path).expr
        == ExprWithPath.from_expr(e, roundtrip).expr
        == parse_expr_string(res)
    )
