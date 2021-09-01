from math import isclose

from rlo import absint
from rlo.expr_sets import get_expression_set
from rlo import prettyprint
from ksc.type import Type
from rlo.absint import _core_cost as _cost
from testutils import parse_expr_typed


def _eval(ks_str, exp_res):
    exprenv = parse_expr_typed(ks_str)
    res, c = absint.evaluate(exprenv.expr, {})
    assert res == exp_res
    assert c == exprenv.cost()


def test_evaluate():
    _eval("(tuple 1.0 2)", (1.0, 2))

    _eval("(let xx 2.0 (tuple 1 xx))", (1, 2.0))

    _eval("(tuple (let x 1.0 (let x 2.0 x)) 2)", (2.0, 2))

    _eval("(tuple 3 (let x 2.0 x) 2)", (3, 2.0, 2))

    # In approx_mode (default), we count costs for both branches and return
    # abstract value:
    _eval("(let x 1.0 (if true 2.0 x))", absint.AbstractFloat())
    _eval("(let x 1.0 (if false 2.0 x))", absint.AbstractFloat())

    _eval("(let x (tuple 1 2 3) (select x 0))", 1)
    _eval("(let x (tuple 1 2 3) (select x 1))", 2)
    _eval("(let x (tuple 1 2 3) (select x 2))", 3)

    _eval("(let f (lam (x : Integer) (sub (sub 0 x) 0)) (f -33))", 33)
    _eval(
        "(let f (lam (x : Integer) (if (gt x 0) x (sub 0 x))) (f -33))",
        absint.AbstractInt(),
    )


def test_cost():
    """Check the cost of non-approx_mode. approx_mode is tested by comparing
    results with Expression.cost on blas_combined below.
    """

    def check(ks_str, exp_cost):
        e = parse_expr_typed(ks_str)
        c = absint.cost(e, approx_mode=False)
        assert c == exp_cost

    # Only count cost of branch actually taken:
    check("(let x 1.0 (if true 2.0 x))", _cost["let"] + _cost["if"] + _cost["constant"])
    check(
        "(let x 1.0 (if false 2.0 x))", _cost["let"] + _cost["if"] + _cost["variable"]
    )
    check(
        "(let f (lam (x : Integer) (if (gt x 0) x (sub 0 x))) (f -33))",
        (
            _cost["let"]
            + _cost["apply"]
            + _cost["if"]
            + 1  # cost of gt
            + 1  # cost of sub
        ),
    )

    # Cost of build should depend on size of vector built:
    check("(build 10 (lam i i))", _cost["build"] + 10 * _cost["variable"])

    def check_build_size(n):
        ks_str = """
            (let f (lam (x : (Vec Float)) (build (size x) (lam i (mul i i))))
            (f (build {n} (lam i 1.0))))
            """.format(
            n=n
        )
        exp_cost = (
            _cost["let"]
            + _cost["build"]
            + 1  # cost of size
            + n * 2  # cost of a mul
            + _cost["apply"]
            + _cost["build"]
            + n * _cost["variable"]
        )
        check(ks_str, exp_cost)

    for n in [10, 20, 35, 999]:
        check_build_size(n)


def test_cost_apply():
    e = parse_expr_typed("(lam (x : (Vec Float)) (build (size x) (lam i (mul i i))))")
    assert (
        absint.cost(e, absint.AbstractTensor((10,), Type.Float), False)
        == _cost["build"] + 10 * 2 + _cost["size"]
    )
    assert (
        absint.cost(e, absint.AbstractTensor((20,), Type.Float), False)
        == _cost["build"] + 20 * 2 + _cost["size"]
    )

    assert absint.cost(parse_expr_typed("(let x 1.0 (if true 2.0 x))")) == 1.1


def test_blas_combined():
    blas_expressions = get_expression_set("ksc/blas/blas_combined.kso").named_exprenvs()

    for n, e in blas_expressions:
        c0 = e.cost()
        c1 = absint.cost(e)
        if not isclose(c0, c1):
            print()
            prettyprint.cpprint(e)
            print(n, c0, c1)
            assert False


if __name__ == "__main__":
    test_evaluate()
