import pytest

from rlo.costs import (
    elementwise_cost,
    build_malloc_cost,
    assumed_vector_size,
    apply_cost,
    let_cost,
)
from rlo.expression import Expression, EF, TypePropagationError
from rlo.expression_util import SymtabAndDefs
from testutils import make_toplevel as MT
from rlo.sparser import parse_defs
from ksc.type import Type

from numpy.testing import assert_approx_equal

from test_cost_ksc import basic_symtab  # pylint: disable=unused-import


def test_cost_model_if():
    p = Expression.Variable("p", Type.Bool)
    existingvec = Expression.Variable("existingvec", Type.Tensor(1, Type.Integer))
    build1 = EF.Build(100, "x", Expression.Variable("x") * 2)
    e = MT(EF.If(p, build1, existingvec))

    # Symmetry:
    assert e.cost() == MT(EF.If(p, existingvec, build1)).cost()

    # If (True, ...) costs more than first arm:
    assert e.cost() == MT(EF.If(True, build1, existingvec)).cost()
    assert e.cost() > MT(build1).cost()

    # Change to the expensive arm has more impact than to the cheaper one
    cheaper = EF.Build(100, "y", Expression.Variable("y") + 1)
    e2 = MT(EF.If(p, build1, cheaper))
    assert e2.cost() > e.cost()

    e3 = MT(EF.If(p, cheaper, existingvec))
    assert e3.cost() < e.cost()
    assert (e.cost() - e3.cost()) > 100 * (e2.cost() - e.cost())


def test_cost_model_apply():
    x = Expression.Variable("x")
    y = Expression.Variable("y", Type.Integer)

    func = EF.Lam(y, EF.Build(100, x, x * y))
    func_cost = MT(func).cost()
    applied = MT(EF.Let("f", func, EF.Apply("f", 10)))
    assert applied.cost() > func_cost
    inlined = MT(EF.Let(func.left, 10, func.right))
    assert applied.cost() > inlined.cost()

    applied_in_build = MT(EF.Let("f", func, EF.Build(100, "i", EF.Apply("f", "i"))))
    assert applied_in_build.cost() > func_cost * assumed_vector_size
    inlined_in_build = MT(EF.Build(100, "i", EF.Let(y, "i", func.right)))
    assert applied_in_build.cost() > inlined_in_build.cost()
    # We can't quite expect the cost difference to be assumed_vector_size* because of the number of times we execute LetBind's
    assert (applied_in_build.cost() - inlined_in_build.cost()) > (
        assumed_vector_size * 0.9 * (applied.cost() - inlined.cost())
    )


def test_cost_model_apply_edef():
    e = SymtabAndDefs(
        symtab={"f": Type.Lam(Type.Integer, Type.Float)},
        defs={"cost$f": EF.Lam(Expression.Variable("x", Type.Integer), 2)},
    ).make_toplevel(EF.Apply("f", 10))
    assert e.cost() == apply_cost + 2

    with pytest.raises(TypePropagationError, match="free variable f"):
        MT(EF.Apply("f", 10))

    sd = SymtabAndDefs(symtab={"f": Type.Lam(Type.Float, Type.Float)})
    with pytest.raises(TypePropagationError, match="cannot accept argument"):
        sd.make_toplevel(EF.Apply("f", 10))


def test_cost_model_parsed_edef():
    ((n, e),) = parse_defs(
        """
    (def cost$f Float (x : Integer) 2.3)
    (edef f Float (Integer))
    (def foo Float (arg : Integer) (f (add arg 1)))"""
    )
    assert n == "foo"
    assert_approx_equal(e.cost(), apply_cost + let_cost + 2.3 + 1)  # 1 is add_cost


def test_cost_vector_vector_add():
    v1 = Expression.Variable("v1", Type.Tensor(1, Type.Float))
    v2 = Expression.Variable("v2", Type.Tensor(1, Type.Float))
    e = MT(v1 + v2)
    assert e.cost() == build_malloc_cost + assumed_vector_size * 1
    assert e.cost() == elementwise_cost(Type.Tensor(1, Type.Float), 1)


def test_cost_scalar_vector_mul():
    f = Expression.Variable("f", Type.Float)
    v = Expression.Variable("v", Type.Tensor(1, Type.Float))
    e = MT(f * v)
    assert e.cost() == build_malloc_cost + assumed_vector_size * 2


def test_cost_scalar_matrix_mul():
    f = Expression.Variable("f", Type.Float)
    m = Expression.Variable("m", Type.Tensor(1, Type.Tensor(1, Type.Float)))
    e = MT(f * m)
    assert e.cost() == (build_malloc_cost + assumed_vector_size ** 2 * 2)


def test_cost_scalar_tuple_mul():
    tup = Expression.Variable(
        "tup", Type.Tuple(Type.Tensor(1, Type.Float), Type.Tensor(1, Type.Float))
    )
    f = Expression.Variable("f", Type.Float)
    e = MT(f * tup)
    assert e.cost() == (build_malloc_cost + assumed_vector_size * 2 * 2)


def test_ksc_rlo_costs_same(basic_symtab):  # pylint: disable=redefined-outer-name
    from rlo.costs_ksc import compute_cost
    from ksc.parse_ks import parse_ks_file
    from test_cost_ksc import parse_and_type
    from ksc.type_propagate import type_propagate
    from testutils import parse_expr_typed

    e = "(add (let (z 3.0) (mul y z)) x)"
    st = {"x": Type.Float, "y": Type.Float}
    assert (
        compute_cost(parse_and_type(e, {**basic_symtab, **st}), {})
        == parse_expr_typed(e, st).cost()
    )

    e2 = "(build (size v) (lam (i : Integer) (add (index i v) i)))"
    st2 = {"v": Type.Tensor(1, Type.Integer)}
    assert (
        compute_cost(parse_and_type(e2, {**basic_symtab, **st2}), {})
        == parse_expr_typed(e2, st2).cost()
    )

    e3 = "(mul (foo 5.2) (foo 6.1))"
    symtab3 = {**basic_symtab}
    (def_foo,) = parse_ks_file("(def foo Float (x : Float) (add x 1.3))")
    type_propagate(def_foo, symtab3)
    cost_ksc = compute_cost(parse_and_type(e3, symtab3), {def_foo.name: def_foo})

    cost_rlo = parse_expr_typed(
        f"(let (foo (lam (x : Float) (add x 1.3))) {e3})"
    ).cost()
    # RLO adds a cost of 0.1 for the "let" corresponding to that "def" in the prelude
    assert cost_ksc == cost_rlo - let_cost
