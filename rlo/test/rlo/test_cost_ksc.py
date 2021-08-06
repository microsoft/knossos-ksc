import pytest
from typing import Mapping, Union

from rlo.costs import (
    elementwise_cost,
    build_malloc_cost,
    assumed_vector_size,
    apply_cost,
    mul_cost,
    add_cost,
)

from ksc.type import Type
from ksc.expr import Expr, StructuredName
from ksc.parse_ks import parse_ks_file, parse_expr_string
from ksc.type_propagate import type_propagate, type_propagate_decls
from rlo.costs_ksc import compute_cost


# pylint: disable=redefined-outer-name
@pytest.fixture
def basic_symtab():
    symtab = {}
    type_propagate_decls(
        list(
            parse_ks_file(
                # Add some extra edef's in that are assumed by RLO
                """
(edef add Integer (Tuple Integer Integer))
(edef add Float (Tuple Float Float))
(edef add (Vec Float) (Tuple (Vec Float) (Vec Float)))
(edef mul Integer (Tuple Integer Integer))
(edef mul Float (Tuple Float Float))
(edef mul (Tuple (Vec Float) (Vec Float)) (Tuple Float (Tuple (Vec Float) (Vec Float))))
(edef mul (Vec (Vec Float)) (Tuple Float (Tensor 2 Float)))
(edef mul (Vec Float) (Tuple Float (Vec Float)))
    """
            )
        ),
        symtab,
    )
    return symtab


def parse_and_type(
    expr: str, symtab: Mapping[Union[str, StructuredName], Type]
) -> Expr:
    e = parse_expr_string(expr)
    type_propagate(e, symtab)
    return e


def test_cost_model_if(basic_symtab):
    symtab_with_p_v = {
        **basic_symtab,
        "p": Type.Bool,
        "v": Type.Tensor(1, Type.Integer),
    }
    build1 = "(build 100 (lam (x : Integer) (mul x 2)))"
    e = parse_and_type(f"(if p {build1} v)", symtab_with_p_v)
    cost = compute_cost(e, {})

    # Symmetry:
    assert cost == compute_cost(
        parse_and_type(f"(if p v {build1})", symtab_with_p_v), {}
    )

    # If (True, ...) costs more than first arm:
    assert cost == compute_cost(
        parse_and_type(f"(if true {build1} v)", symtab_with_p_v), {}
    )
    assert cost > compute_cost(parse_and_type(build1, symtab_with_p_v), {})

    # Change to the expensive arm has more impact than to the cheaper one
    cheap_build = "(build 100 (lam (y : Integer) (add y 1)))"
    cost2 = compute_cost(
        parse_and_type(f"(if p {build1} {cheap_build})", symtab_with_p_v), {}
    )
    assert cost2 > cost

    cost3 = compute_cost(parse_and_type(f"(if p {cheap_build} v)", symtab_with_p_v), {})
    assert cost3 < cost
    assert (cost - cost3) > 100 * (cost2 - cost)


def test_cost_model_apply(basic_symtab):
    f_body = "(build 100 (lam (x : Integer) (mul x y)))"
    symtab_with_y = {**basic_symtab, "y": Type.Integer}
    body_cost = compute_cost(parse_and_type(f_body, symtab_with_y), {})

    (decl,) = parse_ks_file(f"(def f (Vec Integer) (y : Integer) {f_body})")
    symtab_with_f = {**basic_symtab}
    type_propagate(decl, symtab_with_f)
    defs = {decl.name: decl}

    applied_cost = compute_cost(parse_and_type("(f 10)", symtab_with_f), defs)

    assert applied_cost > body_cost
    inlined_cost = compute_cost(
        parse_and_type(f"(let (y 10) {f_body})", basic_symtab), {}
    )
    assert applied_cost > inlined_cost

    applied_in_build_cost = compute_cost(
        parse_and_type("(build 100 (lam (i : Integer) (f i)))", symtab_with_f), defs
    )
    assert applied_in_build_cost > body_cost * assumed_vector_size
    inlined_in_build = parse_and_type(
        f"(build 100 (lam (i : Integer) (let (y i) {f_body})))", basic_symtab
    )
    inlined_in_build_cost = compute_cost(inlined_in_build, {})
    assert applied_in_build_cost > inlined_in_build_cost
    # We can't quite expect the cost difference to be assumed_vector_size* because of the number of times we execute LetBind's
    assert (applied_in_build_cost - inlined_in_build_cost) > (
        assumed_vector_size * 0.9 * (applied_cost - inlined_cost)
    )


def test_cost_model_edef(basic_symtab):
    f, cost_f = parse_ks_file(
        """(edef f Float (Integer))
    (def cost$f Float (x : Integer) 2.3)"""
    )
    symtab_with_f = {**basic_symtab}
    type_propagate_decls([f, cost_f], symtab_with_f)
    defs = {cost_f.name: cost_f}

    e = parse_and_type("(f 10)", symtab_with_f)
    assert compute_cost(e, defs) == apply_cost + 2.3

    with pytest.raises(ValueError, match=r"definition or cost\$ function for f"):
        compute_cost(e, {})


def test_cost_vector_vector_add(basic_symtab):
    symtab_with_v1_v2 = {
        **basic_symtab,
        "v1": Type.Tensor(1, Type.Float),
        "v2": Type.Tensor(1, Type.Float),
    }

    cost = compute_cost(parse_and_type("(add v1 v2)", symtab_with_v1_v2), {})
    assert cost == build_malloc_cost + assumed_vector_size * add_cost
    assert cost == elementwise_cost(Type.Tensor(1, Type.Float), 1)


def test_cost_scalar_vector_mul(basic_symtab):
    symtab_with_f_v = {**basic_symtab, "f": Type.Float, "v": Type.Tensor(1, Type.Float)}
    cost = compute_cost(parse_and_type("(mul f v)", symtab_with_f_v), {})
    assert cost == build_malloc_cost + assumed_vector_size * mul_cost


def test_cost_scalar_matrix_mul(basic_symtab):
    symtab_with_f_m = {**basic_symtab, "f": Type.Float, "m": Type.Tensor(2, Type.Float)}
    cost = compute_cost(parse_and_type("(mul f m)", symtab_with_f_m), {})
    assert cost == (build_malloc_cost + assumed_vector_size ** 2 * 2)


def test_cost_scalar_tuple_mul(basic_symtab):
    tuple_type = Type.Tuple(Type.Tensor(1, Type.Float), Type.Tensor(1, Type.Float))
    symtab_with_x_tup = {**basic_symtab, "x": Type.Float, "tup": tuple_type}
    cost = compute_cost(parse_and_type("(mul x tup)", symtab_with_x_tup), {},)
    assert cost == (build_malloc_cost + assumed_vector_size * mul_cost * 2)


def test_assert_ignores_condition(basic_symtab):
    symtab_with_v = {**basic_symtab, "v": Type.Tensor(1, Type.Float)}
    expensive_pred = parse_and_type("(gt (sum v) 10.0)", symtab_with_v)
    cheap_pred = parse_and_type("(gt (index 0 v) 10.0)", symtab_with_v)
    assert compute_cost(expensive_pred, {}) > compute_cost(cheap_pred, {})
    assert (
        compute_cost(
            parse_and_type("(assert (gt (sum v) 10.0) 1.0)", symtab_with_v), {}
        )
        == compute_cost(
            parse_and_type("(assert (gt (index 0 v) 10.0) 1.0)", symtab_with_v), {}
        )
        < compute_cost(
            parse_and_type("(assert (gt (index 0 v) 10.0) (sum v))", symtab_with_v), {}
        )
    )
