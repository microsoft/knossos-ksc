import pytest

from ksc.type import Type, KSTypeError
from ksc.type_propagate import type_propagate_decls, type_propagate
from ksc.expr import StructuredName, If, Const
from ksc.parse_ks import (
    parse_expr_string,
    parse_ks_filename,
    parse_ks_string,
    parse_structured_name,
    s_exps_from_string,
)
from ksc.utils import single_elem


def test_type_propagate_prelude_and_primer():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    decls_file = list(parse_ks_filename("test/ksc/syntax-primer.ks"))
    type_propagate_decls(decls_file, symtab)
    assert parse_structured_name(
        s_exps_from_string("[D [my_log Float]]", __file__)[0]
    ) in [d.name for d in decls_file]


def test_type_propagate_works(prelude_symtab):
    decls = list(
        parse_ks_string(
            """
    (def foo Float (a : Float) 1.0)
    """,
            __file__,
        )
    )
    type_propagate_decls(decls, {})
    assert decls[0].return_type == Type.Float

    decls = list(
        parse_ks_string(
            """
    (def foo Float (a : Float) (add a 1.0))
    """,
            __file__,
        )
    )
    type_propagate_decls(decls, prelude_symtab)
    assert decls[0].return_type == Type.Float


def test_type_propagate_empty_return():
    # Empty return type allowed
    decls = list(
        parse_ks_string(
            """
    (def foo None (a : Float) 1.0)
    """,
            __file__,
        )
    )
    type_propagate_decls(decls, {})
    assert decls[0].return_type == Type.Float


def test_type_propagate_warnings():
    decls = list(
        parse_ks_string(
            """
    (def foo Float (a : Float) 1)
    """,
            __file__,
        )
    )
    with pytest.raises(KSTypeError) as excinfo:
        type_propagate_decls(decls, {})
    assert "does not match declaration" in str(excinfo.value)

    decls = list(
        parse_ks_string(
            """
    (def foo Float (a : Float) 1.0)
    (edef foo Integer (Float))
    """,
            __file__,
        )
    )
    with pytest.raises(KSTypeError) as excinfo:
        type_propagate_decls(decls, {})
    assert "Double definition" in str(excinfo.value)

    decls = list(
        parse_ks_string(
            """
    (def foo Float (a : Float) 1.0)
    (def foo Integer (a : Float) 1)
    """,
            __file__,
        )
    )
    with pytest.raises(KSTypeError) as excinfo:
        type_propagate_decls(decls, {})
    assert "Redefinition of [foo Float]" in str(excinfo.value)

    decls = list(
        parse_ks_string(
            """
    (def foo Float (a : Float) 1.0)
    (def goo Integer (a : Float) (floo 2.0))
    """,
            __file__,
        )
    )
    with pytest.raises(KSTypeError) as excinfo:
        type_propagate_decls(decls, {})
    assert "Couldn't find" in str(excinfo.value)


def test_type_propagate_respect_existing():
    decls = list(
        parse_ks_string(
            "(def foo Float ((p : Bool) (x : Float)) (let (y 3.0) (if p y y)))",
            __file__,
        )
    )
    type_propagate_decls(decls, {})
    foo = single_elem(decls)
    if_node = foo.body.body
    assert isinstance(if_node, If)
    assert if_node.type_ == Type.Float

    # Make the types of the Let internally inconsistent by replacing RHS Float with Integer
    foo.body.rhs = Const(3)
    assert foo.body.rhs.type_ == Type.Integer
    # Remove type of Let and If, but leave the y's still having type Float
    foo.body.type_ = if_node.type_ = None
    type_propagate(foo, {}, respect_existing=True)
    # Types recomputed but still using old type for y:
    assert (
        foo.body.type_
        == if_node.type_
        == if_node.t_body.type_
        == if_node.f_body.type_
        == Type.Float
    )

    # Force to recompute types - the declaration is now mistyped
    with pytest.raises(KSTypeError) as excinfo:
        type_propagate(foo, {}, respect_existing=False)
    assert "inferred return type Integer" in str(excinfo.value)
    assert "does not match declaration Float" in str(excinfo.value)
    assert (
        foo.body.type_
        == if_node.type_
        == if_node.t_body.type_
        == if_node.f_body.type_
        == Type.Integer
    )


def test_type_propagate_respect_existing_does_not_traverse(prelude_symtab):
    e = parse_expr_string("(add 3.0 (mul x x))")
    mul = e.args[1]
    assert not mul.name.has_type()

    # Set the type of the mul; this prevents recursing any deeper
    e.args[1].type_ = Type.Float
    type_propagate(e, prelude_symtab, respect_existing=True)
    assert e.type_ == Type.Float  # Did something
    # But did not go inside mul...
    assert not mul.name.has_type()
    assert mul.args[0].type_ == mul.args[1].type_ == None

    with pytest.raises(KSTypeError, match="Unknown symbol x"):
        type_propagate(e, prelude_symtab, respect_existing=False)
