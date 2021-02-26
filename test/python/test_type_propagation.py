import pytest

from ksc.type import Type, KSTypeError
from ksc.type_propagate import type_propagate_decls
from ksc.parse_ks import parse_ks_filename, parse_ks_string
from ksc.expr import StructuredName

def test_type_propagate_prelude_and_primer():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    decls_file = list(parse_ks_filename("test/ksc/syntax-primer.ks"))
    type_propagate_decls(decls_file, symtab)
    assert "Dt$my_log" in [d.name.mangled() for d in decls_file]

def test_type_propagate_works():
    decls = list(parse_ks_string("""
    (def foo Float (a : Float) 1.0)
    """, __file__))
    type_propagate_decls(decls, {})
    assert decls[0].return_type == Type.Float

    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    decls = list(parse_ks_string("""
    (def foo Float (a : Float) (add a 1.0))
    """, __file__))
    type_propagate_decls(decls, symtab)
    assert decls[0].return_type == Type.Float

def test_type_propagate_empty_return():
    # Empty return type allowed
    decls = list(parse_ks_string("""
    (def foo None (a : Float) 1.0)
    """, __file__))
    type_propagate_decls(decls, {})
    assert decls[0].return_type == Type.Float

def test_type_propagate_warnings():
    decls = list(parse_ks_string("""
    (def foo Float (a : Float) 1)
    """, __file__))
    with pytest.raises(KSTypeError) as excinfo:
        type_propagate_decls(decls, {})
    assert "does not match declaration" in str(excinfo.value)

    decls = list(parse_ks_string("""
    (def foo Float (a : Float) 1.0)
    (edef foo Integer (Float))
    """, __file__))
    with pytest.raises(KSTypeError) as excinfo:
        type_propagate_decls(decls, {})
    assert "Double definition" in str(excinfo.value)

    decls = list(parse_ks_string("""
    (def foo Float (a : Float) 1.0)
    (def foo Integer (a : Float) 1)
    """, __file__))
    with pytest.raises(KSTypeError) as excinfo:
        type_propagate_decls(decls, {})
    assert "Redefinition of [foo Float]" in str(excinfo.value)
