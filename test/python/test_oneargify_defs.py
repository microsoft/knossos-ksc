from ksc.oneargify_defs import oneargify_def, oneargify_defs
from ksc.parse_ks import parse_ks_file
from ksc.utils import single_elem
from ksc.type_propagate import type_propagate, type_propagate_decls


def test_oneargify_def(prelude_symtab):
    d, expected = list(
        parse_ks_file(
            """
(def foo Float ((a : Float) (b : Float)) (mul (add a 2.0) (add b 3.0)))
(def foo Float (func_arg_0 : Tuple Float Float)
    (let (a (get$1$2 func_arg_0))
       (let (b (get$2$2 func_arg_0))
           (mul (add a 2.0) (add b 3.0)))))
    """
        )
    )
    type_propagate(d, prelude_symtab)
    actual = oneargify_def(d)
    type_propagate(expected, prelude_symtab)
    assert actual == expected


def test_oneargify_defs(prelude_symtab):
    decls = list(
        parse_ks_file(
            """
(edef foo Float (Tuple Float Float))
(def inc Integer (x : Integer) (add x 1))
(def rev (Tuple Float Float) (x : Tuple Float Float) (tuple (get$2$2 x) (get$1$2 x)))
(rule "rev_of_rev" (in : Tuple Float Float) (rev (rev in)) in)
(def twoarg Float ((a : Float) (b : Float)) (foo a b))
"""
        )
    )
    expected_twoarg = single_elem(
        list(
            parse_ks_file(
                """
(def twoarg Float (func_arg_0 : Tuple Float Float)
    (let (a (get$1$2 func_arg_0))
        (let (b (get$2$2 func_arg_0))
            (foo a b))))
    """
            )
        )
    )
    type_propagate_decls(decls, prelude_symtab)
    type_propagate(expected_twoarg, prelude_symtab)
    # Check edef + rule are preserved
    assert oneargify_defs(decls) == decls[:-1] + [expected_twoarg]
