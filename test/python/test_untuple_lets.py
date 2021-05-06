from ksc.untuple_lets import untuple_lets
from ksc.parse_ks import parse_expr_string


def test_untupling():
    actual = untuple_lets(parse_expr_string("(let ((a b) (foo x)) (add a b))"))
    expected = parse_expr_string(
        "(let (temp_0 (foo x)) (let (a (get$1$2 temp_0)) (let (b (get$2$2 temp_0)) (add a b))))"
    )
    assert untuple_lets(actual) == expected
    assert untuple_lets(expected) == expected


def test_untupling2():
    actual = untuple_lets(parse_expr_string("(let ((a b) (if p (let ((a b c) (foo x)) (tuple b a)) (bar x))) a)"))
    expected = parse_expr_string(
        """(let (temp_0 (if p 
        (let (temp_0 (foo x)) (let (a (get$1$3 temp_0)) (let (b (get$2$3 temp_0)) (let (c (get$3$3 temp_0)) (tuple b a)))))
        (bar x)))
    (let (a (get$1$2 temp_0)) (let (b (get$2$2 temp_0)) a)))"""
    )
    assert untuple_lets(actual) == expected
    assert untuple_lets(expected) == expected


def test_untupling_avoids_capture():
    def test_with_var_name(v: str):
        actual = untuple_lets(parse_expr_string(f"(let ((a b) (foo {v})) (add a b))"))
        gen_name = actual.vars.name
        assert gen_name != v
        expected = parse_expr_string(
            f"(let ({gen_name} (foo {v})) (let (a (get$1$2 {gen_name})) (let (b (get$2$2 {gen_name})) (add a b))))"
        )
        assert actual == expected
        return gen_name

    gen_name = test_with_var_name("x")
    test_with_var_name(gen_name)


def test_untupling_tuple_val():
    actual = untuple_lets(parse_expr_string("(let ((a b) (tuple (foo x) (bar x))) (add a b))"))
    expected = parse_expr_string("(let (a (foo x)) (let (b (bar x)) (add a b)))")
    assert untuple_lets(actual) == expected
    assert untuple_lets(expected) == expected
