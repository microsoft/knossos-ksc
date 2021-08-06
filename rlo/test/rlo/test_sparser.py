# fmt: off
import os
import glob
import pytest

from rlo import costs
from rlo.expression import Expression, EF
from rlo.expression_util import NamedExprWithEnv, ExprWithEnv
from rlo.expr_sets import ExpressionSetFromFile
from rlo import rewrites
from rlo import sparser
from rlo.utils import read_file, get_func_body, single_elem
from ksc.type import Type
from testutils import make_toplevel as MT

FOLDER = os.path.dirname(os.path.abspath(__file__))

def _parse_defs_no_symtab(ks_str):
    exprs, _ = sparser.parse_defs_with_symtab(ks_str)
    return exprs

def parse_defs_folder_file(filename, **kwargs):
    return _parse_defs_no_symtab(read_file(os.path.join(FOLDER, filename)), **kwargs)

def load_expression_set(filename):
    return ExpressionSetFromFile(os.path.join(FOLDER, filename))

def test_s_expression_parser():
    a = Expression.Variable("a")
    b = Expression.Variable("b")
    assert sparser.parse_expr("(sub (mul a b ) 3)") == a * b - 3.0
    assert sparser.parse_expr("(let a (div b 2) (add a 4))") == EF.Let(a, b / 2.0, a + 4.0)
    assert sparser.parse_expr("(lt a b)") == (b > a)
    assert sparser.parse_expr("(get$3$3 (tuple a b a))") == EF.Select(EF.Tuple(a,b,a), 2) # ksc-indices are 1-based, we use 0-based
    assert sparser.parse_expr("(neg a)") == (Expression.Constant(0.0) - a)

def test_s_expression_parser_error():
    with pytest.raises(AssertionError):
        sparser.parse_expr("(get$0$2 t)")
    with pytest.raises(AssertionError):
        sparser.parse_expr("(get$3$2 t)")

def test_s_expression_lambda():
    x = Expression.Variable("x", Type.Float)
    inc_fn = EF.Lam(x, x + 1.0)
    assert sparser.parse_expr("(lam (x : Float) (add x 1.0))") == inc_fn
    assert sparser.parse_expr("(lam (x : (Vec Float)) x)") != sparser.parse_expr("(lam (y : Float) y)")

    # Lambdas must have one argument, possibly of tuple type
    with pytest.raises(ValueError):
        sparser.parse_expr("(lam (x : Float) (y: Float) body)")
    with pytest.raises(ValueError):
        sparser.parse_expr("(lam ((x : Float) (y: Float)) body)")

    assert sparser.parse_expr("(let f (lam (x : Float) (add x 1.0)) (f 2.0))") == EF.Let(
        "f", inc_fn, EF.Apply("f", 2.0))

    assert sparser.parse_expr("(let f (lam (t : (Tuple Integer Float)) (get$1$2 t)) (f a b))") == EF.Let(
        "f",
        EF.Lam(Expression.Variable("x", Type.Tuple(Type.Integer, Type.Float)), EF.Select("x", 0)),
        EF.Apply("f", EF.Tuple("a", "b")))

def check_can_rewrite(start, finish, rules):
    rewritten = [rewrite.apply(start) for rewrite in rules.get_all_rewrites(start)]
    assert any([r == finish for r in rewritten])

def test_lets():
    a = Expression.Variable("a")
    b = Expression.Variable("b")

    step0 = sparser.parse_expr("(let ((a (mul 2 3)) (b (div 1 2))) (add a b))") #variables are independent
    check0 = EF.Let(a, Expression.Constant(2) * 3, EF.Let(b, Expression.Constant(1) / 2, a + b))
    assert step0 == check0

    step1 = sparser.parse_expr("(let ((a (mul 2 3)) (b (div a 2))) (add a b))") #variables are defined in increasing order of dependencies
    check1 = EF.Let(a, Expression.Constant(2) * 3, EF.Let(b, a / 2, a + b))
    assert step1 == check1

    step2 = sparser.parse_expr("(let (a (mul 2 3)) (add a 1))")
    check2 = EF.Let(a, Expression.Constant(2) * 3, a + 1)
    assert step2 == check2

def test_def_single_line():

    a = Expression.Variable("a")
    x = Expression.Variable("x", Type.Float)

    assert _parse_defs_no_symtab("(def a Float ((x : Float)) (mul 3.0 x))") == [
        ("a", EF.Let(a, EF.Lam(x, 3.0 * x), a))]

def test_def_simple_dependency():

    a = Expression.Variable("a")
    b = Expression.Variable("b")
    c = Expression.Variable("c")
    d = Expression.Variable("d")
    f = Expression.Variable("f")

    x = Expression.Variable("x", Type.Float)
    z = Expression.Variable("z", Type.Float)
    w = Expression.Variable("w", Type.Float)

    assert _parse_defs_no_symtab("(def f Float ((x : Float)) (x))") == [
        ("f", EF.Let(f, EF.Lam(x, x), f))]

    f_a = EF.Lam(x, 3.0 * x)
    f_b = EF.Lam(x, 2.0 - EF.Apply(a, x))
    f_c = EF.Lam(z, 4.0 + EF.Apply(b, z))
    f_d = EF.Lam(w, EF.Apply(c, w))
    # check step by step parsing of simple depepndencies (dependency on one function)
    assert parse_defs_folder_file("test_sparser_files/simple_dependency.kso") == [
        ("a", EF.Let(a, f_a, a)),
        ("b", EF.Let(a, f_a, EF.Let(b, f_b, b))),
        ("c", EF.Let(a, f_a, EF.Let(b, f_b, EF.Let(c, f_c, c)))),
        ("d", EF.Let(a, f_a, EF.Let(b, f_b, EF.Let(c, f_c, EF.Let(d, f_d, d)))))]

def test_def_harder_dependency():
    import harder_dependency as f

    assert parse_defs_folder_file("test_sparser_files/harder_dependency.kso") == [
        ("a", EF.Let(f.a, f.f_a, f.a)),
        # note here that "a" isn't used, so hopefully (Alan says) RLO will easily remove it
        # and simplify to EF.Let(b, EF.Lam(x, 1.0 / (x * x)), b), or even just the EF.Lam.
        # Hence the below is acceptable from the parser; and similarly for other examples below
        ("b", EF.Let(f.a, f.f_a, EF.Let(f.b, f.f_b, f.b))),
        ("c", EF.Let(f.a, f.f_a, EF.Let(f.b, f.f_b, EF.Let(f.c, f.f_c, f.c)))),
        ("d", EF.Let(f.a, f.f_a, EF.Let(f.b, f.f_b, EF.Let(f.c, f.f_c, EF.Let(f.d, f.f_d, f.d))))),
        ("e", EF.Let(f.a, f.f_a, EF.Let(f.b, f.f_b, EF.Let(f.c, f.f_c, EF.Let(f.d, f.f_d, EF.Let(f.e, f.f_e, f.e)))))),
        # note here the obvious dead-code-elimination to EF.Let(f, EF.Lam(x, x / (x * x)), f)
        ("f", EF.Let(f.a, f.f_a, EF.Let(f.b, f.f_b, EF.Let(f.c, f.f_c, EF.Let(f.d, f.f_d, EF.Let(f.e, f.f_e, EF.Let(f.f, f.f_f, f.f)))))))]

def test_def_assertions():
    with pytest.raises(ValueError): #No standalone exp in mult lined files
        parse_defs_folder_file("test_sparser_files/assertions/assertion_alone_exp.kso")

def test_def_simple_dependency_tuples():

    a = Expression.Variable("a")
    b = Expression.Variable("b")
    c = Expression.Variable("c")

    x = Expression.Variable("x", Type.Float)
    y = Expression.Variable("y", Type.Float)
    t1 = Expression.Variable("t1", Type.Tuple(Type.Float, Type.Float))

    f_a = EF.Lam(t1, EF.Let(y, EF.Select(t1, 1), EF.Let(x, EF.Select(t1, 0), (x * y))))
    f_b = EF.Lam(t1, EF.Let(y, EF.Select(t1, 1), EF.Let(x, EF.Select(t1, 0), (2.0 - EF.Apply(a, EF.Tuple(x, y))))))
    # check step by step parsing of simple dependencies (dependency on one function)
    assert parse_defs_folder_file("test_sparser_files/simple_dependency_tuples.kso") == [
        ("a", EF.Let(a, f_a, a)),
        ("b", EF.Let(a, f_a, EF.Let(b, f_b, b)))]

    assert _parse_defs_no_symtab("(def c Float ((y : Float) (x : Float)) (add x x))") == [
        ("c", EF.Let(c, EF.Lam(t1, EF.Let(x, EF.Select(t1, 1), EF.Let(y, EF.Select(t1, 0), (x + x)))), c))]

    with pytest.raises(ValueError): # Wrong tuple format
        sparser.parse_defs("(def f Float ((x : Float) (y)) (mul x y))")

def test_def_harder_dependency_tuples():

    a = Expression.Variable("a")
    b = Expression.Variable("b")
    c = Expression.Variable("c")
    d = Expression.Variable("d")

    x = Expression.Variable("x", Type.Float)
    y = Expression.Variable("y", Type.Float)
    tff = Type.Tuple(Type.Float, Type.Float)
    t1 = Expression.Variable("t1", tff)
    t2 = Expression.Variable("t2", tff)
    t3 = Expression.Variable("t3", tff)

    res = dict(parse_defs_folder_file("test_sparser_files/harder_dependency_tuples.kso"))
    c_body = EF.Lam(t1, EF.Let(y, EF.Select(t1, 1), EF.Let(x, EF.Select(t1, 0), ((2.0 * EF.Apply(b, EF.Tuple(x, y))) - (EF.Apply(a, EF.Tuple(y, x)) + 3.0)))))
    b_body = EF.Lam(t2, EF.Let(y, EF.Select(t2, 1), EF.Let(x, EF.Select(t2, 0), (1.0 / (y * x)))))
    a_body = EF.Lam(t3, EF.Let(y, EF.Select(t3, 1), EF.Let(x, EF.Select(t3, 0), (x * y))))
    assert res["c"] == EF.Let(a, a_body, EF.Let(b, b_body, EF.Let(c, c_body, c)))

    d_body = EF.Lam(t1, EF.Let(y, EF.Select(t1, 1), EF.Let(x, EF.Select(t1, 0), (4.0 + EF.Apply(b,  EF.Tuple(x, (2.0 * EF.Apply(a, EF.Tuple(x, y)))))))))
    assert res["d"] == EF.Let(a, a_body, EF.Let(b, b_body, EF.Let(c, c_body, EF.Let(d, d_body, d))))

def test_def_args_syntax():

    d = Expression.Variable("d")
    x = Expression.Variable("x", Type.Float)
    z = Expression.Variable("z", Type.Tensor(1,Type.Float))
    t1 = Expression.Variable("t1", Type.Tuple(x.type, z.type))

    step0_2 = _parse_defs_no_symtab("(def d (Vec Float) ((x : Float) (z : (Vec Float))) (div z x))") # this is just to check parsing (not semantically correct)
    check0 = EF.Lam(t1, EF.Let(z, EF.Select(t1,1), EF.Let(x, EF.Select(t1,0), (z / x))))
    assert step0_2 == [("d", EF.Let(d, check0, d))]

    short_type_brackets = _parse_defs_no_symtab("(def d (Vec Float) ((x : Float)) (build 10 (lam (i : Integer) (div x x))))")
    short_type_no_brackets = _parse_defs_no_symtab("(def d (Vec Float) (x : Float) (build 10 (lam (i : Integer) (div x x))))")
    expected_short_type = [("d", EF.Let(d, EF.Lam(x, EF.Build(10, "i", x / x)), d))]
    assert short_type_brackets == expected_short_type
    assert short_type_no_brackets == expected_short_type

    long_type = _parse_defs_no_symtab("(def d (Vec Float) (x : Vec Float) (div x x))")
    long_type_brackets = _parse_defs_no_symtab("(def d (Vec Float) (x : (Vec Float)) (div x x))")
    brackets_long_type = _parse_defs_no_symtab("(def d (Vec Float) ((x : Vec Float)) (div x x))")
    brackets_long_type_brackets = _parse_defs_no_symtab("(def d (Vec Float) ((x : (Vec Float))) (div x x))")
    expected_type = Type.Lam(Type.Tensor(1,Type.Float), Type.Tensor(1,Type.Float))

    def get_type(defs):
        return get_func_body("d", single_elem(defs)[1]).type

    assert get_type(long_type) == expected_type
    assert get_type(long_type_brackets) == expected_type
    assert get_type(brackets_long_type) == expected_type
    assert get_type(brackets_long_type_brackets) == expected_type


def test_def_body_syntax():

    d = Expression.Variable("d")
    x = Expression.Variable("x", Type.Tensor(1,Type.Float))

    const_brackets = _parse_defs_no_symtab("(def d (Float) (x : Vec Float) (0.0))")
    const_no_brackets = _parse_defs_no_symtab("(def d (Float) (x : Vec Float) 0.0)")
    expected_const = EF.Lam(x, 0.0)
    assert const_brackets == [("d", EF.Let(d, expected_const, d))]
    assert const_no_brackets == [("d", EF.Let(d, expected_const, d))]

    var_brackets = _parse_defs_no_symtab("(def d (Vec Float) (x : Vec Float) (x))")
    var_no_brackets = _parse_defs_no_symtab("(def d (Vec Float) (x : Vec Float) x)")
    expected_fn = EF.Lam(x,  x)
    assert var_brackets == [("d", EF.Let(d, expected_fn, d))]
    assert var_no_brackets == [("d", EF.Let(d, expected_fn, d))]

    with pytest.raises(ValueError): # Wrong body format
        sparser.parse_defs("(def d (Vec Float) (x : Vec Float) * x x)")

    with pytest.raises(ValueError): # Wrong body format
        sparser.parse_defs("(def d (Vec Float) (x : Vec Float) ())")

def test_get_expressions():
    import harder_dependency as f

    func_list_1 = load_expression_set("../../test/rlo/test_sparser_files/harder_dependency.kso").named_exprenvs()
    func_list_check_1 = [("a", EF.Let(f.a, f.f_a, f.a)),
                        ("b", EF.Let(f.b, f.f_b, f.b)),
                        ("c", EF.Let(f.a, f.f_a, EF.Let(f.b, f.f_b, EF.Let(f.c, f.f_c, f.c)))),
                        ("d", EF.Let(f.a, f.f_a, EF.Let(f.b, f.f_b, EF.Let(f.d, f.f_d, f.d)))),
                        ("e", EF.Let(f.a, f.f_a, EF.Let(f.b, f.f_b, EF.Let(f.c, f.f_c, EF.Let(f.d, f.f_d, EF.Let(f.e, f.f_e, f.e)))))),
                        ("f", EF.Let(f.f, f.f_f, f.f))]

    for func, (expected_name, expected_body) in zip(func_list_1, func_list_check_1):
        assert func == NamedExprWithEnv(expected_name, MT(expected_body))


def test_type_in_get_expressions():
    ksc_files_train = glob.glob(os.path.join(FOLDER, "../../src/rlo/ksc/*/*_train.kso"))
    ksc_files_test = glob.glob(os.path.join(FOLDER, "../../src/rlo/ksc/*/*_test.kso"))
    ksc_files = ksc_files_train + ksc_files_test
    assert len(ksc_files) > 0

def test_edef():
    name, e = single_elem(sparser.parse_defs("""
        (edef foo Float ((Vec Float) Float))
        (def cost$foo Float ((a : Vec Float) (b : Float)) 8.0)
        (def bar Float (a : Vec Float) (foo a (index 0 a)))"""))
    assert name == "bar"
    assert e.expr.op == "let" and e.expr.first.name == "bar"
    a = Expression.Variable("a", Type.Tensor(1,Type.Float))
    assert e.expr == EF.Let("bar", EF.Lam(a, EF.Apply("foo", EF.Tuple(a, EF.Index(0, a)))), "bar")
    assert e.expr.type == Type.Lam(Type.Tensor(1,Type.Float), Type.Float)
    assert e.cost() == 8.0 + costs.apply_cost + costs.let_cost + MT(EF.Tuple(a, EF.Index(0, a))).cost() # cost() requires that the Expression passes type-checking

def test_edef_in_symtab():
    _defs, symtab_and_defs = sparser.parse_defs_with_symtab(
        "(edef foo Integer (Tuple Float Float))\n\n(def bar Integer (x : Float) (foo x x))")
    entry = symtab_and_defs.symtab["foo"]
    assert entry == Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Integer)

def test_illegal_variables():
    with pytest.raises(Exception):
        sparser.parse_expr("(x / x)")
    with pytest.raises(Exception):
        sparser.parse_expr("x:")
    with pytest.raises(Exception):
        sparser.parse_expr("(div build x)")

    with pytest.raises(Exception):
        sparser.parse_expr("(f let)")
    sparser.parse_expr("(f x)") # ok

    with pytest.raises(Exception):
        sparser.parse_expr("(div 99x x)")
    sparser.parse_expr("(div x99 x)") #ok

def test_roundtrip_str():
    # Get a reasonable corpus
    from rlo import best_results
    exprenvs = [expr for seqs in best_results._best_results.values() for (seq, _) in seqs for expr in seq]
    # Now test roundtrip on each of those exprs
    for exprenv in exprenvs:
        # First that output-then-parse gives us back something that's Expression.== (alpha-conversion etc.)
        s = str(exprenv.expr)
        # Free variable types are lost in serialization but it is ok for this test
        e2 = sparser.parse_expr(s)
        assert e2 == exprenv.expr
        # Then that parse-then-output gives the same string
        # (This is a stronger test, e.g. that there has been no alpha-conversion)
        s2 = str(e2)
        assert s == s2

@pytest.mark.notquick
def test_roundtrip_ksc_str():
    # This tests that sparser.parse_defs and Expression.ksc_str are inverses.
    # It makes only very minor checks that the output (string) point in that roundtrip is what we expect.
    # ksc_str isn't generally applicable, it works only on chain of lets+lambdas that can be output to a def.
    test_sparser_files = glob.glob(os.path.join(FOLDER, "test_sparser_files/*.kso"))
    assert len(test_sparser_files) > 0
    ksc_files_train = glob.glob(os.path.join(FOLDER, "../../src/rlo/ksc/*/*_train.kso"))
    ksc_files_test = glob.glob(os.path.join(FOLDER, "../../src/rlo/ksc/*/*_test.kso"))
    blas_combined_path = os.path.join(FOLDER, "../../src/rlo/ksc/blas/blas_combined.kso")
    ksc_files = ksc_files_train + ksc_files_test + [blas_combined_path]
    assert len(ksc_files) > 0

    for file_path in test_sparser_files + ksc_files:
        print("Processing {}".format(file_path))
        symtab_and_defs = None
        def parse(s):
            nonlocal symtab_and_defs
            exprs, symtab_and_defs = sparser.parse_defs_with_symtab(s, symtab_and_defs)
            return [(n, rewrites.delete_unused_defs(ExprWithEnv(e, symtab_and_defs))) for n,e in exprs]

        for (func_name, exprenv) in parse(read_file(file_path)):
            cost = exprenv.cost()
            type = exprenv.expr.type
            assert cost > 0.1
            assert type.lam_return_type is not None
            s2 = exprenv.expr.ksc_str()
            # Test on the string. We don't expect s2 == s in this general case,
            # as s contains e.g. comments, formatting/indenting, etc. (and may need alpha-renaming).
            # Here we check only the absence of lambdas; it's quite hard to check there are no (let ... lam), what with variable names and possibly types,
            # instead, we expect one lam inside each build or sumbuild.
            assert s2.count("lam") == s2.count("build")

            # Finish the roundtrip test
            fn2, exprenv2 = parse(s2)[-1]
            assert fn2 == func_name
            assert exprenv == exprenv2

            # Our parsing (of defs) generates new variables (generally from inside out, i.e. we build
            # the innermost "let f = ... in f" first), which are then alpha-renamed as the expression
            # is put together.
            # When we remove these synthetic variables in .ksc_str(), they leave "gaps" in the variable naming scheme in the output,
            # meaning that variable name generation and alpha-renaming proceeds differently when we re-parse that output.
            # Thus, to reach string equality, we iterate until a fixpoint is reached.
            s3 = exprenv2.expr.ksc_str()
            while s2 != s3:
                print("ITER {}".format(func_name))
                s2 = s3
                fn3, exprenv3 = parse(s3)[-1]
                assert fn3 == func_name
                assert exprenv3 == exprenv2
                s3 = exprenv3.expr.ksc_str()

def test_ksc_exact_roundtrip_normalization():
    # Multiple elements => all bar the last will become the last (which roundtrips)
    for testset in [
        ["(def f Float ((x : Float)) (add x 1.0))", "(def f Float ((x : Float)) (add x 1.0))\n\n"],
        ["(def f Float ((x : Float)) (add x 1.0))\n\n(def g Float ((y : Float)) (add (f y) (f (mul y 2.0))))\n\n"],
        ["(def f Float (x : Float) (if (lt x 1.0) 1.0 x))", "(def f Float (x : Float) (if (lt x 1.0) 1.0 x))", "(def f Float (x : Float) (if (gt 1.0 x) 1.0 x))", "(def f Float ((x : Float)) (if (gt 1.0 x) 1.0 x))\n\n"],
        ["(def f Float ((x : Float)) (x))\n\n"],
        ["(def f Float ((x : Float)) (let y (add x 1.0) y))", "(def f Float ((x : Float)) (let ((y (add x 1.0))) y))",
            "(def f Float ((x : Float)) (let (y (add x 1.0)) y))\n\n"],
        ["(def f Float ((x : Float)) (let ((y (add x 1.0)) (z (mul y 2.0))) y))", "(def f Float ((x : Float)) (let (y (add x 1.0)) (let (z (mul y 2.0)) y)))\n\n"],
        # This will not be converted to multi-argument form because the body is not of the correct shape.
        ["(def sum$TupFF Float ((t : (Tuple Float Float))) (add (get$1$2 t) (get$2$2 t)))\n\n"],
        # Multiple arguments - ok to convert tuple to multi-arg form:
        ["(def f Float (t : Tuple Float Float) (let ((y (get$2$2 t)) (x (get$1$2 t))) (add x y)))",
             "(def f Float ((x : Float) (y : Float)) (add x y))\n\n"],
        ["(def add$F$TupFF (Tuple Float Float) ((x : Float) (t : (Tuple Float Float))) (tuple (add x (get$1$2 t)) (add x (get$2$2 t))))\n\n"],
        # A vector of empty tuples - yes, note Tuple must be in brackets.
        ["(def len Integer ((v : (Tensor 1 (Tuple)))) (size v))\n\n"]
    ]:
        for testcase in testset:
            (_, exprenv) = sparser.parse_defs(testcase)[-1]
            assert exprenv.expr.ksc_str() == testset[-1]

@pytest.mark.skip
def test_ksc_exact_roundtrip_normalization_edefs():
    # TODO should `edef`s roundtrip?
    for testset in [
        ["(edef foo Integer (Tuple Float Float))\n\n(def bar Integer (x : Float) (foo x x))",
            "(edef foo Integer ((Tuple Float Float)))\n\n(def bar Integer (x : Float) (foo x x))",
            "(edef foo Integer (Float Float))\n\n(def bar Integer ((x : Float)) (foo x x))",
            "(edef foo Integer (Float Float))\n\n(def bar Integer ((x : Float)) (foo x x))\n\n"]
    ]:
        for testcase in testset:
            # pylint: disable=unexpected-keyword-arg
            (_, exp) = sparser.parse_defs(testcase, include_edefs=True)[-1]
            assert exp.ksc_str() == testset[-1]

def test_get_func_body_roundtrip():
    import harder_dependency as f
    func_list = load_expression_set("../../test/rlo/test_sparser_files/harder_dependency.kso")
    func_list_check = [EF.Let(f.a, f.f_a, f.a), EF.Let(f.b, f.f_b, f.b), EF.Let(f.c, f.f_c, f.c), EF.Let(f.d, f.f_d, f.d), EF.Let(f.e, f.f_e, f.e), EF.Let(f.f, f.f_f, f.f)]

    for (func_name, functab), func_check in zip(func_list.named_exprenvs(), func_list_check):
        expr_lam = get_func_body(func_name, functab.expr)

        expr_name = Expression.Variable(func_name)
        assert EF.Let(expr_name, expr_lam, expr_name) == func_check
