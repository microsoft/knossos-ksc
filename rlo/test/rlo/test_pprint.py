# fmt: off

#%% Test/demo code
from rlo.expression import Expression
from rlo import sparser

# Importing prettyprint to get the decorated printers for Expression and Type
from rlo.prettyprint import cpprint, pformat, ExprWithComments

# Needed this in order to see the error messages when pprint fails
import warnings
warnings.filterwarnings("always")

def print_to_width(e, w):
    msg = "--------width=" + str(w)
    msg += "-" * (w-len(msg))
    print(msg)
    cpprint(e, width=w)

def parse_def(s):
    _name, exprenv = sparser.parse_defs(s)[-1]
    return exprenv.expr

def test_avxpavy():
    s = """    
    (def avxpavy (Vec Float)
            ((a : Float)
            (vx : (Vec Float))
            (vy : (Vec Float)))
        (let ((n (size vx))
              (x2 (build n (lam (i : Integer) 
                       (mul a (index i vx)))))
              (y2 (build n (lam (i : Integer)
                       (mul a (index i vy)))))) ; Two independent intermediates
        (build n (lam (i : Integer) (add (index i x2) (index i y2))))))
    """
    e = parse_def(s)
    print_to_width(e, 80)
    print_to_width(e, 60)
    print_to_width(e, 50)
    print_to_width(e, 40)

    e2 = parse_def(pformat(e))
    assert e2 == e

def test_multi_def():
    s = """
        (def foo Float ((x : Float) (y : Float)) (mul$ff (add$ff x y) (add$ff x 1.0)))
        (def bar Float (x : Integer) (foo (exp$f (to_float x)) (sumbuild x (lam (i : Integer) (to_float i)))))
    """

    e = parse_def(s)
    s2 = pformat(e)
    print(s2)
    e2 = parse_def(s2)
    assert e2 == e

def test_pprint():
    s = """
    (def myfun (Tuple Float Float)
            ((x : Float) (t : (Tuple Float Float)))
        (tuple
            (if (lt 1 2) x (add x (get$1$2 t)))
            (add x (get$2$2 t))))
    """
    e = parse_def(s)
    print("-------STR:")
    print(str(e))
    print_to_width(e, 40)
    print_to_width(e, 60)
    print_to_width(e, 70)
    print_to_width(e, 80)

    # Print to string with minimal width to stress-test
    lines = pformat(e, width=10)
    print("LINES[40]:")
    print(lines)
    e2 = parse_def(lines)
    # check it roundtrips
    # Roundtrip test is pretty decent -- certainly if it reverts to __repr__ it will fail
    assert e2 == e
    # check that it's not just using str, by checking for number of newlines
    num_lines = lines.count('\n')
    print('num_lines=', num_lines)
    assert num_lines == 22 # This needs to be updated after visual checking that the output is OK

    # Check roundtrip on another width
    e2 = parse_def(pformat(e))
    assert e2 == e

def test_comments_same_object():
    x = Expression.Variable("x")
    e = x * x
    assert pformat(e) == "(mul x x)"
    assert e.nodes[1] is e.nodes[2] # The two occurrences of x are the same object
    assert pformat(ExprWithComments(e, {1: "foo", 2: "bar"})) == "(mul #|foo|#x #|bar|#x)"

def test_comments_apply():
    e = parse_def("""
    (edef foo Float (Float))
    (def myfun Float (x : Float) (mul$ff (foo x) (foo (add$ff x 1.0))))
    """)
    s = pformat(e)
    assert s.count("(foo x)") == 1

    assert e.nodes[7] == sparser.parse_expr("(foo x)")
    s_comments = pformat(ExprWithComments(e, {7: "[call]", 8: "<func>"}))

    # The (foo (add$ff x 1.0)) should not be affected
    assert s.replace("(foo x)", "#|[call]|#(#|<func>|#foo x)") == s_comments

if __name__ == "__main__":
    test_pprint()
