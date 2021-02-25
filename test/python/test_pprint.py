

#%% Test/demo code
from ksc.parse_ks import parse_expr, parse_tld, s_exps_from_string

# Importing prettyprint to get the decorated printers for Expression and Type
import ksc.prettyprint # pylint: disable=unused-import

# Import the prettyprinter routines we use explicitly in this file
from prettyprinter import cpprint, pformat

# Needed this in order to see the error messages when pprint fails
import warnings
warnings.filterwarnings("always")

def expr(s):
    return parse_expr(s_exps_from_string(s)[0])

def tld(s):
    return parse_tld(s_exps_from_string(s)[0])

def test_pprint():
    s = """
        (def [rev myfun] (Tuple Float Float)
            ((x : Float) 
             (y : Vec Integer)
             (t : (Tuple Float Float))) 
            (let (l (lam (i : Integer) (add i 1)))
            (let (b 2)
              (assert (gt b 0) 
                 (tuple 
                    (if (lt 1 b) x (add x (get$1$2 t))) 
                    (mul b (get$1$2 t)))))))
    """
    e = tld(s)
    print("-------STR:")
    print(str(e))
    print("--------PP: width=40")
    cpprint(e,width=40)
    print("--------PP: width=60")
    cpprint(e,width=60)
    print("--------PP: width=70")
    cpprint(e,width=70)
    print("--------PP: width=80")
    cpprint(e,width=80)

    print("--------PP: width=10")
    # Print to string with minimal width to stress-test
    lines = pformat(e, width=10)
    print("WIDTH10:")
    print(lines)
    e2 = tld(lines)
    # check it roundtrips
    # Roundtrip test is pretty decent -- certainly if it reverts to __repr__ it will fail
    assert e2 == e
    # check that it's not just using str, by checking for number of newlines
    num_lines = lines.count('\n')
    print('num_lines=', num_lines)
    assert num_lines == 32 # This needs to be updated after visual checking that the output is OK

    # Check roundtrip on another width
    e2 = tld(pformat(e))
    assert e2 == e

def test_rule():
    s = """
        (rule "my" ((x : Float) (y : Vec Integer))
              (add x y) (add y x))
    """
    e = tld(s)
    cpprint(e, width=60)
    e1 = tld(pformat(e))
    assert e == e1

if __name__ == "__main__":
    test_rule()
