#####################################################################
# parse_ks: Convert s-expressions to Expr
import sexpdata

from ksc.type import Type
from ksc.utils import ensure_list_of_lists
from ksc.expr import Def, EDef, Rule, Const, Var, Lam, Call, Let, If, Assert
from ksc.expr import pystr

# Pretty printing
# Importing prettyprint to get the decorated printers for Expression and Type
import ksc.prettyprint # pylint: disable=unused-import

# Import the prettyprinter routines we use explicitly in this file
from prettyprinter import cpprint, pprint, pformat

# Needed this in order to see the error messages when pprint fails
import warnings
warnings.filterwarnings("always")



#####################################################################
## S-expression Utils

# Parse a fixed-length s-exp into a list given a set of parsers, e.g.
# parse_seq(se, parse_name, parse_int, parse_int)
# would accept (fred 3 4)
def parse_seq(se, *parsers):
    if len(se) != len(parsers):
        raise ParseError("Cannot parse ", se, " with ", parsers)

    return [parser(term) for (parser, term) in zip(parsers, se)]

# Exception for parse errors
class ParseError(Exception):
    pass

# Raise ParseError if cond not true
def check(cond, *message):
    if not cond:
        raise ParseError(*message)

# Reserved word constants
_def = sexpdata.Symbol("def")
_edef = sexpdata.Symbol("edef")
_assert = sexpdata.Symbol("assert")
_if = sexpdata.Symbol("if")
_let = sexpdata.Symbol("let")
_lam = sexpdata.Symbol("lam")
_tuple = sexpdata.Symbol("tuple")
_rule = sexpdata.Symbol("rule")
_colon = sexpdata.Symbol(":")

def parse_type(se, allow_implicit_tuple=False):
    """ Converts an S-Expression representing a type, like (Vec Float) or (Tuple Float (Vec Float)),
        into a Type object, e.g. Type.Vec(Type.Float) or Type.Tuple(Type.Float, Type.Vec(Type.Float)).

        If allow_implicit_tuple is true, also converts a list of types into a Tuple, e.g.
        (Float (Vec Float)) becomes Type.Tuple(Type.Float, Type.Vec(Type.Float)), i.e. as if
        the S-Expression began with an extra "Tuple".
    """
    while isinstance(se, list) and len(se)==1:
        se=se[0] # Discard ((pointless)) brackets
    if isinstance(se, sexpdata.Symbol):
        return Type(se.value())
    if isinstance(se, list) and len(se)>0:
        if isinstance(se[0], sexpdata.Symbol):
            sym = se[0].value()
            children = [parse_type(s, allow_implicit_tuple) for s in se[1:]]
            if sym == "Vec" and len(se)==2:
                return Type.Vec(*children)
            if sym == "Tuple":
                return Type.Tuple(*children)
            if sym == "Lam":
                return Type.Lam(*children)
            if sym == "LM":
                return Type.LM(*children)
            # Fall through in case it's a list of types with allow_implicit_tuple.
        if allow_implicit_tuple:
            return Type.Tuple(*[parse_type(s, allow_implicit_tuple) for s in se])
    raise ValueError("Did not know how to parse type {}".format(se))

def parse_types(ses):
    return list(map(parse_type, ses))

# "x" -> string
def parse_name(se):
    check(isinstance(se, sexpdata.Symbol), "Wanted identifier, got: ", se)
    return se.value()

# "\"x\"" -> string
def parse_string(se):
    check(isinstance(se, str), "Expected string, got: ", se) 
    return se

# "x : Float" -> Var(x, Type.Float)
def parse_arg(arg):
    check(len(arg) >= 3, "Expect (arg : type), not: ", arg)
    check(arg[1] == _colon, "No colon: ", arg)

    return Var(parse_name(arg[0]), parse_type(arg[2:]), True)

# "((x : Float) (y : Integer))" -> [Var("x", Type.Float), Var("y", Type.Integer)]
def parse_args(se):
    return [parse_arg(arg) for arg in ensure_list_of_lists(se)]

def parse_expr(se):
    # "x" -> always a variable
    if isinstance(se, sexpdata.Symbol):
        return Var(se.value(), None, False)

    # "1.2", "1", "\"string\"""
    if not isinstance(se, list):
        return Const(se)

    # Remaining forms are lists

    # Empty lists one should not occur.
    check(len(se) > 0, "Empty list")
    check(len(se) > 1 or se[0] == _tuple, "Singleton list other than (tuple): ", se)

    head = se[0]

    # TODO: It is tempting to always strip ((redundant)) parens, but in general a
    # profusion of such defenses can hide carbuncles elsewhere
    # Nested exp e.g. ((sin 5)) -> (sin 5)
    if len(se) == 1 and head != _tuple:
        return parse_expr(head)

    # List-of-const literals (1 2 3 4)
    # TODO: ksc does not allow this, so either add in ksc, or rm here
    #  Ideally it is best not to add such sugar unless it significantly 
    #  improves efficiency/readability. This feels the wrong side, as it 
    #  just replaces (tuple 1 2 3) with (1 2 3)  
    if True:
        if isinstance(head, (int, float)):
            check(all(isinstance(se, type(head)) for se in se), "Constant tuple should be all the same type: ", se)
            return [v for v in se]

    # If(cond, t, f)
    if head == _if:
        return If(*parse_seq(se[1:], parse_expr, parse_expr, parse_expr))

    # Assert(cond, body)
    if head == _assert:
        return Assert(*parse_seq(se[1:], parse_expr, parse_expr))

    # Let(var, rhs, body)
    if head == _let:
        bindings = ensure_list_of_lists(se[1])
        ans = parse_expr(se[2])
        for b in bindings[::-1]:
            check(len(b) == 2, "Let bindings should be pairs", b, "in", se)
            # TODO: bindings could be tupled
            var = Var(parse_name(b[0]), None, False)
            rhs = parse_expr(b[1])
            ans = Let(var, rhs, ans)
        return ans

    # Lam(var, type, body)
    if head == _lam:
        var = parse_arg(se[1])
        body = parse_expr(se[2])
        return Lam(var, body)

    # The remainder are calls
    return Call(parse_name(head), [parse_expr(se) for se in se[1:]])

# Parse a top-level definition (def, edef, rule)
def parse_tld(se):
    check(isinstance(se, list), "Non-list at top level", se)
    check(len(se) > 0, "Empty list at top level")
    head = se[0]
    if head == _def:
        return Def(*parse_seq(se[1:], parse_name, parse_type, parse_args, parse_expr))

    if head == _edef:
        return EDef(*parse_seq(se[1:], parse_name, parse_type, parse_types))

    if head == _rule:
        return Rule(*parse_seq(se[1:], parse_string, parse_args, parse_expr, parse_expr))

    raise ParseError("unrecognised top-level definition:", se)

################################################################
import argparse
import sys
import re

def s_exps_from_string(string_or_stream):
    return sexpdata.Parser(string_or_stream, nil=None, true="true", false="false", line_comment=";").parse()

def strip_block_comments(string):
    # Strip block comments
    regex = r"""\#\|               # hash bar
                    (
                        [^\|\#]     # (not bar or hash)  Do not be tempted to suffix with "+" - bad perf
                    |               # or...
                        (\|(?!\#))  # (bar not followed by hash)
                    |               # or...
                        (\#(?!\|))  # (hash not followed by bar)
                    )*             # many times 
                    \|\#           # bar hash
                """
    regex = re.compile(regex, flags=re.DOTALL | re.VERBOSE)
    while True:
        string, n = re.subn(regex, '', string)
#       print(f"Zapped {n} block comment(s)")
        if n == 0:
            return string    


def parse_ks_string(string):
    string = strip_block_comments(string)

    for s_exp in s_exps_from_string(string):
        try:
            yield parse_tld(s_exp)
            
        except ParseError:
            print("ERROR at ", s_exp)
            print(sys.exc_info()[1])

def parse_ks_file(filename):
    with open(filename) as f:
        ks_str = f.read()
        return parse_ks_string(ks_str)

def main():
    parser = argparse.ArgumentParser(prog="parse_ks.py", description=__doc__)
    parser.add_argument("input_ks_file", nargs='?', type=str, default="test/ksc/syntax-primer.ks")
    args = parser.parse_args()

    for x in parse_ks_file(args.input_ks_file):
        cpprint(x)
        print(pystr(x, 0))

    return 0

if __name__ == "__main__":
    sys.exit(main())
