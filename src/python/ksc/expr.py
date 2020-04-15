"""
Expr: lightweight classes implementing the Knossos IR
"""
from collections import namedtuple

from ksc.type import Type

#####################################################################
## Expr classes

Def = namedtuple("Def", ["name", "return_type", "args", "body"])
EDef = namedtuple("EDef", ["name", "return_type", "args"])
Rule = namedtuple("Rule", ["name", "args", "e1", "e2"])
Const = namedtuple("Const", ["value"])
Var = namedtuple("Var", ["name", "type", "decl"])
Call = namedtuple("Call", ["f", "args"])
Lam = namedtuple("Lam", ["arg", "body"])
Let = namedtuple("Let", ["var", "rhs", "body"])
If = namedtuple("If", ["cond", "t_body", "f_body"])


#####################################################################
# pystr:
#  Expression to string, formatted in a loose python-like syntax
#  This isn't a backend, just a way to view the expr structure in a format
#  slightly more palatable than s-expressions
from functools import singledispatch
def nl(indent):
    return "\n" + "  " * indent

def pystr_intercomma(indent, exprs):
    return ", ".join([pystr(ex, indent) for ex in exprs])
 
@singledispatch
def pystr(expr, indent=0):
    """
    Expression to string, formatted in a loose python-like syntax
    This isn't a backend, just a way to view the expr structure in a format
    slightly more palatable than s-expressions
    """
    # Default implementation, for types not specialized below
    return str(expr)

@pystr.register(Def)
def _(ex, indent):
    indent += 1
    return "def " + ex.name + "(" + pystr_intercomma(indent, ex.args) + ") -> " \
           + pystr(ex.return_type, indent) + ":" \
           + nl(indent+1) + pystr(ex.body, indent+1)

@pystr.register(EDef)
def _(ex, indent):
    indent += 1
    return "edef " + ex.name + "(" + pystr_intercomma(indent, ex.args) + ") -> "\
           + pystr(ex.return_type, indent) + nl(indent)

@pystr.register(Rule)
def _(ex, indent):
    indent += 1
    return "@rule\ndef " + ex.name + " " + "(" + pystr(ex.args,indent) + ")" + ":" + nl(indent) \
           + pystr(ex.e1, indent+1) + nl(indent) \
           + "<===> " + nl(indent) \
           + pystr(ex.e2, indent+1)

@pystr.register(Const)
def _(ex, indent):
    return repr(ex.value)

@pystr.register(Var)
def _(ex, indent):
    if ex.decl:
        return ex.name + " : " + str(ex.type)
    else:
        return ex.name

@pystr.register(Call)
def _(ex, indent):
    indent += 1

    # Some calls deserve fancy treatment or printing; but should not be AST nodes.
    # Assert is very much on the borderline
    if ex.f == "assert":
        assert len(ex.args) == 2
        return "assert(" + pystr(ex.args[0], indent) + ")" + nl(indent) \
               + pystr(ex.args[1], indent)

    return pystr(ex.f, indent) + "(" + pystr_intercomma(indent, ex.args) + ")"

@pystr.register(Lam)
def _(ex, indent):
    indent += 1
    return "{lambda (" + pystr(ex.arg, indent) + ") : " + nl(indent+1) + pystr(ex.body, indent+1) + "}"

@pystr.register(Let)
def _(ex, indent):
    return pystr(ex.var, indent) + " = " + pystr(ex.rhs, indent+1) + nl(indent) \
         + pystr(ex.body, indent)

@pystr.register(If)
def _(ex, indent):
    return "(" + pystr(ex.t_body, indent+2) + " if " +  pystr(ex.cond, indent+1) + nl(indent+1) \
               + " else " + pystr(ex.f_body, indent+1) + ")\n"

#####################################################################
# parse_: Convert s-expressions to Expr
import sexpdata

from translate import _convert_to_type, ensure_list_of_lists

#####################################################################
## S-expression Utils

# Parse a fixed-length s-exp into a list given a set of parsers, e.g.
# parse_seq(se, parse_name, parse_int, parse_int) 
# would accept (fred 3 4)
def parse_seq(se, *parsers):
    if len(se) != len(parsers):
        raise ParseError("Cannot parse ", se, " with ", parsers)

    return [parser(term) for parser,term in zip(parsers, se)]

#####################################################################
# parse_{expr,tld}: Convert s-expressions to Exprs

class ParseError(Exception):
    pass

# Reserved word constants
_def = sexpdata.Symbol("def")
_edef = sexpdata.Symbol("edef")
_if = sexpdata.Symbol("if")
_let = sexpdata.Symbol("let")
_lam = sexpdata.Symbol("lam")
_tuple = sexpdata.Symbol("tuple")
_rule = sexpdata.Symbol("rule")
_colon = sexpdata.Symbol(":")

# Knossos Type -> string
def parse_type(se):
    return _convert_to_type(se)

# "x" -> string
def parse_name(se):
    if isinstance(se, sexpdata.Symbol):
        return se.value()
    raise ParseError("Wanted identifier, got: ", se)

# "\"x\"" -> string
def parse_string(se):
    assert isinstance(se, str)
    return se

# "x : Float" -> Var(x, Type.Float)
def parse_arg(arg):
    if len(arg) < 3:
        raise ParseError("expect (arg : type), not: ", arg)
    if arg[1] != _colon:
        raise ParseError("no colon: ", arg) 
        
    return Var(parse_name(arg[0]), parse_type(arg[2:]), True)

# "((x : Float) (y : Integer))" -> [Var("x", Type.Float), Var("y", Type.Integer)]
def parse_args(se):
    return [parse_arg(arg) for arg in ensure_list_of_lists(se)]

def parse_expr(se):
    # "x" -> always a variable
    if isinstance(se, sexpdata.Symbol):
        return Var(se.value(), None, False)

    # "1.2", "1", "'string'"
    if not isinstance(se, list):
        return Const(se)

    # Lists of length one should not occur.  
    # TODO: It is tempting to always strip ((redundant)) parens, but in general a 
    # profusion of such defenses can hide carbuncles elsewhere
    if len(se) < 2 and se[0] != _tuple:
        raise ParseError("Cannot parse ", se, " in TODO context")

    head = se[0]
    if head == _if:
        return If(*parse_seq(se[1:], parse_expr, parse_expr, parse_expr))

    # Let(var, rhs, body)
    if head == _let:
        bindings = ensure_list_of_lists(se[1])
        ans = parse_expr(se[2])
        for b in bindings[::-1]:
            assert len(b) == 2
            var = parse_name(b[0])
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
    try:
        assert isinstance(se, list)
        assert len(se) > 0
        head = se[0]
        if head == _def:
            return Def(*parse_seq(se[1:], parse_name, parse_type, parse_args, parse_expr))

        if head == _edef:
            return EDef(*parse_seq(se[1:], parse_name, parse_type, 
                        lambda ts : map(parse_type, ts)))

        if head == _rule:
            return Rule(*parse_seq(se[1:], parse_string, parse_args, parse_expr, parse_expr))

        raise ParseError("unrecognised top-level definition:", se)

    except ParseError:
        print("ERROR: ", se)
        print(sys.exc_info()[1])
        raise ParseError

################################################################
import argparse
import sys

def parse_ks_file(string_or_stream):
    s_exps = sexpdata.Parser(string_or_stream, nil=None, true="True", false="False").parse()
    return [parse_tld(s_exp) for s_exp in s_exps]

def main():
    parser = argparse.ArgumentParser(prog="python -m ksc.translate", description=__doc__)
    parser.add_argument("input_ks_file", type=str)
    args = parser.parse_args()

    with open(args.input_ks_file) as f:
        ks_str = f.read()
        print(ks_str)
        print("--------------->")
        [print(pystr(x, 0)) for x in parse_ks_file(ks_str)]

if __name__ == "__main__":
    sys.exit(main())

