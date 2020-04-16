"""
Expr: lightweight classes implementing the Knossos IR
"""

from typing import NamedTuple, Union, List

from ksc.type import Type

#####################################################################
## Expr classes

class Expr:
    type: Type

## Def
# (def add   (Vec Float)  ((a : Float) (b : (Vec Float))) ...)
#      ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^
#      name  return_type  args                            body
class Def(NamedTuple):
    name: str
    return_type: Type
    args: list
    body: Expr

## Edef
# (edef add   (Vec Float)  ((a : Float) (b : (Vec Float))) )
#       ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#       name  return_type  args
class EDef(NamedTuple):
    name: str
    return_type: Type
    args: list

## Rule
# (rule "add0"  (a : Float) (add a 0) a)
#       ^^^^^^  ^^^^^^^^^^^ ^^^^^^^^^ ^
#       name    args        e1        e2
class Rule(NamedTuple):
    name: str
    args: list
    e1: Expr
    e2: Expr

## Const
# (combine 1.234      "a string")
#          ^^^^^      ^^^^^^^^^^
#          value      value'
class Const(NamedTuple, Expr):
    value: Union[int, str, float, bool]

## Var
# (add x 1.234)  ; use of var, decl=false, type is None or propagated
#      ^
#      name
# (lam (x :    Float) ...)  ; decl of var, decl=true, type is known at parse time
#       ^      ^^^^^
#       name   type
class Var(NamedTuple, Expr):
    name: str
    type: Type
    decl: bool

## Call
# (add   1.234 4.56)
#  ^     ^^^^^^^^^^
#  name  args
class Call(NamedTuple, Expr):
    name: str
    args: list

## Lam
# (lam (i : Integer)  (add i 4))
#      ^^^^^^^^^^^^^  ^^^^^^^^^
#      arg            body
class Lam(NamedTuple, Expr):
    arg: Var
    body: Expr

## Let
# (let (a    1)   (add a a))
#       ^    ^    ^^^^^^^^^
#       var  rhs  body
class Let(NamedTuple, Expr):
    var: Var
    rhs: Expr
    body: Expr

## If
# (if (eq a a) "good"  "bad")
#     ^^^^^^^^ ^^^^^^  ^^^^^
#     cond     t_body  f_body
class If(NamedTuple, Expr):
    cond: Expr    # Condition
    t_body: Expr  # Value if true
    f_body: Expr  # Value if false

#####################################################################
# pystr:
#  Expression to string, formatted in a loose python-like syntax
#  This isn't a backend, just a way to view the expr structure in a format
#  slightly more palatable than s-expressions
#
# This also serves as an example of a user-defined AST visitor.
# Defining functions on each node type using singledispatch is a clean
# way to write recursive tree transformations.

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
    return "@rule\ndef " + ex.name + " " + "(" + pystr(ex.args, indent) + ")" + ":" + nl(indent) \
           + pystr(ex.e1, indent+1) + nl(indent) \
           + "<===> " + nl(indent) \
           + pystr(ex.e2, indent+1)

@pystr.register(Const)
def _(ex, indent):
    return repr(ex.value)

@pystr.register(Var)
def _(ex, indent):
    if ex.decl:
        return ex.name + ": " + str(ex.type)
    else:
        return ex.name

@pystr.register(Call)
def _(ex, indent):
    indent += 1

    # Some calls deserve fancy treatment or printing; but should not be AST nodes.
    # "If" is on the borderline, so assert is definitely fine.
    if ex.name == "assert":
        assert len(ex.args) == 2
        return "assert(" + pystr(ex.args[0], indent) + ")" + nl(indent) \
               + pystr(ex.args[1], indent)

    return pystr(ex.name, indent) + "(" + pystr_intercomma(indent, ex.args) + ")"

@pystr.register(Lam)
def _(ex, indent):
    indent += 1
    return "{lambda (" + pystr(ex.arg, indent) + "): " + nl(indent+1)\
            + pystr(ex.body, indent+1) + "}"

@pystr.register(Let)
def _(ex, indent):
    return pystr(ex.var, indent) + " = " + pystr(ex.rhs, indent+1) + nl(indent) \
         + pystr(ex.body, indent)

@pystr.register(If)
def _(ex, indent):
    return "(" + pystr(ex.t_body, indent+2) + " if " +  pystr(ex.cond, indent+1) + nl(indent+1) \
               + " else " + pystr(ex.f_body, indent+1) + ")\n"
