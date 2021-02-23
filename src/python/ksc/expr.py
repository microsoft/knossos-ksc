"""
Expr: lightweight classes implementing the Knossos IR
"""

from typing import Union, List, Tuple, Any
from dataclasses import dataclass
from ksc.type import Type
from ksc.utils import paren, KRecord

#####################################################################
# 
# The Knossos IR is a lightweight, clean, functional IR with strong
# similarities to Lisp.  The AST has just a few basic concepts, 
# each with a backing class defined below.  The concepts are, in
# lisp-like (KS) syntax, with named fields below:
#
# Structured Names
# StructuredName: identifier | [identifier Type] | ["derivation" StructuredName]
#
### Top-level definitions (TLDs):
#
# Def: Function definition
# (def add   (Vec Float)  ((a : Float) (b : (Vec Float))) ...)
#      ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^
#      name  return_type  args                            body
#
# Edef: Declaration for externally-supplied function
# (edef add   (Vec Float)  ((a : Float) (b : (Vec Float))) )
#       ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#       name  return_type  args
#
# Rule: Rewrite rule for the Knossos optimizer 
# (rule "add0"  (a : Float) (add a 0) a)
#       ^^^^^^  ^^^^^^^^^^^ ^^^^^^^^^ ^
#       name    args        e1        e2
#
### Expression nodes (Expr):
# 
# Const: Constant float, int, string, bool
# (combine 1.234      "a string")
#          ^^^^^      ^^^^^^^^^^
#          value      value'
#
# Var: Variable use or declaration (with type)
# (add x 1.234)  ; use of var, decl=false, type is None or propagated
#      ^
#      name
# (lam (x :    Float) ...)  ; decl of var, decl=true, type is known at parse time
#       ^      ^^^^^
#       name   type
#
# Call: Function call, including assert, tuple, and other "builtins" 
# (add   1.234 4.56)
#  ^     ^^^^^^^^^^
#  name  args
#
# Lam: Anonymous function/lambda, single argument -- use tuples to pass more 
# (lam (i : Integer)  (add i 4))
#      ^^^^^^^^^^^^^  ^^^^^^^^^
#      arg            body
#
# Let: Variable binding with scope limited to "body".
# (let (a    1)   (add a a))
#       ^    ^    ^^^^^^^^^
#       var  rhs  body
#
# If: We could just consider "if" to be another function call, but it defines sequencing.
# (if (eq a a) "good"  "bad")
#     ^^^^^^^^ ^^^^^^  ^^^^^
#     cond     t_body  f_body
#
# Assert: As with If, it defines a sequence, so is included.
# (assert (gt i 0) (do_stuff))
#         ^^^^^^^^ ^^^^^^^^^^
#         cond     body

########################################################################
# Structured names. A structured name can be
#  fred       - string
#  [rev <structured_name>]    - "derivation" name
#  [fred Type]
@dataclass(frozen=True)
class StructuredName:
    se: Union[str, Tuple[str, Type], Tuple[str, 'StructuredName']]

    def is_derivation(self):
        return isinstance(self.se, tuple) and isinstance(self.se[1], StructuredName)

    def is_derived(self, derivation):
        return self.is_derivation() and self.se[0] == derivation

    def mangled(self):
        if isinstance(self.se, str):
            return self.se
        elif self.is_derivation():
            return self.se[0] + "$" + self.se[1].mangled()
        else:
            assert isinstance(self.se[1], Type)
            return self.se[0] + "$a" + self.se[1].shortstr()

    def __repr__(self):
        return "StructuredName(" + str(self.se) + ")"


def make_structured_name(se) -> StructuredName:
    """
    Convert tuple form to StructuredName
    E.g. make_structured_name(("rev", ("f", Type.Integer)))
    """
    if isinstance(se, str):
        return StructuredName(se)

    if isinstance(se, tuple):
        assert len(se) == 2
        assert isinstance(se[0], str)
        # (str, Type)
        if isinstance(se[1], Type):
            return StructuredName((se[0], se[1]))
        # (str, StructuredName): Recurse
        se1 = make_structured_name(se[1])
        return StructuredName((se[0], se1))

    assert False, f"Bad name {se}"

########################################################################
# TLDs. 

class Expr(KRecord):
    '''Base class for AST nodes.'''

    type_: Type # All expressions have a type.  It may be initialized to None, and then filled in by type inference
    # TODO: lift Def,EDef,Rule to "ASTNode" and make Expr an "ASTNode".

    def __init__(self, **args):
        self.type_ = None
        super().__init__(**args)

    def nodes(self):
        """
        Return child nodes of this expr
        """
        assert False # TODO: remove this method
        for nt in self.__annotations__:
            yield getattr(self, nt)

    def __str__(self):
        nodes = (str(getattr(self, nt)) for nt in self.__annotations__)
        return paren(type(self).__name__ + ' ' + ' '.join(nodes))

class Def(Expr):
    '''Def(name, return_type, args, body). 
    Example:
    ```
    (def add   (Vec Float)  ((a : Float) (b : (Vec Float))) ...)
         ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^
         name  return_type  args                            body
    ```
    '''
    name: StructuredName
    return_type: Type
    args: list
    body: Expr

    def __init__(self, name, return_type, args, body):
        super().__init__(name=name, return_type=return_type, args=args, body=body)

class EDef(Expr):
    '''Edef(name, return_type, args). 
    Example:
    ```
    (edef add   (Vec Float)  (Float (Vec Float)) )
          ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          name  return_type  arg_types
    ```
    '''
    name: StructuredName
    return_type: Type
    arg_types: List[Type]

    def __init__(self, name, return_type, arg_types):
        super().__init__(name=name, return_type=return_type, arg_types=arg_types)

class GDef(Expr):
    '''Gdef(name, return_type, args). 
    Example:
    ```
    (gdef rev         [add (Tuple Float Float)])
          ^^^         ^^^^
          derivation  function_name
    ```
    '''
    derivation: str
    function_name: StructuredName

    def __init__(self, derivation, function_name):
        super().__init__(derivation=derivation, function_name=function_name)

    def name(self):
        return StructuredName((derivation, function_name))

class Rule(Expr):
    '''Rule(name, args, e1, e2). 
    Example:
    ```
    (rule "add0"  (a : Float) (add a 0) a)
          ^^^^^^  ^^^^^^^^^^^ ^^^^^^^^^ ^
          name    args        e1        e2
    ```
    '''
    name: str
    args: list
    e1: Expr
    e2: Expr

    def __init__(self, name, args, e1, e2):
        super().__init__(name=name, args=args, e1=e1, e2=e2)

class Const(Expr):
    '''Const(value). 
    Examples:
    ```
    (combine 1.234      "a string")
             ^^^^^      ^^^^^^^^^^
             value      value'
    ```
    '''
    value: Union[int, str, float, bool]

    def __init__(self, value):
        super().__init__(type_=Type.fromValue(value), value=value)

    def __str__(self):
        return repr(self.value)

class Var(Expr):
    '''Var(name, type, decl). 
    Examples:
    ```
    (add x 1.234)  ; use of var, decl=false, type is None or propagated
         ^
         name
    (lam (x :    Float) ...)  ; decl of var, decl=true, type is known at parse time
          ^      ^^^^^
          name   type
    ```
    '''
    name: str
    decl: bool

    def __init__(self, name, type=None, decl=False):
        super().__init__(type_=type, name=name, decl=decl)

    def __str__(self):
        if self.decl:
            return self.name + " : " + str(self.type_)
        else:
            return self.name

class Call(Expr):
    '''Call(name, args). 
    Example:
    ```
    (add   1.234 4.56)
     ^^^   ^^^^^^^^^^
     name  args
    ```
    '''
    name: str
    args: List[Expr]

    def __init__(self, name, args):
        super().__init__(name=name, args=args)

class Lam(Expr):
    '''Lam(arg, body).
     Example:
    ```
    (lam (i : Integer)  (add i 4))
         ^^^^^^^^^^^^^  ^^^^^^^^^
         arg            body
    ```
    '''
    arg: Var
    body: Expr

    def __init__(self, arg, body):
        super().__init__(type_=None, arg=arg, body=body)

class Let(Expr):
    '''Let(vars, rhs, body). 
    Example:
    ```
    (let (a    1)   (add a a))
          ^    ^    ^^^^^^^^^
          var  rhs  body
    ```
    ```
    (let ((a b)  (tuple p q))   (add a a))
          ^      ^              ^^^^^^^^^
          var    rhs            body
    ```
    '''
    vars: Union[Var, List[Var]]
    rhs: Expr
    body: Expr

    def __init__(self, vars, rhs, body):
        super().__init__(vars=vars, rhs=rhs, body=body)


class If(Expr):
    '''If(cond, t_body, f_body). 
    Example:
    ```
    (if (eq a a) "good"  "bad")
        ^^^^^^^^ ^^^^^^  ^^^^^
        cond     t_body  f_body
    ```
    '''
    cond: Expr    # Condition
    t_body: Expr  # Value if true
    f_body: Expr  # Value if false

    def __init__(self, cond, t_body, f_body):
        super().__init__(cond=cond, t_body=t_body, f_body=f_body)

class Assert(Expr):
    '''Assert(cond, body).
    Example:
    ```
    (assert (gt i 0) (do_stuff))
            ^^^^^^^^ ^^^^^^^^^^
            cond     body
    ```
    '''
    cond: Expr    # Condition
    body: Expr    # Value if true

    def __init__(self, cond, body):
        super().__init__(cond=cond, body=body)

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

from ksc.expr import Expr, Def, EDef, GDef, Call, Const, Var, If

def pyname(s : str) -> str:
    return s.replace('$','_s')

def nl(indent):
    return "\n" + "  " * indent

def pystr_intercomma(indent, exprs):
    return ", ".join([pystr(ex, indent) for ex in exprs])

@singledispatch
def pystr(expr, indent):
    """
    Expression to string, formatted in a loose python-like syntax
    """
    # Default implementation, for types not specialized below
    return str(expr)

@pystr.register(Type)
def _(ty, indent):
    if ty.is_scalar:
        return ty.kind
    elems = [pystr(c, indent+1) for c in ty.children]
    return ty.kind + "[" + ",".join(elems) + "]"    

@pystr.register(Def)
def _(ex, indent):
    indent += 1
    return "def " + pyname(ex.name.mangled()) + "(" + pystr_intercomma(indent, ex.args) + ") -> " \
           + pystr(ex.return_type, indent) + ":" \
           + nl(indent+1) + pystr(ex.body, indent+1)

@pystr.register(EDef)
def _(ex, indent):
    indent += 1
    return "#edef " + str(ex.name) + "(" + pystr_intercomma(indent, ex.arg_types) + ") -> "\
           + pystr(ex.return_type, indent) + nl(indent)

@pystr.register(GDef)
def _(ex, indent):
    indent += 1
    return "#gdef " + ex.derivation + " " + str(ex.function_name)

@pystr.register(Rule)
def _(ex, indent):
    indent += 1
    return "@rule\ndef " + pyname(ex.name) + " " + "(" + pystr(ex.args, indent) + ")" + ":" + nl(indent) \
           + pystr(ex.e1, indent+1) + nl(indent) \
           + "<===> " + nl(indent) \
           + pystr(ex.e2, indent+1)

@pystr.register(Const)
def _(ex, indent):
    return repr(ex.value)

@pystr.register(Var)
def _(ex, indent):
    if ex.decl:
        return pyname(ex.name) + ": " + pystr(ex.type_, indent)
    else:
        return pyname(ex.name)

@pystr.register(Call)
def _(ex, indent):
    indent += 1
    return pystr(pyname(ex.name), indent) + "(" + pystr_intercomma(indent, ex.args) + ")"

@pystr.register(Lam)
def _(ex, indent):
    indent += 1
    return "lambda " + pyname(ex.arg.name) + ": " + nl(indent+1)\
            + "(" + pystr(ex.body, indent+1) + ")"

@pystr.register(Let)
def _(ex, indent):
    if isinstance(ex.vars, list):
        var_str = ",".join(pystr(v, indent) for v in ex.vars)
    else:
        var_str = pystr(ex.vars, indent)
    return var_str + " = " + pystr(ex.rhs, indent+1) + nl(indent) \
         + pystr(ex.body, indent)

@pystr.register(If)
def _(ex, indent):
    return "(" + pystr(ex.t_body, indent+2) + " if " +  pystr(ex.cond, indent+1) + nl(indent+1) \
               + " else " + pystr(ex.f_body, indent+1) + ")\n"

@pystr.register(Assert)
def _(ex, indent):
    indent += 1
    return "assert(" + pystr(ex.cond, indent) + ")" + nl(indent) \
            + pystr(ex.body, indent)

if __name__ == "__main__":
    from ksc.parse_ks import parse_ks_file
    for decl in parse_ks_file("test/ksc/syntax-primer.ks"):
        print(decl)
        print(pystr(decl,0))  # Pystr here doesn't get dispatched properly... singledispatch sees __main__.Def, not ksc.expr.def
