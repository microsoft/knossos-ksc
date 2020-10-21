"""
Expr: lightweight classes implementing the Knossos IR
"""

from typing import Union, List
from ksc.type import Type
from ksc.utils import paren

#####################################################################
# 
# The Knossos IR is a lightweight, clean, functional IR with strong
# similarities to Lisp.  The AST has just a few basic concepts, 
# each with a backing class defined below.  The concepts are, in
# lisp-like (KS) syntax, with named fields below:
#
### Top-level definitions (TLDs):
#
# Def: Function definition
# (def add   (Vec Float)  ((a : Float) (b : (Vec Float))) ...)
#      ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^
#      name  type         args                            body
#
# Edef: Declaration for externally-supplied function
# (edef add   (Vec Float)  (Float (Vec Float)) )
#       ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^
#       name  type         arg_types
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
# (let (a     1)   (add a a))
#       ^     ^    ^^^^^^^^^
#       vars  rhs  body
# Tuple-unpacking form:
# (let ((a b)  tup)   (add a a))
#       ^      ^      ^^^^^^^^^
#       vars   rhs    body
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

class Expr:
    '''Base class for AST nodes.'''
    type: Type

    def __init__(self, **args):
        for (nt,v) in args.items():
            setattr(self, nt, v)

    def __eq__(self, that):
        if type(self) != type(that):
            return False

        for nt in self.__annotations__:
            if getattr(self, nt) != getattr(that,nt):
                return False
        return True

    def nodes(self):
        """
        Return child nodes of this expr
        """
        for nt in self.__annotations__:
            yield getattr(self, nt)

    def __str__(self):
        return paren(type(self).__name__ + ' ' + ' '.join(str(node) for node in self.nodes()))


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
        super().__init__(type=Type.fromValue(value), value=value)

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

    def __init__(self, name, type, decl):
        super().__init__(type=type, name=name, decl=decl)

    def __str__(self):
        if self.decl:
            return self.name + " : " + str(self.type)
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
        for a in args:
            assert isinstance(a, Expr)

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
        super().__init__(type=None, arg=arg, body=body)

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
          vars   rhs            body
    ```
    '''
    vars: Union[Var, List[Var]]  # TODO: Just List[Var]?  Not really as detupling is different from straight assignment
    rhs: Expr
    body: Expr

    def __init__(self, vars, rhs, body):
        super().__init__(type=None, vars=vars, rhs=rhs, body=body)
        assert isinstance(vars,Var)


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
        super().__init__(type=None, cond=cond, t_body=t_body, f_body=f_body)

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
        super().__init__(type=None, cond=cond, body=body)

class Def(Expr):
    '''Def(name, type, args, body). 
    Example:
    ```
    (def add   (Vec Float)  ((a : Float) (b : (Vec Float))) ...)
         ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^
         name  type         args                            body
    ```
    '''
    name: str
    args: List[Var]
    body: Expr

    def __init__(self, name, type, args, body):
        super().__init__(type=type, name=name, args=args, body=body)

class EDef(Expr):
    '''Edef(name, type, args). 
    Example:
    ```
    (edef add   (Vec Float)  ((Float) ((Vec Float))) )
          ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          name  type         arg_types
    ```
    '''
    name: str
    arg_types: List[Type]

    def __init__(self, name, type, arg_types):
        super().__init__(type=type, name=name, arg_types=arg_types)

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
        super().__init__(type=None, name=name, args=args, e1=e1, e2=e2)

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

from ksc.expr import Expr, Def, EDef, Call, Const, Var, If

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
    if len(ty.children) == 0 and (ty.kind != "Tuple"):
        return ty.kind
    elems = [pystr(c, indent+1) for c in ty.children]
    return ty.kind + "[" + ",".join(map(lambda x: pystr(x,indent+1), elems)) + "]"    

@pystr.register(Def)
def _(ex, indent):
    indent += 1
    return "def " + pyname(ex.name) + "(" + pystr_intercomma(indent, ex.args) + ") -> " \
           + pystr(ex.type, indent) + ":" \
           + nl(indent+1) + pystr(ex.body, indent+1)

@pystr.register(EDef)
def _(ex, indent):
    indent += 1
    return "#edef " + pyname(ex.name) + "(" + pystr_intercomma(indent, ex.arg_types) + ") -> "\
           + pystr(ex.type, indent) + nl(indent)

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
        return pyname(ex.name) + ": " + pystr(ex.type, indent)
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
    return pystr(ex.vars, indent) + " = " + pystr(ex.rhs, indent+1) + nl(indent) \
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
        print(pystr(decl,0))  # Pystr here doesn't get dispatched properly... singledispacth sees __main__.Def, not ksc.expr.def
