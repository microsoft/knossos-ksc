"""
Expr: lightweight classes implementing the Knossos IR
"""

from typing import Union
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

class Expr:
    '''Base class for AST nodes.'''
    def __init__(self, *args):
        for (nt,v) in zip(self.__annotations__, args):
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

class Def(Expr):
    '''Def(name, return_type, args, body). 
    Example:
    ```
    (def add   (Vec Float)  ((a : Float) (b : (Vec Float))) ...)
         ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^
         name  return_type  args                            body
    ```
    '''
    name: str
    return_type: Type
    args: list
    body: Expr

class EDef(Expr):
    '''Edef(name, return_type, args). 
    Example:
    ```
    (edef add   (Vec Float)  ((a : Float) (b : (Vec Float))) )
          ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          name  return_type  args
    ```
    '''
    name: str
    return_type: Type
    args: list

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
    type: Type
    decl: bool

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
    args: list

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

class Let(Expr):
    '''Let(var, rhs, body). 
    Example:
    ```
    (let (a    1)   (add a a))
          ^    ^    ^^^^^^^^^
          var  rhs  body
    ```
    '''
    var: Var
    rhs: Expr
    body: Expr

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

@pystr.register(Assert)
def _(ex, indent):
    indent += 1
    return "assert(" + pystr(ex.cond, indent) + ")" + nl(indent) \
            + pystr(ex.body, indent)
