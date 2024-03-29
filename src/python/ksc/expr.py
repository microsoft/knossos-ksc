"""
Expr: lightweight classes implementing the Knossos IR
"""

from typing import FrozenSet, List, Tuple, Union, Optional
from dataclasses import dataclass

from prettyprinter import pformat

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
# (edef add   (Vec Float)  (Tuple Float (Vec Float)))
#       ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#       name  return_type  arg_type
#
# Rule: Rewrite rule for the Knossos optimizer
# (rule "add0"  (a : Float)     (add a 0)   a)
#       ^^^^^^  ^^^^^^^^^^^     ^^^^^^^^^   ^
#       name    template_vars   template    replacement
#
### Expression nodes (Expr):
#
# Const: Constant float, int, string, bool
# (combine 1.234      "a string")
#          ^^^^^      ^^^^^^^^^^
#          value      value'
#
# Var: Variable use or declaration (with type)
# (add x 1.234)  ; use of var, type is None or propagated
#      ^
#      name
# (lam (x :    Float) ...)  ; decl of var, type is known at parse time
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
    se: Union[str, Tuple[str, Type], Tuple[str, "StructuredName"]]

    def is_derivation(self):
        """
        True if this is a "rev"/"fwd" etc of another StructuredName
        """
        return isinstance(self.se, tuple) and isinstance(self.se[1], StructuredName)

    def is_derived(self, derivation: str):
        """
        True if this is exactly <derivation> of another StructuredName
        e.g.
        True == parse("[rev [foo Float]]").is_derived("rev")
        """
        return self.is_derivation() and self.se[0] == derivation

    def mangled(self) -> str:
        """
        Return the mangled form of this name, for shorthand printing
         [rev foo] -> "rev$foo"
         [foo (Tuple Float Float)] -> "foo@ff"
         [rev [fwd [foo (Tuple Float Float)]]] -> "rev$fwd$foo@ff"

         Note that the "real" mangled name, as will appear in say C++ code,
         is correct only if self.has_type()
        """
        if isinstance(self.se, str):
            return self.se

        if self.is_derivation():
            assert isinstance(self.se[1], StructuredName)  # typechecking
            return self.se[0] + "$" + self.se[1].mangled()

        assert isinstance(self.se[1], Type)
        return self.se[0] + "@" + self.se[1].shortstr(te="", tb="")

    def has_type(self) -> bool:
        """
        True if the innermost se is a type
        """
        if isinstance(self.se, str):
            return False

        if self.is_derivation():
            assert isinstance(self.se[1], StructuredName)  # typechecking
            return self.se[1].has_type()

        assert isinstance(self.se[1], Type)
        return True

    def get_type(self) -> Optional[Type]:
        """
        If the innermost se is a type, return it, else None
        """
        if isinstance(self.se, str):
            return None

        if self.is_derivation():
            assert isinstance(self.se[1], StructuredName)  # typechecking
            return self.se[1].get_type()

        assert isinstance(self.se[1], Type)
        return self.se[1]

    def add_type(self, ty) -> Tuple["StructuredName", Optional[Type]]:
        """
        Return a new structured name, with "ty" inserted in the corner, returning the old type if any
        sn = parse("[shape [rev foo]]")
        old_type, new_sname = sn.add_type(Type.Float)
        Now old_type is None
            new_sname is "[shape [rev [foo Float]]]"
        """
        if isinstance(self.se, str):
            return StructuredName((self.se, ty)), None
        if self.is_derivation():
            assert isinstance(self.se[1], StructuredName)  # typechecking
            new_sn, old_ty = self.se[1].add_type(ty)
            return StructuredName((self.se[0], new_sn)), old_ty

        assert isinstance(self.se[1], Type)
        old_ty = self.se[1]
        return StructuredName((self.se[0], ty)), old_ty

    def mangle_without_type(self) -> str:
        """
        Return the mangled form of this name
         [rev foo] -> "rev$foo"
         [foo (Tuple Float Float)] -> "foo"
         [rev [fwd [foo (Tuple Float Float)]]] -> "rev$fwd$foo"
        """
        if isinstance(self.se, str):
            return self.se

        if self.is_derivation():
            assert isinstance(self.se[1], StructuredName)  # typechecking
            return self.se[0] + "$" + self.se[1].mangle_without_type()

        return self.se[0]

    def __str__(self):
        if isinstance(self.se, str):
            return self.se
        return f"[{self.se[0]} {self.se[1]}]"

    def __repr__(self):
        return "StructuredName(" + str(self.se) + ")"

    @staticmethod
    def from_str(s: str):
        assert "$" not in s
        return StructuredName(s)


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


class ASTNode(KRecord):
    """ Base class for AST nodes. Not directly instantiable. """

    def __init__(self, **kwargs):
        # This assertion prevents intermediate classes (ASTNode, Expr) from being instantiated.
        # (KRecord doesn't mind).
        assert kwargs.keys() == self.__annotations__.keys()
        super().__init__(**kwargs)

    def __str__(self):
        # This registers the various handlers, we don't call it directly.
        # Can't be at toplevel because it imports ksc.expr.
        from ksc import prettyprint

        return pformat(self)

    def __repr__(self):
        # This does not satisfy the general contract of `__repr__` to return python
        # code that reproduces the object. But it is still useful for dobugging.
        nodes = (repr(getattr(self, nt)) for nt in self.__annotations__)
        return paren(type(self).__name__ + " " + " ".join(nodes))


class Expr(ASTNode):
    """Base class for Expression AST nodes. Not directly instantiable."""

    type_: Type  # All expressions have a type.  It may be initialized to None, and then filled in by type inference
    free_vars_: FrozenSet[str]  # Filled in by constructor

    def __init__(self, **args):
        self.type_ = args.pop("type_", None)
        super().__init__(**args)
        self.free_vars_ = compute_free_vars(self)


class Def(ASTNode):
    """Def(name, return_type, args, body). 
    Example:
    ```
    (def add   (Vec Float)  ((a : Float) (b : (Vec Float))) ...)
         ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^
         name  return_type  args                            body
    ```
    """

    name: StructuredName
    return_type: Type
    args: List["Var"]
    body: Expr

    def __init__(self, name, return_type, args, body):
        assert isinstance(name, StructuredName)
        super().__init__(name=name, return_type=return_type, args=args, body=body)

    def __repr__(self):
        elems = [
            self.name,
            self.return_type,
            "[" + ", ".join([arg.decl_str() for arg in self.args]) + "]",
            self.body,
        ]
        return paren("Def " + " ".join([str(e) for e in elems]))

    def __eq__(self, other):
        # KRecord's default for Var's (and other Exprs) is not to check type_ as it is "inherited" from a superclass.
        # See #941. TODO remove this when migrated from KRecord to dataclass.
        return super().__eq__(other) and all(
            a.type_ == oa.type_ for a, oa in zip(self.args, other.args)
        )


class EDef(ASTNode):
    """Edef(name, return_type, arg). 
    Example:
    ```
    (edef add   (Vec Float)  (Tuple Float (Vec Float)) )
          ^^^   ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
          name  return_type  arg_type
    ```
    """

    name: StructuredName
    return_type: Type
    arg_type: Type

    def __init__(self, name, return_type, arg_type):
        super().__init__(name=name, return_type=return_type, arg_type=arg_type)


class GDef(ASTNode):
    """Gdef(name, return_type, args). 
    Example:
    ```
    (gdef rev         [add (Tuple Float Float)])
          ^^^         ^^^^
          derivation  function_name
    ```
    """

    derivation: str
    function_name: StructuredName

    def __init__(self, derivation, function_name):
        super().__init__(derivation=derivation, function_name=function_name)

    def name(self):
        return StructuredName((self.derivation, self.function_name))


class Rule(ASTNode):
    """Rule(name, template_vars, template, replacement). 
    Example:
    ```
    (rule "add0"  (a : Float)     (add a 0)   a)
          ^^^^^^  ^^^^^^^^^^^     ^^^^^^^^^   ^
          name    template_vars   template    replacement
    ```
    See further detail in class ParsedRuleMatcher.
    """

    name: str
    template_vars: List["Var"]
    template: Expr
    replacement: Expr

    def __init__(self, name, template_vars, template, replacement):
        super().__init__(
            name=name,
            template_vars=template_vars,
            template=template,
            replacement=replacement,
        )


ConstantType = Union[int, str, float, bool]


class Const(Expr):
    """Const(value). 
    Examples:
    ```
    (combine 1.234      "a string")
             ^^^^^      ^^^^^^^^^^
             value      value'
    ```
    """

    value: ConstantType

    def __init__(self, value: ConstantType):
        super().__init__(type_=Type.fromValue(value), value=value)

    def __repr__(self):
        return repr(self.value)


class Var(Expr):
    """Var(name, type, decl). 
    Examples:
    ```
    (add x 1.234)  ; use of var, type is None or propagated
         ^
         name
    (lam (x :    Float) ...)  ; decl of var, type is known at parse time
          ^      ^^^^^
          name   type
    ```
    """

    name: str

    def __init__(self, name, type=None):
        super().__init__(type_=type, name=name)

    def __repr__(self):
        return self.name  # Omit "(Var )" and don't show type

    def decl_str(self):
        return self.name + " : " + str(self.type_)


class Call(Expr):
    """Call(name, args). 
    Example:
    ```
    (add   1.234 4.56)
     ^^^   ^^^^^^^^^^
     name  args
    ([rev [add (Tuple Float Float)]]  1.234 4.56)
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^
     name                             args
    ```
    """

    name: StructuredName
    args: List[Expr]

    def __init__(self, name, args, type=None):
        if isinstance(name, str):
            name = StructuredName.from_str(name)
        super().__init__(name=name, args=args, type_=type)


class Lam(Expr):
    """Lam(arg, body).
     Example:
    ```
    (lam (i : Integer)  (add i 4))
         ^^^^^^^^^^^^^  ^^^^^^^^^
         arg            body
    ```
    """

    arg: Var
    body: Expr

    def __init__(self, arg, body, type=None):
        assert arg.type_ is not None
        super().__init__(arg=arg, body=body, type_=type)

    def __repr__(self):
        return paren("Lam " + " ".join([self.arg.decl_str(), str(self.body)]))

    def __eq__(self, other):
        # KRecord's default for Var's (and other Exprs) is not to check type_ as it is "inherited" from a superclass.
        # See #941. TODO remove this when migrated from KRecord to dataclass.
        return super().__eq__(other) and self.arg.type_ == other.arg.type_


class Let(Expr):
    """Let(vars, rhs, body). 
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
    """

    vars: Union[Var, List[Var]]
    rhs: Expr
    body: Expr

    def __init__(self, vars, rhs, body, type=None):
        super().__init__(vars=vars, rhs=rhs, body=body, type_=type)


class If(Expr):
    """If(cond, t_body, f_body). 
    Example:
    ```
    (if (eq a a) "good"  "bad")
        ^^^^^^^^ ^^^^^^  ^^^^^
        cond     t_body  f_body
    ```
    """

    cond: Expr  # Condition
    t_body: Expr  # Value if true
    f_body: Expr  # Value if false

    def __init__(self, cond, t_body, f_body, type=None):
        super().__init__(cond=cond, t_body=t_body, f_body=f_body, type_=type)


class Assert(Expr):
    """Assert(cond, body).
    Example:
    ```
    (assert (gt i 0) (do_stuff))
            ^^^^^^^^ ^^^^^^^^^^
            cond     body
    ```
    """

    cond: Expr  # Condition
    body: Expr  # Value if true

    def __init__(self, cond, body, type=None):
        super().__init__(cond=cond, body=body, type_=type)


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


def pyname(s: str) -> str:
    return s.replace("$", "_s")


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
def pystr_Type(ty, indent):
    if ty.is_scalar:
        return ty.kind
    elems = [pystr(c, indent + 1) for c in ty.children]
    return ty.kind + "[" + ",".join(elems) + "]"


@pystr.register(StructuredName)
def pystr_StructuredName(sn, indent):
    return pyname(sn.mangled())


@pystr.register(Def)
def pystr_Def(ex, indent):
    indent += 1
    return (
        "def "
        + pystr(ex.name, indent)
        + "("
        + ", ".join([pyname(a.name) + ": " + pystr(a.type_, indent) for a in ex.args])
        + ") -> "
        + pystr(ex.return_type, indent)
        + ":"
        + nl(indent + 1)
        + pystr(ex.body, indent + 1)
    )


@pystr.register(EDef)
def pystr_EDef(ex, indent):
    indent += 1
    return (
        "#edef "
        + str(ex.name)
        + pystr(ex.arg_type, indent)
        + " -> "
        + pystr(ex.return_type, indent)
        + nl(indent)
    )


@pystr.register(GDef)
def pystr_GDef(ex, indent):
    indent += 1
    return "#gdef " + ex.derivation + " " + str(ex.function_name)


@pystr.register(Rule)
def pystr_Rule(ex, indent):
    indent += 1
    return (
        "@rule\ndef "
        + pyname(ex.name)
        + " "
        + "("
        + pystr(ex.template_vars, indent)
        + ")"
        + ":"
        + nl(indent)
        + pystr(ex.template, indent + 1)
        + nl(indent)
        + "<===> "
        + nl(indent)
        + pystr(ex.replacement, indent + 1)
    )


@pystr.register(Const)
def pystr_Const(ex, indent):
    return repr(ex.value)


@pystr.register(Var)
def pystr_Var(ex, indent):
    return pyname(ex.name)


@pystr.register(Call)
def pystr_Call(ex, indent):
    indent += 1
    return pystr(ex.name, indent) + "(" + pystr_intercomma(indent, ex.args) + ")"


@pystr.register(Lam)
def pystr_Lam(ex, indent):
    indent += 1
    return (
        "lambda "
        + pyname(ex.arg.name)
        + ": "
        + nl(indent + 1)
        + "("
        + pystr(ex.body, indent + 1)
        + ")"
    )


@pystr.register(Let)
def pystr_Let(ex, indent):
    if isinstance(ex.vars, list):
        var_str = ",".join(pystr(v, indent) for v in ex.vars)
    else:
        var_str = pystr(ex.vars, indent)
    return (
        var_str
        + " = "
        + pystr(ex.rhs, indent + 1)
        + nl(indent)
        + pystr(ex.body, indent)
    )


@pystr.register(If)
def pystr_If(ex, indent):
    return (
        "("
        + pystr(ex.t_body, indent + 2)
        + " if "
        + pystr(ex.cond, indent + 1)
        + nl(indent + 1)
        + " else "
        + pystr(ex.f_body, indent + 1)
        + ")\n"
    )


@pystr.register(Assert)
def pystr_Assert(ex, indent):
    indent += 1
    return (
        "assert(" + pystr(ex.cond, indent) + ")" + nl(indent) + pystr(ex.body, indent)
    )


#####################################################################
# Calculate free variables of an Expr.


@singledispatch
def compute_free_vars(e: Expr) -> FrozenSet[str]:
    raise ValueError("Must be overridden for every Expr subclass")


@compute_free_vars.register
def fv_var(e: Var):
    return frozenset([e.name])


@compute_free_vars.register
def fv_call(e: Call):
    return (
        frozenset()
        if len(e.args) == 0
        else frozenset.union(*[arg.free_vars_ for arg in e.args])
    )


@compute_free_vars.register
def fv_lam(e: Lam):
    return e.body.free_vars_ - e.arg.free_vars_


@compute_free_vars.register
def fv_let(e: Let):
    bound_here = (
        e.vars.free_vars_
        if isinstance(e.vars, Var)
        else frozenset.union(*[var.free_vars_ for var in e.vars])
    )
    return e.rhs.free_vars_.union(e.body.free_vars_ - bound_here)


@compute_free_vars.register
def fv_const(e: Const):
    return frozenset()


@compute_free_vars.register
def fv_assert(e: Assert):
    return frozenset.union(e.cond.free_vars_, e.body.free_vars_)


@compute_free_vars.register
def fv_if(e: If):
    return frozenset.union(e.cond.free_vars_, e.t_body.free_vars_, e.f_body.free_vars_)


if __name__ == "__main__":
    from ksc.parse_ks import parse_ks_file

    for decl in parse_ks_file("test/ksc/syntax-primer.ks"):
        print(decl)
        print(
            pystr(decl, 0)
        )  # Pystr here doesn't get dispatched properly... singledispatch sees __main__.Def, not ksc.expr.def
