from copy import deepcopy
from functools import singledispatch
from typing import final

from ksc.expr import Expr, Let, Call, Var, If, Assert, Const, Lam

class ExprTransformer:
    """ Superclass for functions that transform Expressions by recursive traversal.
        The default "transformation" is the identity function, but each case first recursively transforms the sub-Expr's within the Expr,
        allowing overriding of specific cases.
    """

    def __init__(self):
        # One might expect to use singledispatchmethod. However, that expects to act on the class member (a function expecting a self argument),
        # and returns a class member (expecting a self argument - unlike self.transform).
        self.transform = singledispatch(self.transform)

        # Provide specific types rather than depending on subclasses to specify them correctly
        for type, meth in [(Var, self.transform_var), (Const, self.transform_const),
                           (Let, self.transform_let), (Lam, self.transform_lam),
                           (Call, self.transform_call), (If, self.transform_if), (Assert, self.transform_assert)]:
            self.transform.register(type)(meth)

    @final # Not enforced, merely a marker if type-checking with pyright/mypy/etc.
    def transform(self, e: Expr) -> Expr:
        """ Interface to call to process an Expr; should not be overridden.
            It will switch on the type of its first argument to one of the member functions transform_(var,const,let,lam,call,if,assert) """
        # Extending to ASTNode should be straightforward.
        raise AssertionError("Overridden for all Expr subclasses")

    def transform_var(self, v: Var) -> Expr:
        """ Overridable method that is called to handle a non-decl Var being passed to transform """
        # We *could* pass Lam args and Let vars into self.transform and thus here, allowing arg.is_decl to tell whether being a binding occurrence;
        # or even have a separate method self.transform_binder.
        # But we expect subclasses to override transform_lam/let,
        # as any change to the bound variable is likely to have to be co-ordinated with the rest of the binding Expr.
        return v

    def transform_const(self, c: Const) -> Expr:
        """ Overridable method that is called to handle a Const being passed to transform """
        return c

    def transform_let(self, l: Let) -> Expr:
        """ Overridable method that is called to handle a Let being passed to transform """
        return Let(l.vars, self.transform(l.rhs), self.transform(l.body))

    def transform_lam(self, l: Lam) -> Expr:
        """ Overridable method that is called to handle a Lam being passed to transform """
        return Lam(l.arg, self.transform(l.body))

    def transform_call(self, c: Call) -> Expr:
        """ Overridable method that is called to handle a Call being passed to transform """
        return Call(c.name, [self.transform(a) for a in c.args])

    def transform_if(self, i: If) -> Expr:
        """ Overridable method that is called to handle an If being passed to transform """
        return If(self.transform(i.cond), self.transform(i.t_body), self.transform(i.f_body))

    def transform_assert(self, a: Assert) -> Expr:
        """ Overridable method that is called to handle an Assert being passed to transform """
        return Assert(self.transform(a.cond), self.transform(a.body))

