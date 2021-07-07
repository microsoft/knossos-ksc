from typing import Union

from ksc.expr import Expr, Let, Call, Var, If, Assert, Const, Lam

from ksc.path import ExprWithPath


class ExprVisitor:
    """ Superclass for functions that act upon particular subclasses of Expr.
        Like singledispatch, but allows a hierarchy of Visitor subclasses to inherit/override
         behaviour for specific Expr-subclasses
        The default implementation of visit does a recursive traversal of each sub-Expr,
         but does nothing and returns None; subclasses can override for Expr subclasses of interest.

        If called to visit an ExprWithPath, calls the visit_foo method for the appropriate subclass
        *of the ExprWithPath's subtree*, passing the ExprWithPath as first argument.
    """

    def __init__(self, visit_decls=False):
        self._visit_decls = visit_decls
        self._dispatch_table = {
            Var: self.visit_var,
            Const: self.visit_const,
            Lam: self.visit_lam,
            Let: self.visit_let,
            If: self.visit_if,
            Assert: self.visit_assert,
            Call: self.visit_call,
        }

    def visit(self, e: Union[Expr, ExprWithPath], *args, **kwargs):
        clas = (e.expr if isinstance(e, ExprWithPath) else e).__class__
        return self._dispatch_table[clas](e, *args, **kwargs)

    def visit_var(self, v: Union[Var, ExprWithPath], *args, **kwargs):
        """ Overridable method that is called to handle a non-decl Var being passed to visit """

    def visit_const(self, c: Union[Const, ExprWithPath], *args, **kwargs) -> None:
        """ Overridable method that is called to handle a Const being passed to visit """

    def visit_lam(self, l: Union[Lam, ExprWithPath], *args, **kwargs) -> None:
        """ Overridable method that is called to handle a Lam being passed to visit """
        if self._visit_decls:
            self.visit(l.arg, *args, **kwargs)
        self.visit(l.body, *args, **kwargs)

    def visit_let(self, l: Union[Let, ExprWithPath], *args, **kwargs) -> None:
        """ Overridable method that is called to handle a Let being passed to visit """
        if self._visit_decls:
            for v in [l.vars] if isinstance(l.vars, Var) else l.vars:
                self.visit(v, *args, **kwargs)
        self.visit(l.rhs, *args, **kwargs)
        self.visit(l.body, *args, **kwargs)

    def visit_if(self, i: Union[If, ExprWithPath], *args, **kwargs) -> None:
        """ Overridable method that is called to handle an If being passed to visit """
        self.visit(i.cond, *args, **kwargs)
        self.visit(i.t_body, *args, **kwargs)
        self.visit(i.f_body, *args, **kwargs)

    def visit_assert(self, a: Union[Assert, ExprWithPath], *args, **kwargs) -> None:
        """ Overridable method that is called to handle an Assert being passed to visit """
        self.visit(a.cond, *args, **kwargs)
        self.visit(a.body, *args, **kwargs)

    def visit_call(self, c: Union[Call, ExprWithPath], *args, **kwargs) -> None:
        """ Overridable method that is called to handle a Call being passed to visit """
        for a in c.args:
            self.visit(a, *args, **kwargs)


class ExprTransformer(ExprVisitor):
    """ Superclass for functions that transform Expressions by recursive traversal.
        The default "transformation" is the identity function, but each case first recursively visits the sub-Expr's within the Expr,
        allowing overriding of specific cases.
    """

    def __init__(self):
        # We expect any subclass interested in binding occurences of variables, to override visit_lam / visit_let,
        # so it can inspect the binder "in situ" and co-ordinate with changes to the rest of the binder.
        # Hence, this class does not support visit_decls=True.
        super().__init__(visit_decls=False)

    def visit_var(self, v: Union[Var, ExprWithPath], *args, **kwargs) -> Expr:
        return v.expr if isinstance(v, ExprWithPath) else v

    def visit_const(self, c: Const, *args, **kwargs) -> Expr:
        return c.expr if isinstance(c, ExprWithPath) else c

    def visit_let(self, l: Let, *args, **kwargs) -> Expr:
        return Let(
            l.vars,
            self.visit(l.rhs, *args, **kwargs),
            self.visit(l.body, *args, **kwargs),
            type=l.type_,
        )

    def visit_lam(self, l: Lam, *args, **kwargs) -> Expr:
        return Lam(l.arg, self.visit(l.body, *args, **kwargs), type=l.type_)

    def visit_call(self, c: Call, *args, **kwargs) -> Expr:
        return Call(
            c.name, [self.visit(a, *args, **kwargs) for a in c.args], type=c.type_
        )

    def visit_if(self, i: If, *args, **kwargs) -> Expr:
        return If(
            self.visit(i.cond, *args, **kwargs),
            self.visit(i.t_body, *args, **kwargs),
            self.visit(i.f_body, *args, **kwargs),
            type=i.type_,
        )

    def visit_assert(self, a: Assert, *args, **kwargs) -> Expr:
        return Assert(
            self.visit(a.cond, *args, **kwargs),
            self.visit(a.body, *args, **kwargs),
            type=a.type_,
        )
