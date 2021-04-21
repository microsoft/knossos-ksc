from functools import singledispatchmethod
from ksc.expr import Expr, Let, Call, Var, If, Assert, Const, Lam

class ExprTransformer:
    """ Superclass for functions that transform Expressions by recursive traversal.
        The default "transformation" is the identity function, but each case first recursively transforms the sub-Expr's within the Expr,
        allowing easy overriding of specific cases.

        Subclasses must explicitly register their overrides in their constructors, as follows:

        def __init__(self):
            self.transform_foo = self.transform.register(self.transform_foo)
    """
    @singledispatchmethod
    def transform(self, e: Expr) -> Expr:
        # Extending to ASTNode should be straightforward.
        raise AssertionError("Overridden for all Expr subclasses")

    @transform.register
    def transform_var(self, v: Var) -> Expr:
        return v

    @transform.register
    def transform_const(self, c: Const) -> Expr:
        return c

    @transform.register
    def transform_if(self, i: If) -> Expr:
        return If(self.transform(i.cond), self.transform(i.t_body), self.transform(i.f_body))

    @transform.register
    def transform_call(self, c: Call) -> Expr:
        return Call(c.name, [self.transform(a) for a in c.args])

    @transform.register
    def transform_lam(self, l: Lam) -> Expr:
        # We *could* pass l.arg to self.transform too, checking arg.is_decl to detect arg being a binding occurrence;
        # or even have a separate method self.transform_binder.
        # But we expect subclasses to override transform_lam/let,
        # as any change to the bound variable is likely to have to be co-ordinated with the rest of the binding Expr.
        return Lam(l.arg, self.transform(l.body))

    @transform.register
    def transform_assert(self, a: Assert) -> Expr:
        return Assert(self.transform(a.cond), self.transform(a.body))

    @transform.register
    def transform_let(self, l: Let) -> Expr:
        return Let(l.vars, self.transform(l.rhs), self.transform(l.body))
