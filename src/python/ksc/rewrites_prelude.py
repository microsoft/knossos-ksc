from typing import Callable, FrozenSet, Iterator

from ksc.cav_subst import replace_subtree
from ksc.expr import StructuredName, Expr, Const, Call
from ksc.filter_term import FilterTerm
from ksc.interpreter import native_impls
from ksc.path import ExprWithPath
from ksc.rewrites import RuleMatcher, Environment, Match_XYZ

###############################################################################
# Constant-folding
#


class ConstantFolder(RuleMatcher):
    def __init__(self, name: StructuredName, native_impl: Callable):
        self.possible_filter_terms = frozenset([name])
        super().__init__("cfold_" + name.mangled())
        self._name = name
        self._native_impl = native_impl

    def possible_filter_terms(self) -> FrozenSet[FilterTerm]:
        return frozenset([self._name])

    def matches_for_possible_expr(
        self, ewp: ExprWithPath, env: Environment,
    ) -> Iterator[Match_XYZ]:
        assert isinstance(ewp.expr, Call) and ewp.name == self._name
        if all(isinstance(arg, Const) for arg in ewp.expr.args):

            def apply() -> Expr:
                def apply_here(const_zero: Expr, subtree: Expr):
                    assert const_zero == Const(
                        0.0
                    )  # Payload passed to replace_subtree below
                    assert isinstance(subtree, Call) and subtree.name == self._name
                    return Const(
                        self._native_impl(*[arg.value for arg in subtree.args])
                    )

                return replace_subtree(ewp.root, ewp.path, Const(0.0), apply_here)

            yield Match_XYZ(ewp=ewp, apply_rule=apply)


constant_folding_rules = [ConstantFolder(sn, func) for sn, func in native_impls.items()]
