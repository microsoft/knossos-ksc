from dataclasses import dataclass, field
from functools import singledispatch
from typing import Any, Iterator, Mapping, Tuple

from abc import ABC, abstractmethod
from ksc.expr import Expr, Let, Lam, Var, Const, Call
from ksc.cav_subst import Location, get_children, replace_subtree
from ksc.utils import singleton

@dataclass(frozen=True)
class Rewrite:
    rule: "Rule"
    root: Expr # RLO required rule to be passed to Rewrite.apply(), but this seems prone to accidents
    path: Location

    # Anything the Rule needs to pass from matching to rewriting. Used immutably, but dataclasses don't allow default {}
    extra_data: Mapping[str, Any] = field(default_factory=dict)

    def __call__(self):
        return self.rule.apply_at(self.root, self.path, **self.extra_data)
    
class AbstractMatcher(ABC):
    def get_all_rewrites(self, e: Expr) -> Iterator[Rewrite]:
        yield from _subtree_rewrites(e, [], self, e, {})

    @abstractmethod
    def get_local_rewrites(self, subtree: Expr, path_from_root: Location, root: Expr, env: Mapping) -> Iterator[Rewrite]:
        pass

_rule_dict: Mapping[str, "Rule"] = {}

def rule(name: str) -> "NamedRule":
    """Lookup method for all `NamedRule`s."""
    return _rule_dict[name]

class Rule(AbstractMatcher):
    def __init__(self, name=None):
        if name is None:
            name = self.__class__.__name__
        assert name not in _rule_dict
        _rule_dict[name] = self
        self._name = name

    @property
    def name(self):
        return self._name

    @abstractmethod
    def possible_expr_classes(self):
        """ Return a set of the subclasses of Expr that this rule could possibly match. """
        pass

    @abstractmethod
    def apply_at(self, expr: Expr, path: Location, **kwargs) -> Expr:
        """ Applies this rule at the specified <path> within <expr>. kwargs are any stored in the Rewrites. """

    def __reduce__(self):
        # This allows pickling and sending Rules across processes/machines via Ray.
        return (rule, (self.name,))

class RuleSet(AbstractMatcher):
    def __init__(self, rules):
        # TODO also allow global (any-class) rules?
        self.rules_by_class = {}
        for rule in rules:
            for clazz in rule.possible_expr_classes():
                self.rules_by_class.setdefault(clazz, []).append(rule)
    
    def get_local_rewrites(self, subtree: Expr, path_from_root: Location, root: Expr, env: Mapping) -> Iterator[Rewrite]:
        for rule in self.rules_by_class.get(subtree.__class__, []):
            yield from rule.get_local_rewrites(subtree, path_from_root, root, env)

def _subtree_rewrites(e: Expr, path_from_root: Location, rules: AbstractMatcher, root: Expr, env: Mapping[str, Tuple[Location, Expr]]) -> Iterator[Rewrite]:
    yield from rules.get_local_rewrites(e, path_from_root, root, env)
    yield from _subtree_update_env(e, path_from_root, rules, root, env)

# Note: python3.8 brings singledispatchmethod, which we could use here (moving this and the previous into AbstractMatcher). Sticking with python3.7.
@singledispatch
def _subtree_update_env(e: Expr, loc: Location, rules: AbstractMatcher, root: Expr, env: Mapping[str, Tuple[Location, Expr]]) -> Iterator[Rewrite]:
    for i, ch in enumerate(get_children(e)):
        yield from _subtree_rewrites(ch, loc + [i], rules, root, env)

@_subtree_update_env.register
def _subtree_env_let(e: Let, loc: Location, rules: AbstractMatcher, root: Expr, env: Mapping[str, Tuple[Location, Expr]]) -> Iterator[Rewrite]:
    yield from _subtree_rewrites(e.rhs, loc + [0], rules, root, env)
    def is_tuple(e):
        return isinstance(e, Call) and e.name.mangle_without_type() == "tuple"
    def _rm_dict_keys(d, ks):
        return {k:v for k, v in d.items() if k not in ks}
    child_env = ({**env, e.vars.name: (loc, e.rhs)} if isinstance(e.vars, Var)
                 else {**env, **{var.name: (loc, elem) for var,elem in zip(e.vars, e.rhs.args)}} if is_tuple(e.rhs) # Individual tuple elements
                 else _rm_dict_keys(env, [v.name for v in e.vars])) # Not individually inlinable
    yield from _subtree_rewrites(e.body, loc+[1], rules, root, child_env)

@_subtree_update_env.register
def _subtree_env_lam(e: Lam, loc: Location, rules: AbstractMatcher, root: Expr, env: Mapping[str, Tuple[Location, Expr]]) -> Iterator[Rewrite]:
    yield from _subtree_rewrites(e.body, loc + [0], rules, root, {k:v for k,v in env.items() if k != e.arg.name})


@singleton
class inline_var(Rule):
    @staticmethod
    def possible_expr_classes():
        return {Var}

    def apply_at(self, expr: Expr, path_to_var: Location, binding_location: Location, bound_value: Expr) -> Expr:
        # binding_location and bound_value come from the Rewrite.
        # Note alternatives:
        # 1. the environment+Rewrite store only the binding location; we carry the entire bound value from the binding-let down to the variable, in order to inline perhaps only part of it
        # 2. the environment+Rewrite store only the binding location; we traverse the path from binder to variable-use twice:
        #        the first time just to find the target variable, so that we can then extract the appropriate bound value (perhaps part of a tuple) from the binder;
        #        the second time, to do renaming to avoid capturing anything in the value to be inlined
        # 3. Neither environment/ReWrite store any extra information (neither binding_location nor bound_value); instead, we traverse the path from root to usage site twice,
        #        the first traversal building the environment (varname -> binding_location) as it goes down to the usage site - duplicating some of the work done by the matcher (which at the least needs to build an environment of which variables are inlinable)
        #        the second traversal uses two nested calls of replace_subtree: (a) from root down to the binder, without any renaming; then (b) from the binder down to the usage site, renaming to avoid capture of anything in the value-to-be-inlined.
        assert path_to_var[:len(binding_location)] == binding_location
        return replace_subtree(expr, binding_location, Const(0.0),
            lambda _payload, let: replace_subtree(let, path_to_var[len(binding_location):], bound_value) # No applicator: just take bound_value
        )

    def get_local_rewrites(self, subtree: Expr, path_from_root: Location, root: Expr, env: Mapping[str, Tuple[Location, Expr]]) -> Iterator[Rewrite]:
        if isinstance(subtree, Var): # May not be if rule is used directly outside of a RuleSet. Q: Do we want to support that case.
            if subtree.name in env:
                binding_loc, bound_val = env[subtree.name]
                yield Rewrite(self, root, path_from_root, {"binding_location": binding_loc, "bound_value": bound_val})


@singleton
class delete_let(Rule):
    @staticmethod
    def possible_expr_classes():
        return {Let}

    @staticmethod
    def _let_vars_used(expr: Let) -> bool:
        if isinstance(expr.vars, Var):
            return expr.vars.name in expr.body.free_vars_
        return any(v.name in expr.body.free_vars_ for v in expr.vars)


    def apply_at(self, expr: Expr, path: Location) -> Expr:
        # The constant here says that there are no free variables to avoid capturing en route to the target Location
        return replace_subtree(expr, path, Const(0.0), self._apply)

    def _apply(self, const_zero: Expr, let_node: Expr) -> Expr:
        assert const_zero == Const(0.0) # Specified in apply_at
        assert not self._let_vars_used(let_node)
        return let_node.body

    def get_local_rewrites(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Rewrite]:
        del env
        if isinstance(subtree, Let) and not self._let_vars_used(subtree):
            yield Rewrite(self, root, path_from_root)
