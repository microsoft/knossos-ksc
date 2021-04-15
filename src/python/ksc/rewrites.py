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
        yield from self._all_rewrites_with_env(e, tuple(), e, {})
    
    def _all_rewrites_with_env(self, e: Expr, path_from_root: Location, root: Expr, env: Mapping[str, Location]) -> Iterator[Rewrite]:
        # Env maps bound variables to their binders, used for inline_let (only).
        yield from self.get_local_rewrites(e, path_from_root, root, env)
        for i, ch in enumerate(get_children(e)):
            yield from self._all_rewrites_with_env(ch, path_from_root + (i,), root, _update_env_for_subtree(e, path_from_root, i, env))

    @abstractmethod
    def get_local_rewrites(self, subtree: Expr, path_from_root: Location, root: Expr, env: Mapping) -> Iterator[Rewrite]:
        pass

@singledispatch
def _update_env_for_subtree(parent: Expr, parent_path: Location, which_child: int, env: Mapping[str, Location]) -> Mapping[str, Location]:
    # Default is to use same environment as parent
    return env

@_update_env_for_subtree.register
def _update_env_let(parent: Let, parent_path: Location, which_child: int, env: Mapping[str, Location]) -> Mapping[str, Location]:
    assert isinstance(parent.vars, Var), "Tupled lets are not supported - use untuple_lets first"
    assert 0 <= which_child <= 1
    return (env if which_child == 0 # rhs
        else {**env, parent.vars.name: parent_path})

@_update_env_for_subtree.register
def _update_env_lam(parent: Lam, parent_path: Location, which_child: int, env: Mapping[str, Location]) -> Mapping[str, Location]:
    assert which_child == 0
    return {k:v for k,v in env.items() if k != parent.arg.name}


_rule_dict: Mapping[str, "Rule"] = {}

def rule(name: str) -> "Rule":
    """Lookup method for all `Rule`s."""
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

@singleton
class inline_var(Rule):
    @staticmethod
    def possible_expr_classes():
        return {Var}

    def apply_at(self, expr: Expr, path_to_var: Location, binding_location: Location) -> Expr:
        # binding_location comes from the Rewrite.
        # Note there is an alternative design, where we don't store any "extra" info in the Rewrite.
        # Thus, at application time (here), we would have to first do an extra traversal all the way down path_to_var, to identify which variable to inline (and its binding location).
        # (Followed by the same traversal as here, that does renaming-to-avoid-capture from the binding location to the variable usage.)
        assert path_to_var[:len(binding_location)] == binding_location
        return replace_subtree(expr, binding_location, Const(0.0), # Nothing to avoid capturing in outer call
            lambda _zero, let: replace_subtree(let, path_to_var[len(binding_location):], let.rhs) # No applicator; renaming will prevent capturing let.rhs, so just insert that
        )

    def get_local_rewrites(self, subtree: Expr, path_from_root: Location, root: Expr, env: Mapping[str, Location]) -> Iterator[Rewrite]:
        if isinstance(subtree, Var): # May not be if rule is used directly outside of a RuleSet. Q: Do we want to support that case.
            if subtree.name in env:
                binding_loc = env[subtree.name]
                yield Rewrite(self, root, path_from_root, {"binding_location": binding_loc})


@singleton
class delete_let(Rule):
    @staticmethod
    def possible_expr_classes():
        return {Let}

    def apply_at(self, expr: Expr, path: Location) -> Expr:
        def apply_here(const_zero: Expr, let_node: Expr) -> Expr:
            assert const_zero == Const(0.0) # Passed to replace_subtree below
            assert let_node.vars.name not in let_node.body.free_vars_
            return let_node.body
        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(expr, path, Const(0.0), apply_here)

    def get_local_rewrites(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Rewrite]:
        del env
        if isinstance(subtree, Let) and subtree.vars.name not in subtree.body.free_vars_:
            yield Rewrite(self, root, path_from_root)
