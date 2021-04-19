from dataclasses import dataclass, field
from functools import singledispatch
from typing import Any, Iterator, Mapping, Tuple, Union, Type, FrozenSet, final

from abc import ABC, abstractmethod, abstractproperty
from ksc.expr import Expr, Let, Lam, Var, Const, Call, ConstantType, StructuredName
from ksc.cav_subst import Location, get_children, replace_subtree
from ksc.utils import singleton

@dataclass(frozen=True)
class Match:
    rule: "Rule"
    expr: Expr # RLO required rule to be passed to Rewrite.apply(), but this seems prone to accidents
    path: Location

    # Anything the Rule needs to pass from matching to rewriting. Used immutably, but dataclasses don't allow default {}
    extra_data: Mapping[str, Any] = field(default_factory=dict)

    def rewrite(self):
        return self.rule.apply_at(self.expr, self.path, **self.extra_data)

Environment = Mapping[str, Location]

class AbstractMatcher(ABC):
    def find_all_matches(self, e: Expr) -> Iterator[Match]:
        yield from self._matches_with_env(e, tuple(), e, {})
    
    def _matches_with_env(self, e: Expr, path_from_root: Location, root: Expr, env: Environment) -> Iterator[Match]:
        # Env maps bound variables to their binders, used for inline_let (only).
        yield from self.matches_here(e, path_from_root, root, env)
        for i, ch in enumerate(get_children(e)):
            yield from self._matches_with_env(ch, path_from_root + (i,), root, _update_env_for_subtree(e, path_from_root, i, env))

    @abstractmethod
    def matches_here(self, subtree: Expr, path_from_root: Location, root: Expr, env: Environment) -> Iterator[Match]:
        """ Return any rewrites acting on the topmost node of the specified subtree """

@singledispatch
def _update_env_for_subtree(parent: Expr, parent_path: Location, which_child: int, env: Environment) -> Environment:
    # Default is to use same environment as parent
    return env

@_update_env_for_subtree.register
def _update_env_let(parent: Let, parent_path: Location, which_child: int, env: Environment) -> Environment:
    assert isinstance(parent.vars, Var), "Tupled lets are not supported - use untuple_lets first"
    assert 0 <= which_child <= 1
    return (env if which_child == 0 # rhs
        else {**env, parent.vars.name: parent_path})

@_update_env_for_subtree.register
def _update_env_lam(parent: Lam, parent_path: Location, which_child: int, env: Environment) -> Environment:
    assert which_child == 0
    return {k:v for k,v in env.items() if k != parent.arg.name}


_rule_dict: Mapping[str, "Rule"] = {}

def rule(name: str) -> "Rule":
    """Lookup method for all `Rule`s."""
    return _rule_dict[name]

@singledispatch
def match_filter(e : Expr) -> Union[Type, ConstantType, StructuredName]:
    # Allows to quick-reject rules that can never match a particular expr. See Rule.possible_expr_filter.
    return e.__class__

@match_filter.register
def match_filter_const(e : Const) -> ConstantType:
    return e.value

@match_filter.register
def match_filter_call(e : Call):
    return e.name

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

    @abstractproperty
    def possible_expr_filter(self) -> FrozenSet[Union[Type, ConstantType, StructuredName]]:
        """ A set of values that might be returned by match_filter() for any Expr that this rule could possibly match. """

    @abstractmethod
    def apply_at(self, expr: Expr, path: Location, **kwargs) -> Expr:
        """ Applies this rule at the specified <path> within <expr>. kwargs are any stored in the Match's extra_data field. """

    @abstractmethod
    def matches_for_possible_expr(self, expr: Expr, path_from_root: Location, root: Expr, env: Environment) -> Iterator[Match]:
        """ Returns any 'Match's acting on the topmost node of the specified Expr, given that <match_filter(expr)>
            is of one of <self.possible_expr_filter>. """

    @final
    def matches_here(self, expr: Expr, path_from_root: Location, root: Expr, env: Environment) -> Iterator[Match]:
        if match_filter(expr) in self.possible_expr_filter:
            yield from self.matches_for_possible_expr(expr, path_from_root, root, env)

    def __reduce__(self):
        # This allows pickling and sending Rules across processes/machines via Ray.
        return (rule, (self.name,))

class RuleSet(AbstractMatcher):
    def __init__(self, rules):
        # TODO also allow global (any-class) rules?
        self._filtered_rules = {}
        for rule in rules:
            for clazz in rule.possible_expr_filter:
                self._filtered_rules.setdefault(clazz, []).append(rule)

    def matches_here(self, subtree: Expr, path_from_root: Location, root: Expr, env: Environment) -> Iterator[Match]:
        for rule in self._filtered_rules.get(match_filter(subtree), []):
            yield from rule.matches_for_possible_expr(subtree, path_from_root, root, env)

@singleton
class inline_var(Rule):
    possible_expr_filter = frozenset([Var])

    def apply_at(self, expr: Expr, path_to_var: Location, binding_location: Location) -> Expr:
        # binding_location comes from the Match.
        # Note there is an alternative design, where we don't store any "extra_data" in the Match.
        # Thus, at application time (here), we would have to first do an extra traversal all the way down path_to_var, to identify which variable to inline (and its binding location).
        # (Followed by the same traversal as here, that does renaming-to-avoid-capture from the binding location to the variable usage.)
        assert path_to_var[:len(binding_location)] == binding_location
        return replace_subtree(expr, binding_location, Const(0.0), # Nothing to avoid capturing in outer call
            lambda _zero, let: replace_subtree(let, path_to_var[len(binding_location):], let.rhs) # No applicator; renaming will prevent capturing let.rhs, so just insert that
        )

    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env: Environment) -> Iterator[Match]:
        assert isinstance(subtree, Var)
        if subtree.name in env:
            binding_loc = env[subtree.name]
            yield Match(self, root, path_from_root, {"binding_location": binding_loc})


@singleton
class delete_let(Rule):
    possible_expr_filter = frozenset([Let])

    def apply_at(self, expr: Expr, path: Location) -> Expr:
        def apply_here(const_zero: Expr, let_node: Expr) -> Expr:
            assert const_zero == Const(0.0) # Passed to replace_subtree below
            assert let_node.vars.name not in let_node.body.free_vars_
            return let_node.body
        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(expr, path, Const(0.0), apply_here)

    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Match]:
        del env
        assert isinstance(subtree, Let)
        if subtree.vars.name not in subtree.body.free_vars_:
            yield Match(self, root, path_from_root)
