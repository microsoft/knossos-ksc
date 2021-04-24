from abc import ABC, abstractmethod, abstractproperty
from dataclasses import dataclass, field
from functools import singledispatch
from typing import Any, Iterator, Optional, Mapping, Tuple, Union, List, FrozenSet, Type as PyType

from ksc.cav_subst import Location, get_children, replace_subtree
from ksc.expr import Expr, Let, Lam, Var, Const, Call, ConstantType, StructuredName, Rule as KSRule
from ksc.parse_ks import parse_ks_file, parse_ks_string
from ksc.type import Type
from ksc.type_propagate import type_propagate
from ksc.utils import singleton, single_elem
from ksc.visitors import ExprTransformer

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
def match_filter(e : Expr) -> Union[PyType, ConstantType, StructuredName]:
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
    def possible_expr_filter(self) -> FrozenSet[Union[PyType, ConstantType, StructuredName]]:
        """ A set of values that might be returned by match_filter() for any Expr that this rule could possibly match. """

    @abstractmethod
    def apply_at(self, expr: Expr, path: Location, **kwargs) -> Expr:
        """ Applies this rule at the specified <path> within <expr>. kwargs are any stored in the Match's extra_data field. """

    @abstractmethod
    def matches_for_possible_expr(self, expr: Expr, path_from_root: Location, root: Expr, env: Environment) -> Iterator[Match]:
        """ Returns any 'Match's acting on the topmost node of the specified Expr, given that <match_filter(expr)>
            is of one of <self.possible_expr_filter>. """

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

def combine_substs(s1: Mapping[str, Expr], s2: Optional[Mapping[str, Expr]]) -> Optional[Mapping[str, Expr]]:
    if s2 is None:
        return None
    common_vars = s1.keys() & s2.keys()
    # We require all children to have exactly the same values (as this is not Most General Unification
    # - we are not finding substitutions for variables on the RHS).
    # Note this means that if the LHS template contains multiple let-expressions, they should have
    # distinct bound variables (or else will only match programs that also use the same variable-name
    # in both let's).
    if not all([s1[v] == s2[v] for v in common_vars]): # TODO use alpha-equivalence rather than strict equality
        return None # Fail
    s1.update(s2)
    return s1

@singledispatch
def fit_template(tmpl: Expr, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Mapping[str, Expr]]:
    """ Finds a substitution for the variable names in template_vars,
        such that applying the resulting substitution to <tmpl> (using subst_template) yields <exp>.
        Returns None if no such substitution exists i.e. the pattern does not match. """
    # Default case for most template exprs: require same type of Expr, and compatible child substitutions
    if tmpl.__class__ != exp.__class__:
        return None # No match
    tmpl_children = get_children(tmpl)
    exp_children = get_children(exp)
    if len(tmpl_children) != len(exp_children):
        return None
    d = dict()
    for t,e in zip(tmpl_children, exp_children):
        d = combine_substs(d, fit_template(t, e, template_vars))
        if d is None:
            return None
    return d

@fit_template.register
def fit_template_var(tmpl: Var, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Mapping[str, Expr]]:
    assert tmpl.name in template_vars
    # Require correct type of subexp in order to match
    return {tmpl.name: exp} if exp.type_ == template_vars[tmpl.name] else None

@fit_template.register
def fit_template_const(tmpl: Const, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Mapping[str, Expr]]:
    # Require same constant value. Empty substitution = success, None = no substitution = failure.
    return {} if tmpl == exp else None

def fit_template_binder(tmpl: Expr, exp: Expr, tmpl_v: Var, exp_v: Var, template_vars) -> Optional[Mapping[str, Expr]]:
    # Returns a substitution, assuming tmpl is inside a binder for tmpl_s and exp inside a corresponding binder for exp_v
    d = fit_template(tmpl.body, exp.body, {**template_vars, tmpl_v.name: exp_v.type_})
    if d is None:
        return None
    if d.pop(tmpl_v.name, exp_v) != exp_v:
        return None
    return d

@fit_template.register
def fit_template_let(tmpl: Let, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Mapping[str, Expr]]:
    assert isinstance(tmpl.vars, Var), "Tupled-lets in pattern are not supported: call untuple_lets first"
    if not isinstance(exp, Let):
        return None
    assert isinstance(exp.vars, Var), "Tupled-lets in subject expression are not supported: call untuple_lets first"
    assert tmpl.vars not in template_vars, "Let-bound variables should not be declared as pattern variables"
    d = fit_template_binder(tmpl.body, exp.body, tmpl.vars, exp.vars, template_vars)
    return d and combine_substs(d, fit_template(tmpl.rhs, exp.rhs, template_vars))

@fit_template.register
def fit_template_lam(tmpl: Lam, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Mapping[str, Expr]]:
    if not isinstance(exp, Lam):
        return None
    assert tmpl.arg not in template_vars, "Lambda arguments should not be declared as pattern variables"
    return fit_template_binder(tmpl.body, exp.body, tmpl.arg, exp.arg, template_vars)

@singleton
class SubstTemplate(ExprTransformer):
    """Substitutes variables to Exprs in a template (including bound
    variables). The substitution is capture-avoiding.

    Note that this is only avoids captures by new variables introduced on the RHS.
    It doesn't handle e.g. (foo (let x e1 e2)) ==> (let x e1 (foo e2))
    where there is potentially capture - if foo contains references to another/outside
    x, they'll be captured by the x bound by that let, which changes their meaning.
    (Hence, we still need the separate lift_bind rule (not a ParsedRule) for that.)
    """
    def visit_var(self, v: Var, var_names_to_exprs: Mapping[str, Expr]):
        assert not v.decl
        return var_names_to_exprs[v.name]

    def maybe_add_binder_to_subst(bound: Var, var_names_to_exprs: Mapping[str, Expr], dont_capture: List[Expr]
    )-> Tuple[Var, Mapping[str, Expr]]:
        assert bound.is_decl
        target_var = var_names_to_exprs.get(bound.name)
        if target_var is None:
            # This is a new binder in the RHS, so make sure the variable is
            # fresh w.r.t bound body and all RHSs of substitutions
            target_var = make_nonfree_var("t_", list(var_names_to_exprs.values()) + dont_capture, type=bound.type_)
            var_names_to_exprs = {**var_names_to_exprs, bound.name: target_var}
        return target_var, var_names_to_exprs

    def visit_let(self, l: Let, var_names_to_exprs: Mapping[str, Expr]) -> Let:
        assert isinstance(l.vars, Var), "use untuple_lets first"
        target_var, var_names_to_exprs = maybe_add_to_subst(l.vars, var_names_to_exprs, [l.body])
        # Substitute bound var with target_var in children. It's fine to apply this substitution outside
        # where the bound var is bound, as the pattern (RHS) can't contain "(let x ...) x" (with x free).
        return Let(Var(target_var.name, type=target_var.type_, decl=True),
            self.visit(l.rhs, var_names_to_exprs),
            self.visit(l.body, var_names_to_exprs))

    def visit_lam(self, l: Lam, var_names_to_exprs: Mapping[str, Expr]) -> Lam:
        target_var, var_names_to_exprs = maybe_add_to_subst(l.arg, var_names_to_exprs, [l.body])
        return Lam(Var(target_var.name, type=target_var.type_, decl=True),
            self.visit(l.body, var_names_to_exprs))

class ParsedRule(Rule):
    """ Matches and substitutes according to a monomorphic ksc.expr.Rule """
    def __init__(self, rule: KSRule):
        # The rule should already have been type-propagated (Call targets resolved to StructuredNames).
        assert rule.e1.type_ == rule.e2.type_ != None
        known_vars = frozenset([v.name for v in rule.args])
        # Check that all pattern variables must be arguments or in the symtab.
        assert known_vars.issuperset(rule.e1.free_vars_)
        # Same should be true of target (no new free variables, only new symtab calls)
        assert known_vars.issuperset(rule.e2.free_vars_)
        super().__init__(rule.name)
        self._rule = rule
        self._arg_types = {v.name: v.type_ for v in rule.args}

    @property
    def possible_expr_filter(self):
        return frozenset([match_filter(self._rule.e1)])

    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Match]:
        substs = fit_template(self._rule.e1, subtree, self._arg_types)
        if substs is not None:
            yield Match(self, root, path_from_root, {"substs": substs})

    def apply_at(self, expr: Expr, path: Location, substs: Mapping[str, Expr]) -> Expr:
        def apply_here(const_zero: Expr, target: Expr) -> Expr:
            assert const_zero == Const(0.0) # Passed to replace_subtree below
            assert SubstTemplate.visit(self._rule.e1, substs) == target # Note == traverses, so expensive.
            result = SubstTemplate.visit(self._rule.e2, substs)
            # Types copied from the pattern (down to the variables, and the subject-expr's types from there).
            # So there should be no need for any further type-propagation.
            assert result.type_ == target.type_
            return result
        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(expr, path, Const(0.0), apply_here)

def parse_rule_str(ks_str, symtab):
    r = single_elem(list(parse_ks_file(ks_str)))
    assert isinstance(r, KSRule)
    type_propagate(r, symtab)
    return ParsedRule(r)

def parse_rules_from_file(filename):
    with open(filename) as f:
        return [ParsedRule(r) for r in parse_ks_string(f, filename)]
