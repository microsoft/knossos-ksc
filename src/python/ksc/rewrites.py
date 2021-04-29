from abc import ABC, abstractmethod, abstractproperty
from dataclasses import dataclass, field
from functools import singledispatch
from typing import Any, Iterator, Optional, Mapping, Protocol, Tuple, List, FrozenSet, TypeVar

from pyrsistent import pmap, PMap as _PMap

from ksc.cav_subst import Location, get_children, replace_subtree, make_nonfree_var
from ksc.expr import Expr, Let, Lam, Var, Const, Call, ConstantType, StructuredName, Rule
from ksc.filter_term import FilterTerm, get_filter_term
from ksc.parse_ks import parse_ks_file, parse_ks_string
from ksc.type import Type
from ksc.type_propagate import type_propagate
from ksc.utils import singleton, single_elem
from ksc.visitors import ExprTransformer

@dataclass(frozen=True)
class Match:
    rule: "RuleMatcher"
    expr: Expr # RLO required rule to be passed to Rewrite.apply(), but this seems prone to accidents
    path: Location

    # Anything the RuleMatcher needs to pass from matching to rewriting. Used immutably, but dataclasses don't allow default {}
    rule_specific_data: Mapping[str, Any] = field(default_factory=dict)

    def apply_rewrite(self):
        return self.rule.apply_at(self.expr, self.path, **self.rule_specific_data)


K = TypeVar("K")
V = TypeVar("V")
# This "class" exists only to be used as a type annotation, because pyrsistent's "PMap" type is not parameterized.
# It should be a Procotol, but Protocols can only inherit from other Protocols (PMap is concrete).
# If frozenmap (https://www.python.org/dev/peps/pep-0603/) makes it into a future version of python, we should switch to that.
# (However, said PEP is *not* in python 3.9).
class PMap(_PMap, Mapping[K,V]):
    pass

# Environments that map variable names to the locations of the nodes binding them.
LetBindingEnvironment = PMap[str, Location]

class AbstractMatcher(ABC):
    def find_all_matches(self, e: Expr) -> Iterator[Match]:
        yield from self._matches_with_env(e, tuple(), e, pmap({}))

    def _matches_with_env(self, e: Expr, path_from_root: Location, root: Expr, env: LetBindingEnvironment) -> Iterator[Match]:
        # Env maps bound variables to their binders, used for inline_let (only).
        yield from self.matches_here(e, path_from_root, root, env)
        for i, ch in enumerate(get_children(e)):
            yield from self._matches_with_env(ch, path_from_root + (i,), root, _update_env_for_subtree(e, path_from_root, i, env))

    @abstractmethod
    def matches_here(self, subtree: Expr, path_from_root: Location, root: Expr, env: LetBindingEnvironment) -> Iterator[Match]:
        """ Return any matches which rewrite the topmost node of the specified subtree """

@singledispatch
def _update_env_for_subtree(parent: Expr, parent_path: Location, which_child: int, env: LetBindingEnvironment) -> LetBindingEnvironment:
    # Default is to use same environment as parent
    return env

@_update_env_for_subtree.register
def _update_env_let(parent: Let, parent_path: Location, which_child: int, env: LetBindingEnvironment) -> LetBindingEnvironment:
    assert isinstance(parent.vars, Var), "Tupled lets are not supported - use untuple_lets first"
    assert 0 <= which_child <= 1
    return (env if which_child == 0 # rhs
        else env.set(parent.vars.name, parent_path))

@_update_env_for_subtree.register
def _update_env_lam(parent: Lam, parent_path: Location, which_child: int, env: LetBindingEnvironment) -> LetBindingEnvironment:
    assert which_child == 0
    return env.remove(parent.arg.name)


_rule_dict: Mapping[str, "RuleMatcher"] = {}

def rule(name: str) -> "RuleMatcher":
    """Lookup method for all `RuleMatcher`s."""
    return _rule_dict[name]

class RuleMatcher(AbstractMatcher):
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
    def possible_filter_terms(self) -> FrozenSet[FilterTerm]:
        """ A set of terms that might be returned by get_filter_term() for any Expr that this rule could possibly match. """

    @abstractmethod
    def apply_at(self, expr: Expr, path: Location, **kwargs) -> Expr:
        """ Applies this rule at the specified <path> within <expr>. kwargs are any stored in the Match's rule_specific_data field. """

    @abstractmethod
    def matches_for_possible_expr(self, expr: Expr, path_from_root: Location, root: Expr, env: LetBindingEnvironment) -> Iterator[Match]:
        """ Returns any 'Match's acting on the topmost node of the specified Expr, given that <get_filter_term(expr)>
            is of one of <self.possible_filter_terms>. """

    def matches_here(self, expr: Expr, path_from_root: Location, root: Expr, env: LetBindingEnvironment) -> Iterator[Match]:
        if get_filter_term(expr) in self.possible_filter_terms:
            yield from self.matches_for_possible_expr(expr, path_from_root, root, env)

    def __reduce__(self):
        # This allows pickling and sending RuleMatchers across processes/machines via Ray.
        return (rule, (self.name,))

class RuleSet(AbstractMatcher):
    def __init__(self, rules):
        # TODO also allow global (any-class) rules?
        self._filtered_rules = {}
        for rule in rules:
            for term in rule.possible_filter_terms:
                self._filtered_rules.setdefault(term, []).append(rule)

    def matches_here(self, subtree: Expr, path_from_root: Location, root: Expr, env: LetBindingEnvironment) -> Iterator[Match]:
        for rule in self._filtered_rules.get(get_filter_term(subtree), []):
            yield from rule.matches_for_possible_expr(subtree, path_from_root, root, env)

@singleton
class inline_var(RuleMatcher):
    possible_filter_terms = frozenset([Var])

    def apply_at(self, expr: Expr, path_to_var: Location, binding_location: Location) -> Expr:
        # binding_location comes from the Match.
        # Note there is an alternative design, where we don't store any "rule_specific_data" in the Match.
        # Thus, at application time (here), we would have to first do an extra traversal all the way down path_to_var, to identify which variable to inline (and its binding location).
        # (Followed by the same traversal as here, that does renaming-to-avoid-capture from the binding location to the variable usage.)
        assert path_to_var[:len(binding_location)] == binding_location
        return replace_subtree(expr, binding_location, Const(0.0), # Nothing to avoid capturing in outer call
            lambda _zero, let: replace_subtree(let, path_to_var[len(binding_location):], let.rhs) # No applicator; renaming will prevent capturing let.rhs, so just insert that
        )

    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env: LetBindingEnvironment) -> Iterator[Match]:
        assert isinstance(subtree, Var)
        if subtree.name in env:
            binding_loc = env[subtree.name]
            yield Match(self, root, path_from_root, {"binding_location": binding_loc})


@singleton
class delete_let(RuleMatcher):
    possible_filter_terms = frozenset([Let])

    def apply_at(self, expr: Expr, path: Location) -> Expr:
        def apply_here(const_zero: Expr, let_node: Expr) -> Expr:
            assert const_zero == Const(0.0) # Passed to replace_subtree below
            assert let_node.vars.name not in let_node.body.free_vars_
            return let_node.body
        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(expr, path, Const(0.0), apply_here)

    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Match]:
        assert isinstance(subtree, Let)
        if subtree.vars.name not in subtree.body.free_vars_:
            yield Match(self, root, path_from_root)

# Substitutions from the names of variables (arguments) in the rule template to Exprs
Subst = Mapping[str, Expr]

def _combine_substs(s1: Subst, s2: Optional[Subst]) -> Optional[Subst]:
    if s2 is None:
        return None
    common_vars = s1.keys() & s2.keys()
    # We require all children to have exactly the same values (as this is not Most General Unification
    # - we are not finding substitutions for variables on the RHS).
    # Note this means that if the LHS template contains multiple binders of the same name,
    # this will only match subject expressions that also use the same variable-name in all those binders.
    if not all([s1[v] == s2[v] for v in common_vars]): # TODO use alpha-equivalence rather than strict equality
        return None # Fail
    s1.update(s2)
    return s1

@singledispatch
def find_template_subst(template: Expr, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Subst]:
    """ Finds a substitution for the variable names in template_vars,
        such that applying the resulting substitution to <template> (using subst_template) yields <exp>.
        Returns None if no such substitution exists i.e. the <exp> does not match the <template>. """
    # Default case for most template exprs: require same type of Expr, and compatible child substitutions.
    # RuleSet will have ensured that the template and subject match at the outermost level,
    # but we still need to check that subtrees match too.
    if get_filter_term(template) != get_filter_term(exp):
        return None # No match
    tmpl_children = get_children(template)
    exp_children = get_children(exp)
    if len(tmpl_children) != len(exp_children):
        return None
    d = dict()
    for t,e in zip(tmpl_children, exp_children):
        d = _combine_substs(d, find_template_subst(t, e, template_vars))
        if d is None:
            return None
    return d

@find_template_subst.register
def find_template_subst_var(template: Var, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Subst]:
    assert template.name in template_vars
    # Require correct type of subexp in order to match
    return {template.name: exp} if exp.type_ == template_vars[template.name] else None

@find_template_subst.register
def find_template_subst_let(template: Let, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Subst]:
    if not isinstance(exp, Let):
        return None
    assert isinstance(template.vars, Var), "Tupled-lets in template are not supported: call untuple_lets first"
    assert isinstance(exp.vars, Var), "Tupled-lets in subject expression are not supported: call untuple_lets first"
    assert template.vars.name not in template_vars, "Let-bound variables should not be declared as template variables"
    d = {template.vars.name: exp.vars}
    d = _combine_substs(d, find_template_subst(template.rhs, exp.rhs, template_vars))
    return d and _combine_substs(d, find_template_subst(template.body, exp.body, {**template_vars, template.vars.name: template.rhs.type_}))

@find_template_subst.register
def find_template_subst_lam(template: Lam, exp: Expr, template_vars: Mapping[str, Type]) -> Optional[Subst]:
    if not isinstance(exp, Lam):
        return None
    assert template.arg not in template_vars, "Lambda arguments should not be declared as template variables"
    if template.arg.type_ != exp.arg.type_:
        return None
    return find_template_subst(template.body, exp.body, {**template_vars, template.arg.name: template.arg.type_})

def _maybe_add_binder_to_subst(bound: Var, var_names_to_exprs: Subst, dont_capture: List[Expr]
    )-> Tuple[Var, Subst]:
        #assert bound.decl # No - only for def args? - not true for 'Let's
        target_var = var_names_to_exprs.get(bound.name)
        if target_var is None:
            # This is a new binder in the RHS, so make sure the variable is
            # fresh w.r.t bound body and all RHSs of substitutions
            target_var = make_nonfree_var("t_", list(var_names_to_exprs.values()) + dont_capture, type=bound.type_)
            var_names_to_exprs = {**var_names_to_exprs, bound.name: target_var}
        return target_var, var_names_to_exprs

@singleton
class SubstTemplate(ExprTransformer):
    """Substitutes variables to Exprs in a template (including bound variables).
    The substitution is capture-avoiding.

    Note that this is only avoids captures by new variables introduced on the RHS.
    It doesn't handle e.g. (foo (let x e1 e2)) ==> (let x e1 (foo e2))
    where there is potentially capture - if foo contains references to another/outside
    x, they'll be captured by the x bound by that let, which changes their meaning.
    (Hence, we still need separate python RuleMatchers, not ParsedRuleMatchers, for e.g. lift_bind and sumbuild_invariant.)
    """
    def visit_var(self, v: Var, var_names_to_exprs: Subst):
        assert not v.decl
        return var_names_to_exprs[v.name]

    def visit_let(self, l: Let, var_names_to_exprs: Subst) -> Let:
        assert isinstance(l.vars, Var), "use untuple_lets first"
        target_var, var_names_to_exprs = _maybe_add_binder_to_subst(l.vars, var_names_to_exprs, [l.body])
        # Substitute bound var with target_var in children. It's fine to apply this substitution outside
        # where the bound var is bound, as the RHS template can't contain "(let x ...) x" (with x free).
        res = Let(Var(target_var.name),# type=target_var.type_, decl=True), # No, not generally set for Let-bound Vars
            self.visit(l.rhs, var_names_to_exprs),
            self.visit(l.body, var_names_to_exprs))
        res.type_ = l.type_
        return res

    def visit_lam(self, l: Lam, var_names_to_exprs: Subst) -> Lam:
        target_var, var_names_to_exprs = _maybe_add_binder_to_subst(l.arg, var_names_to_exprs, [l.body])
        res = Lam(Var(target_var.name, type=target_var.type_, decl=True),
            self.visit(l.body, var_names_to_exprs))
        res.type_ = l.type_
        return res

class ParsedRuleMatcher(RuleMatcher):
    """ Matches and substitutes according to a monomorphic Rule parsed from .ks """
    def __init__(self, rule: Rule):
        # The rule should already have been type-propagated (Call targets resolved to StructuredNames).
        assert rule.e1.type_ == rule.e2.type_ != None
        known_vars = frozenset([v.name for v in rule.args])
        # Check that all free variables in LHS and RHS templates are declared as arguments to the rule.
        assert known_vars.issuperset(rule.e1.free_vars_)
        assert known_vars.issuperset(rule.e2.free_vars_)
        # TODO: it would be good to check here that any variables *bound* in the LHS template are not declared
        # as rule arguments, as we require that at rewriting-time:
        # WRITE: (rule "foo" ((x : Float)) (let (a x) a) x)
        # NOT:   (rule "foo" ((a : Float) (x : Float)) (let (a x) a) x)
        super().__init__(rule.name)
        self._rule = rule
        self._arg_types = {v.name: v.type_ for v in rule.args}

    @property
    def possible_filter_terms(self):
        return frozenset([get_filter_term(self._rule.e1)])

    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Match]:
        substs = find_template_subst(self._rule.e1, subtree, self._arg_types)
        if substs is not None:
            yield Match(self, root, path_from_root, substs)

    def apply_at(self, expr: Expr, path: Location, **substs: Subst) -> Expr:
        def apply_here(const_zero: Expr, target: Expr) -> Expr:
            assert const_zero == Const(0.0) # Passed to replace_subtree below
            assert SubstTemplate.visit(self._rule.e1, substs) == target # Note == traverses, so expensive.
            result = SubstTemplate.visit(self._rule.e2, substs)
            # Types copied from the template (down to the variables, and the subject-expr's types from there).
            # So there should be no need for any further type-propagation.
            assert result.type_ == target.type_
            return result
        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(expr, path, Const(0.0), apply_here)

def parse_rule_str(ks_str, symtab):
    r = single_elem(list(parse_ks_file(ks_str)))
    assert isinstance(r, Rule)
    type_propagate(r, symtab)
    return ParsedRuleMatcher(r)

def parse_rules_from_file(filename):
    with open(filename) as f:
        return [ParsedRuleMatcher(r) for r in parse_ks_string(f, filename)]
