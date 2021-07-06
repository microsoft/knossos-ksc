from abc import ABC, abstractmethod, abstractproperty
from dataclasses import dataclass
from functools import singledispatch
from itertools import chain
from typing import Any, Dict, FrozenSet, Iterator, List, Mapping, Optional, Tuple

from pyrsistent import pmap
from pyrsistent.typing import PMap

from ksc.alpha_equiv import are_alpha_equivalent
from ksc.cav_subst import (
    replace_subtree,
    make_nonfree_var,
    VariableSubstitution,
)
from ksc.expr import (
    ConstantType,
    Def,
    StructuredName,
    Expr,
    Let,
    Lam,
    Var,
    Const,
    Call,
    Rule,
)
from ksc.filter_term import FilterTerm, get_filter_term
from ksc.parse_ks import parse_ks_file, parse_ks_string
from ksc.prim import make_prim_call
from ksc.path import Path, ExprWithPath, subexps_no_binds
from ksc.type import Type
from ksc.type_propagate import type_propagate
from ksc.untuple_lets import untuple_one_let
from ksc.utils import singleton, single_elem
from ksc.visitors import ExprTransformer

# A rule is, conceptually, some kind of formula for transforming an expression: it may be expressed as a Rule parsed from KS, or in python.
# In code, each such rule is an instance of RuleMatcher; class ParsedRuleMatcher deals with "Rule"s written in KS.
# Each place within expression that the RuleMatcher can be applied, is a "Match",
#   and each Match corresponds to exactly one "rewrite":  the process of actually producing the transformed expression.
# (Performing the rewrite, may be much more expensive than merely detecting that it is possible to do so:
# the Match records the latter, its apply_rewrite() method enacts the former.)


@dataclass(frozen=True)
class Match:
    rule: "RuleMatcher"
    ewp: ExprWithPath

    # Anything the RuleMatcher needs to pass from matching to rewriting.
    rule_specific_data: Mapping[str, Any] = pmap()

    def apply_rewrite(self):
        return self.rule.apply_at(self.ewp, **self.rule_specific_data)

    @property
    def path(self):
        return self.ewp.path


@dataclass(frozen=True, eq=False)
class Environment:
    """ Used to carry, around the Expr being matched, information about the context each (sub-)expr is in. """

    let_vars: PMap[str, Path]
    """ Variable(name)s bound by lets, to the locations of the let-nodes binding them """
    defs: PMap[StructuredName, Def]
    """ Names bound by Defs, including those from the prelude. These will be the same throughout the Expr. """


class AbstractMatcher(ABC):
    def find_all_matches(
        self, e: Expr, defs: PMap[StructuredName, Def] = pmap()
    ) -> Iterator[Match]:
        yield from self._matches_with_env(
            ExprWithPath.from_expr(e), Environment(pmap({}), defs)
        )

    def _matches_with_env(self, ewp: ExprWithPath, env: Environment) -> Iterator[Match]:
        # Env maps bound variables to their binders, and StructuredNames for their defs,
        # used only for inlining rules (inline_call and inline_var).
        yield from self.matches_here(ewp, env)
        if isinstance(ewp.expr, Let):
            yield from self._matches_with_env(ewp.rhs, env)
            yield from self._matches_with_env(
                ewp.body,
                Environment(env.let_vars.set(ewp.vars.name, ewp.path), env.defs),
            )
        elif isinstance(ewp.expr, Lam):
            yield from self._matches_with_env(
                ewp.body, Environment(env.let_vars.discard(ewp.arg.name), env.defs)
            )
        else:
            for ch in ewp.all_subexprs_with_paths():
                yield from self._matches_with_env(ch, env)

    @abstractmethod
    def matches_here(self, ewp: ExprWithPath, env: Environment,) -> Iterator[Match]:
        """ Return any matches which rewrite the topmost node of the specified subtree """


_rule_dict: Dict[str, "RuleMatcher"] = {}


def rule(name: str) -> "RuleMatcher":
    """Lookup method for all `RuleMatcher`s."""
    return _rule_dict[name]


class RuleMatcher(AbstractMatcher):
    name: str  # Should be immutable

    def __init__(self, name=None):
        if name is None:
            name = self.__class__.__name__
        assert name not in _rule_dict
        _rule_dict[name] = self
        self.name = name

    @abstractproperty
    def possible_filter_terms(self) -> FrozenSet[FilterTerm]:
        """ A set of terms that might be returned by get_filter_term() of any Expr for which this RuleMatcher
            could possibly generate a match. (See [Note: filter_term] in filter_term.py). """

    may_match_any_call: bool = False
    """ If a RuleMatcher (instance or subclass) returns true, indicates that it might match
        any Expr which is a Call, regardless of the value of get_filter_term() on that Expr. """

    @abstractmethod
    def apply_at(self, ewp: ExprWithPath, **kwargs) -> Expr:
        """ Applies this rule at the specified <path> within <expr>. kwargs are any stored in the Match's rule_specific_data field. """

    @abstractmethod
    def matches_for_possible_expr(
        self, ewp: ExprWithPath, env: Environment,
    ) -> Iterator[Match]:
        """ Returns any 'Match's acting on the topmost node of the specified Expr, given that <get_filter_term(expr)>
            is of one of <self.possible_filter_terms>. """

    def matches_here(self, ewp: ExprWithPath, env: Environment,) -> Iterator[Match]:
        if get_filter_term(ewp.expr) in self.possible_filter_terms or (
            isinstance(ewp.expr, Call) and self.may_match_any_call
        ):
            yield from self.matches_for_possible_expr(ewp, env)

    def __reduce__(self):
        # This allows pickling and sending RuleMatchers across processes/machines via Ray.
        return (rule, (self.name,))


class RuleSet(AbstractMatcher):
    """ Finds 'Match's for many rules (many different RuleMatcher objects) while performing
        only a single traversal of the Expr (and associated environment-building). """

    def __init__(self, rules):
        # As an optimization, at each node in the Expr tree, we'll look for matches only from
        # RuleMatchers whose possible_filter_terms match at that position in the tree.
        # (This checks equality of the outermost constructor of the template, but no deeper.)
        self._rules_by_filter_term: Dict[FilterTerm, List[RuleMatcher]] = {}
        self._any_call_rules: List[RuleMatcher] = []
        for rule in rules:
            if rule.may_match_any_call:
                self._any_call_rules.append(rule)
            for term in rule.possible_filter_terms:
                self._rules_by_filter_term.setdefault(term, []).append(rule)

    def matches_here(self, ewp: ExprWithPath, env: Environment,) -> Iterator[Match]:
        possible_rules = self._rules_by_filter_term.get(get_filter_term(ewp.expr), [])
        if isinstance(ewp.expr, Call):
            possible_rules = chain(possible_rules, self._any_call_rules)
        for rule in possible_rules:
            yield from rule.matches_for_possible_expr(ewp, env)


@singleton
class inline_var(RuleMatcher):
    possible_filter_terms = frozenset([Var])

    def apply_at(self, ewp: ExprWithPath, binding_location: Path) -> Expr:
        # binding_location comes from the Match.
        # Note there is an alternative design, where we don't store any "rule_specific_data" in the Match.
        # Thus, at application time (here), we would have to first do an extra traversal all the way down path_to_var, to identify which variable to inline (and its binding location).
        # (Followed by the same traversal as here, that does renaming-to-avoid-capture from the binding location to the variable usage.)
        assert ewp.path[: len(binding_location)] == binding_location
        return replace_subtree(
            ewp.root,
            binding_location,
            Const(0.0),  # Nothing to avoid capturing in outer call
            lambda _zero, let: replace_subtree(
                let, ewp.path[len(binding_location) :], let.rhs
            ),  # No applicator; renaming will prevent capturing let.rhs, so just insert that
        )

    def matches_for_possible_expr(
        self, ewp: ExprWithPath, env: Environment,
    ) -> Iterator[Match]:
        assert isinstance(ewp.expr, Var)
        binding_location = env.let_vars.get(ewp.expr.name)
        if binding_location is not None:
            yield Match(self, ewp, {"binding_location": binding_location})


@singleton
class inline_call(RuleMatcher):
    possible_filter_terms = frozenset()
    may_match_any_call = True

    def matches_for_possible_expr(
        self, ewp: ExprWithPath, env: Environment
    ) -> Iterator[Match]:
        func_def: Optional[Def] = env.defs.get(ewp.expr.name)
        if func_def is not None:
            yield Match(self, ewp, {"func_def": func_def})

    def apply_at(self, ewp: ExprWithPath, func_def: Def) -> Expr:
        # func_def comes from the Match.
        def apply_here(const_zero, call_node):
            # Drop decl=True from Def.args
            def_args = [Var(arg.name, type=arg.type_) for arg in func_def.args]
            call_arg = (
                call_node.args[0]
                if len(call_node.args) == 1
                else make_prim_call(StructuredName.from_str("tuple"), call_node.args)
            )
            return (
                Let(def_args[0], call_arg, func_def.body)
                if len(def_args) == 1
                else untuple_one_let(Let(def_args, call_arg, func_def.body))
            )

        arg_names = frozenset([arg.name for arg in func_def.args])
        assert func_def.body.free_vars_.issubset(
            arg_names
        )  # Sets may not be equal, if some args unused.
        # There is thus nothing in the function body that could be captured by 'let's around the callsite.
        # Thus, TODO: simplify interface to replace_subtree: only inline_let requires capture-avoidance, and
        # there is no need to support both capture-avoidance + applicator in the same call to replace_subtree.
        # In the meantime, the 0.0 here (as elsewhere) indicates there are no variables to avoid capturing.
        return replace_subtree(ewp.root, ewp.path, Const(0.0), apply_here)


@singleton
class delete_let(RuleMatcher):
    possible_filter_terms = frozenset([Let])

    def apply_at(self, ewp: ExprWithPath) -> Expr:
        def apply_here(const_zero: Expr, let_node: Expr) -> Expr:
            assert const_zero == Const(0.0)  # Passed to replace_subtree below
            assert let_node is ewp.expr
            assert let_node.vars.name not in let_node.body.free_vars_
            return let_node.body

        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(ewp.root, ewp.path, Const(0.0), apply_here)

    def matches_for_possible_expr(
        self, ewp: ExprWithPath, env: Environment
    ) -> Iterator[Match]:
        assert isinstance(ewp.expr, Let)
        if ewp.vars.name not in ewp.body.free_vars_:
            yield Match(self, ewp)


###############################################################################
# Rules parsed from KS. See class Rule (which has a shorter overview of syntax)
#


class ParsedRuleMatcher(RuleMatcher):
    """
    Matches and substitutes according to a monomorphic Rule parsed from .ks. These are of the form
        (rule "name" template_vars template replacement)
    for example a rule to effect a*(b+c) -> a*b+a*c would look like
        (rule "distrib_mul_over_add.t2f"
            ((a : Float) (b : Tensor 2 Float) (c : Tensor 2 Float)) ;; template_vars
            (mul a (add b c)) ;; template
            (add (mul a b) (mul b c)) ;; replacement
        )
    or, the inverse a*b+a*c-> a*(b+c)
        (rule "add_two_muls.double"
            ((a : Float) (b : Float)) ;; template_vars
            (add (mul a b) (mul a c)) ;; template --- note a occurs in multiple places, these must be identical
            (mul a (add b c)) ;; replacement
        )
    where:
         template_vars is a list of (name : Type) pairs
         template is an Expr, whose free vars are `template_vars`
         replacement is an Expr, whose free vars are a subset of `template_vars`
    """

    def __init__(self, rule: Rule, side_conditions=lambda **substs: True):
        # The rule should already have been type-propagated (Call targets resolved to StructuredNames).
        assert rule.template.type_ == rule.replacement.type_ != None
        known_vars = frozenset([v.name for v in rule.template_vars])
        # Check that all free variables in LHS and RHS templates are declared as arguments to the rule.
        assert known_vars == rule.template.free_vars_
        assert known_vars.issuperset(rule.replacement.free_vars_)
        # TODO: check that if there are multiple binders on the LHS, they all bind different names.
        super().__init__(rule.name)
        self._rule = rule
        self._arg_types = pmap({v.name: v.type_ for v in rule.template_vars})
        self._side_conditions = side_conditions

    @property
    def possible_filter_terms(self):
        return frozenset([get_filter_term(self._rule.template)])

    def matches_for_possible_expr(
        self, ewp: ExprWithPath, env: Environment
    ) -> Iterator[Match]:
        # The rule matches if there is a VariableSubstitution from the template_vars such that template[subst] == expr;
        # the result will then be replacement[subst].
        substs = find_template_subst(self._rule.template, ewp.expr, self._arg_types)
        if substs is not None and self._side_conditions(**substs):
            yield Match(self, ewp, substs)

    def apply_at(self, ewp: ExprWithPath, **substs: VariableSubstitution) -> Expr:
        def apply_here(const_zero: Expr, target: Expr) -> Expr:
            assert const_zero == Const(0.0)  # Passed to replace_subtree below
            assert are_alpha_equivalent(
                SubstPattern.visit(self._rule.template, substs), target
            )  # Note this traverses, so expensive.
            result = SubstPattern.visit(self._rule.replacement, substs)
            # Types copied from the template (down to the variables, and the subject-expr's types from there).
            # So there should be no need for any further type-propagation.
            assert result.type_ == target.type_
            return result

        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(ewp.root, ewp.path, Const(0.0), apply_here)


def _combine_substs(
    s1: VariableSubstitution, s2: Optional[VariableSubstitution]
) -> Optional[VariableSubstitution]:
    if s2 is None:
        return None
    common_vars = s1.keys() & s2.keys()
    # We require all children to have exactly the same values (as this is not Most General Unification
    # - we are not finding substitutions for variables on the RHS).
    # Note this means that if the LHS template contains multiple binders of the same name,
    # this will only match subject expressions that also use the same variable-name in all those binders.
    if not all(are_alpha_equivalent(s1[v], s2[v]) for v in common_vars):

        return None  # Fail
    s1.update(s2)
    return s1


@singledispatch
def find_template_subst(
    template: Expr, exp: Expr, template_vars: PMap[str, Type]
) -> Optional[VariableSubstitution]:
    """ Finds a substitution for the variable names in template_vars,
        such that applying the resulting substitution to <template> (using subst_template) yields <exp>.
        Returns None if no such substitution exists i.e. the <exp> does not match the <template>. """
    # Default case for most template exprs: require same type of Expr, and compatible child substitutions.
    # RuleSet will have ensured that the template and subject match at the outermost level,
    # but we still need to check that subtrees match too.
    if get_filter_term(template) != get_filter_term(exp):
        return None  # No match
    tmpl_children = subexps_no_binds(template)
    exp_children = subexps_no_binds(exp)
    if len(tmpl_children) != len(exp_children):
        return None
    d = dict()
    for t, e in zip(tmpl_children, exp_children):
        d = _combine_substs(d, find_template_subst(t, e, template_vars))
        if d is None:
            return None
    return d


@find_template_subst.register
def find_template_subst_var(
    template: Var, exp: Expr, template_vars: PMap[str, Type]
) -> Optional[VariableSubstitution]:
    assert template.name in template_vars
    # Require compatible type of subexp in order to match (the Rule's type may involve Any).
    return (
        {template.name: exp}
        if template_vars[template.name].can_accept_value_of_type(exp.type_)
        else None
    )


@find_template_subst.register
def find_template_subst_let(
    template: Let, exp: Expr, template_vars: PMap[str, Type]
) -> Optional[VariableSubstitution]:
    if not isinstance(exp, Let):
        return None
    assert isinstance(
        template.vars, Var
    ), "Tupled-lets in template are not supported: call untuple_lets first"
    assert isinstance(
        exp.vars, Var
    ), "Tupled-lets in subject expression are not supported: call untuple_lets first"
    assert (
        template.vars.name not in template_vars
    ), "Let-bound variables should not be declared as template variables"
    rhs_subst = find_template_subst(template.rhs, exp.rhs, template_vars)
    rhs_and_bound_subst = _combine_substs({template.vars.name: exp.vars}, rhs_subst)
    if rhs_and_bound_subst is None:
        return None
    # In the let-body, allow a substitution to be found for the let-bound variable; this will have to
    # map to the same variable bound in the expression as in the substitution above,
    # or _combine_substs will return None.
    body_subst = find_template_subst(
        template.body,
        exp.body,
        template_vars.set(template.vars.name, template.rhs.type_),
    )
    return _combine_substs(rhs_and_bound_subst, body_subst)


@find_template_subst.register
def find_template_subst_lam(
    template: Lam, exp: Expr, template_vars: PMap[str, Type]
) -> Optional[VariableSubstitution]:
    if not isinstance(exp, Lam):
        return None
    assert (
        template.arg.name not in template_vars
    ), "Lambda arguments should not be declared as template variables"
    if not template.arg.type_.can_accept_value_of_type(exp.arg.type_):
        return None
    body_subst = find_template_subst(
        template.body, exp.body, template_vars.set(template.arg.name, exp.arg.type_),
    )
    return _combine_substs({template.arg.name: exp.arg}, body_subst)


def _maybe_add_binder_to_subst(
    bound: Var, var_names_to_exprs: VariableSubstitution, dont_capture: List[Expr]
) -> Tuple[Var, VariableSubstitution]:
    # assert bound.decl # No - only for def args? - not true for 'Let's
    target_var = var_names_to_exprs.get(bound.name)
    if target_var is None:
        # This is a new binder in the RHS, so make sure the variable is
        # fresh w.r.t bound body and all RHSs of substitutions
        target_var = make_nonfree_var(
            "t_", list(var_names_to_exprs.values()) + dont_capture, type=bound.type_
        )
        var_names_to_exprs = {**var_names_to_exprs, bound.name: target_var}
    return target_var, var_names_to_exprs


@singleton
class SubstPattern(ExprTransformer):
    """Substitutes variables to Exprs in a pattern (typically the replacement part of a rule),
     including bound variables. The substitution is capture-avoiding.

    Note that this is only avoids captures by new variables introduced on the RHS.
    It doesn't handle e.g. (foo (let x e1 e2)) ==> (let x e1 (foo e2))
    where there is potentially capture - if foo contains references to another/outside
    x, they'll be captured by the x bound by that let, which changes their meaning.
    (Hence, we still need separate python RuleMatchers, not ParsedRuleMatchers, for e.g. lift_bind and sumbuild_invariant.)
    """

    def visit(self, e: Expr, var_names_to_exprs: VariableSubstitution) -> Expr:
        res = super().visit(e, var_names_to_exprs)
        # Type should have been copied from the rule. Replace Any types (from polymorphic rules)
        # with the correct monomorphic type for the expression.
        if res.type_.contains_any():
            # Remove type, so type propagation will recalculate just one level without recursion
            res.type_ = None
            # No symtab should be required: "Any" should only be used in rules for builtins which are
            # universally polymorphic, not for ad-hoc-overloaded functions from prelude etc.
            type_propagate(res, {}, respect_existing=True)
        return res

    def visit_var(self, v: Var, var_names_to_exprs: VariableSubstitution):
        assert not v.decl
        return var_names_to_exprs[v.name]

    def visit_let(self, l: Let, var_names_to_exprs: VariableSubstitution) -> Let:
        assert isinstance(l.vars, Var), "use untuple_lets first"
        target_var, var_names_to_exprs = _maybe_add_binder_to_subst(
            l.vars, var_names_to_exprs, [l.body]
        )
        # Substitute bound var with target_var in children. It's fine to apply this substitution outside
        # where the bound var is bound, as the replacement shouldn't contain "(let x ...) x" (with x free).
        return Let(
            Var(
                target_var.name
            ),  # type=target_var.type_, decl=True), # No, not generally set for Let-bound Vars
            self.visit(l.rhs, var_names_to_exprs),
            self.visit(l.body, var_names_to_exprs),
            type=l.type_,
        )

    def visit_lam(self, l: Lam, var_names_to_exprs: VariableSubstitution) -> Lam:
        target_var, var_names_to_exprs = _maybe_add_binder_to_subst(
            l.arg, var_names_to_exprs, [l.body]
        )
        return Lam(
            Var(target_var.name, type=target_var.type_, decl=True),
            self.visit(l.body, var_names_to_exprs),
            type=l.type_,
        )


def parse_rule_str(ks_str, symtab, **kwargs):
    r = single_elem(list(parse_ks_file(ks_str)))
    assert isinstance(r, Rule)
    type_propagate(r, symtab)
    return ParsedRuleMatcher(r, **kwargs)


def parse_rules_from_file(filename):
    with open(filename) as f:
        return [ParsedRuleMatcher(r) for r in parse_ks_string(f, filename)]
