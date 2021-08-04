# fmt: off
from abc import ABC, abstractmethod
from dataclasses import replace as dataclass_replace
from itertools import chain
import logging
from typing import Callable, Dict, Iterable, Sequence, List, Tuple, Optional, Mapping
import numpy as np # For constant-propagation rules for log/exp

from rlo.native_impls import native_impls
from rlo import sparser
from rlo.expression import Expression, EF
from rlo.expression_util import deep_copy_with_types, ExprWithEnv
from ksc.type import Type
from rlo.utils import Assert, uniquify, single_elem


class Rewrite(ABC):
    """ An operation that transforms an Expression.
        Applies a specific rule at a specific location.
        This can be identified by a pair of ints for the GNN (the rule by the index into the ruleset;
             the location as an AST node index within the Expression).
        Note that we use "action" and "rewrite" somewhat interchangeably;
           action is a term from RL, which applies to a state/node and thus (strictly speaking) includes decrementing time_left;
           rewrite is specific to Expressions.
    """
    def __init__(self, node_id, rule_obj):
        self._node_id = node_id
        self._rule = rule_obj
        assert self._rule is not None

    @property
    def node_id(self):
        return self._node_id

    @property
    def rule(self):
        return self._rule

    @property
    def rule_name(self):
        return str(self._rule)

    def apply(self, exprenv: ExprWithEnv) -> ExprWithEnv:
        """ Returns the result of transforming the specified top-level Expression.
            Default implementation applies the rule at the location indicated by self.node_id
        """
        return dataclass_replace(exprenv, expr=self.apply_expr(exprenv.expr))

    def apply_expr(self, exp: Expression) -> Expression:
        # _apply_to_subtree is a function, and default is no free variables, so there will be no capture-avoidance.
        # (This should mean the replaced subtree, will not be traversed before passing to _apply_to_subtree)
        return exp.replace_subtree(self.node_id, self._apply_to_subtree_typed)

    def _apply_to_subtree_typed(self, exp):
        """ Like _apply_to_subtree, but computes type for the new subtree,
            and checks the rewritten subtree has the same type. """
        res = self._apply_to_subtree(exp)
        if exp.type is not None:
            res = deep_copy_with_types(res, {}, respect_existing=True)
            assert res.type == exp.type
        return res

    @abstractmethod
    def _apply_to_subtree(self, exp):
        """ Apply the action at the top of the indicated subtree """


class RuleMatcher(ABC):
    def get_all_rewrites(self, exprenv: ExprWithEnv) -> Iterable[Rewrite]:
        """Possible actions using this rule or these rules throughout the given expression."""
        # In the future we can use symtab to initialize the environment with info from prelude
        return self.get_all_rewrites_expr(exprenv.expr)

    def get_all_rewrites_expr(self, expr: Expression) -> Iterable[Rewrite]:
        for node_id, node, env in expr.node_ids_with_environment():
            yield from self.get_local_rewrites(node, node_id, env=env)

    @abstractmethod
    def get_local_rewrites(
            self,
            subexp: Expression,
            node_id: int,
            env=None,
        ) -> Iterable[Rewrite]:
        """Possible actions using this rule at the specified subexpression."""


class Rule(RuleMatcher):
    """Interface and base implementation for all rules."""

    def may_yield(self, op: str) -> bool:  # pylint:disable=no-self-use
        """Indicates whether this rule may yield rewrites for the given op."""
        del op
        return True


_rule_dict: Dict[str, "NamedRule"] = dict()

def rule(name: str) -> "NamedRule":
    """Lookup method for all `NamedRule`s."""
    return _rule_dict[name]


class NamedRule(Rule):
    """
    Pickle-able rule implementation which enforces uniqueness of names.

    Creating an instance registers the given name to be looked up in `rule`.
    """

    def __init__(self, name=None):
        if name is None:
            name = self.__class__.__name__
        assert name not in _rule_dict
        _rule_dict[name] = self
        self.name = name

    def __reduce__(self):
        return (rule, (self.name,))

    def __str__(self):
        return self.name


class _LocalRewrite(Rewrite):
    def __init__(self, node_id, rule_obj):
        super().__init__(node_id, rule_obj)

    def _apply_to_subtree(self, e):
        return self._rule._apply(e)

class LocalRule(NamedRule):
    """ Local rules, i.e. that do not depend on environment,
        and can be expressed as functions on the expression to be rewritten. """
    def __init__(self, valid_op: str, name=None):
        self._valid_op = valid_op
        super().__init__(name)

    def may_yield(self, op: str) -> bool:
        return op == self._valid_op

    @abstractmethod
    def _is_applicable(self, exp: Expression) -> bool:
        pass

    @abstractmethod
    def _apply(self, exp: Expression) -> Expression:
        pass

    def get_local_rewrites(self, exp, node_id, **_):
        if self.may_yield(exp.op) and self._is_applicable(exp):
            yield _LocalRewrite(node_id, self)

def fit_template(templ: Expression, exp: Expression, template_vars: Sequence[str]):
    """Finds a substitution for the variable names in template_vars,
       such that applying the resulting substitution to <templ> (using subst_template) yields <exp>.
       Returns None if no such substitution exists i.e. the pattern does not match. """
    if templ.op == "variable":
        if templ.name in template_vars:
            return {templ.name: exp}
        if exp.op == "variable" and exp.name == templ.name:
            return dict() # Match, no substitution necessary
        return None # Fail to match
    if templ.op != exp.op or len(templ.children) != len(exp.children):
        return None # Fail
    if templ.op == "constant":
        return dict() if templ.value == exp.value else None
    d: Dict = dict()
    for t,e in zip(templ.children, exp.children):
        child_substs = fit_template(t, e, template_vars)
        if child_substs is None:
            return None # Fail
        common_vars = child_substs.keys() & d.keys()
        # We require all children to have exactly the same values (as this is not Most General Unification
        # - we are not finding substitutions for variables on the RHS).
        # Note this means that if the LHS template contains multiple let-expressions, they should have
        # distinct bound variables (or else will only match programs that also use the same variable-name
        # in both let's).
        if not all(child_substs[v] == d[v] for v in common_vars):
            return None # Fail
        # Only update d if not already present because only first occurrence of
        # a variable has type information in lambdas
        d.update({x: v for x, v in child_substs.items() if x not in d})
    return d

def subst_template(templ, var_names_to_exprs):
    """Substitutes variables to Expressions in a template (including bound
    variables). The substitution is capture-avoiding.

    Note that this is only avoids captures by new variables introduced on the RHS.
    It doesn't handle e.g. (foo (let x e1 e2)) ==> (let x e1 (foo e2))
    where there is potentially capture - if foo contains references to another/outside
    x, they'll be captured by the x bound by that let, which changes their meaning.
    (Hence, we still need the separate lift_bind rule (not a ParsedRule) for that.)
    """
    if templ.op == "variable":
        # If there's no mapping, variable refers to a primitive/edef and should not be renamed
        return var_names_to_exprs.get(templ.name, templ)
    elif templ.op == "constant":
        return templ
    elif templ.is_binder and templ.bound_var.name not in var_names_to_exprs:
        # This is a new binder in the RHS, so make sure the variable is
        # fresh w.r.t bound body and all RHSs of substitutions
        exprs = chain(
            (c for i, c in enumerate(templ.children) if templ.binds_in_child(i)),
            (e for _, e in var_names_to_exprs.items()),
        )
        fresh_var = Expression.new_var(*exprs, type=templ.bound_var.type)
        # Substitute bound var with fresh_var in children:
        var_names_to_exprs = {**var_names_to_exprs, templ.bound_var.name: fresh_var}
    new_children = [subst_template(c, var_names_to_exprs) for c in templ.children]
    # At least one child of the template should be affected
    # (there are no templates containing 1+1, only 1+e)
    assert any(id(n) != id(c) for n,c in zip(new_children, templ.children))
    return templ.clone_with_new_children(new_children)

class _ApplySubst(Rewrite):
    def __init__(self, node_id, parsed, subst):
        super().__init__(node_id, parsed)
        self._subst = subst

    def _apply_to_subtree(self, _):
        return subst_template(self._rule.rhs, self._subst)

class ParsedRule(NamedRule):
    """A rule expressed as two expressions, a template (whose variables will be substituted
       to match the subject program), and a target (onto which said substitution will be
       applied again to give the result). The template and target can be parsed from an S-Expression
       of the form (rule name (args) from to), for a rule where (only) the variables in <args> are substitutable;
       other variables in the template/pattern must refer to functions in the symtab provided.

       Note the types on the args must pass typechecking, but do *not* restrict the actual types of any match.
    """
    def __init__(self, rule_ks, symtab:Optional[Mapping[str, Type]]=None):
        (name, self._var_types, self.lhs, self.rhs) = sparser.parse_rule(rule_ks)
        if symtab is None:
            known_vars = frozenset(self._var_types.keys())
        else:
            assert self._var_types.keys().isdisjoint(symtab.keys())
            known_vars = frozenset(symtab.keys()).union(self._var_types.keys())
            # Assign types to nodes in RHS.
            # For symtab variables, we could do this by overriding _apply_to_subtree_typed, but that would copy those vars every time the rule was applied, whereas this does so only once;
            # for pattern variables, this is necessary for deep_copy_with_types, which then provides a sanity check that the types specified in the rules do at least pass typechecking.
            types = {**self._var_types, **symtab}
            self.rhs = deep_copy_with_types(self.rhs, types)
        # Check that all pattern variables must be arguments or in the symtab.
        assert known_vars.issuperset(self.lhs.free_var_names)
        # Same should be true of target (no new free variables, only new symtab calls)
        assert known_vars.issuperset(self.rhs.free_var_names)
        # When we do substitution, also include (allow substituting for) variables bound by let/etc. in the pattern:
        bound_vars = frozenset([n.bound_var.name for n in self.lhs.nodes if n.is_binder])
        assert bound_vars.isdisjoint(self._var_types.keys()), "Bound variables should not be declared as parameters"
        self._vars = bound_vars.union(self._var_types.keys())
        super().__init__(name)

    def may_yield(self, op: str) -> bool:
        return self.lhs.op == "variable" or op == self.lhs.op

    def get_local_rewrites(self, exp, node_id, env, **_):
        if exp.op == "stop": return
        substs = fit_template(self.lhs, exp, self._vars)
        if substs is not None:
            assert subst_template(self.lhs, substs) == exp
            yield _ApplySubst(node_id, self, substs)

    def __repr__(self):
        return "(rule {} {} {})".format(self.name, self.lhs, self.rhs)

class _RewriteChild(Rewrite):
    def __init__(self, child_id, parent_id, which_c, rp_rule):
        super().__init__(child_id, rp_rule)
        self._parent_id = parent_id
        self._which_c = which_c
    def apply_expr(self, exp: Expression):
        return exp.replace_subtree(self._parent_id, self._apply_to_subtree_typed)
    def _apply_to_subtree(self, exp):
        # exp is the parent node
        parent_copy_fn = lambda ch: exp.clone_with_new_children(
            exp.children[:self._which_c] + [ch] + exp.children[self._which_c+1:])
        return self._rule.replace_parent(exp.children[self._which_c],
            parent_copy_fn, self._which_c, parent=exp)

class ReplaceParentRule(NamedRule):
    """ Allows a rule that
            * identifies the location (to the GNN) as being a node that is the child of a parent
            * when evaluated, produces a node that replaces *the parent* (rather than the child).
        This allows e.g. a rule that acts on any child of a tuple (of which there may be arbitrarily many),
        but replaces the entire tuple. E.g.
           op(let x=e in e', let x2=e2 in e2') ==>
              let x=e in op(e', let x2=e2 in e2') [node_id identifies the first let]
              let x2=e2 in op(let x=e in e', e2') [node_id identifies the second let]
            op(if p then x else y, if q then a else b) ==>
              if p then op(x, if q then a else b) else op(y, if q then a else b) [node_id identifies the first if]
              if q then op(if p then x else y, a) else op(if p then x else y, b) [node_id identifies the second if] """

    @abstractmethod
    def check_applicable(self, e, parent=None, env=None):
        pass

    def get_local_rewrites(self, exp, node_id, env):
        child_id = node_id + 1
        for which_c, c in enumerate(exp.children):
            if self.check_applicable(c, parent=exp, env=env):
                yield _RewriteChild(child_id, node_id, which_c, self)
            child_id += c.num_nodes

    @abstractmethod
    def replace_parent(self, e, parent_copy_fn, idx_in_parent, parent):
        """ e: the child node, identified by the node_id for this action
            parent: parent node of e, which will actually be replaced
            idx_in_parent: index such that parent.children[idx_in_parent] is e
            parent_copy_fn: a function that, given an expression e', creates a copy of the parent but
                with the child e replaced by e' and the symtab removed. (Thus, parent_copy_fn(e) == parent.)
        """

class RuleSet(Sequence[Rule], RuleMatcher):
    """A container for an ordered set of rules with optimized `get_local_rewrites`."""
    def __init__(self, rules: Iterable[Rule], name=None):
        self.rules_name = name
        self.rules = tuple(rules)
        self._rule_ids = {rule: i for i,rule in enumerate(self.rules)}
        self._may_yields = {
            op: tuple(rule for i, rule in enumerate(self.rules) if rule.may_yield(op))
            for op in Expression.node_types
        }
        if name is not None:
            assert name not in _rule_sets
            _rule_sets[name] = self

    def __len__(self):
        return len(self.rules)

    def __getitem__(self, i):
        return self.rules[i]

    def __add__(self, other):
        assert isinstance(other, RuleSet)
        return RuleSet(self.rules + other.rules)

    def get_local_rewrites(self, node: Expression, node_id: int, env=None):
        for r in self._may_yields[node.op]:
            yield from r.get_local_rewrites(node, node_id, env=env)

    def id_for_rule(self, rule_obj: Rule) -> int:
        return self._rule_ids[rule_obj]

    def __iter__(self):
        return iter(self.rules)

    def __repr__(self):
        return f"RuleSet<{self.rules_name or self.rules}>"

_rule_sets: Dict[str, RuleSet] = {}


def get_rules(rules_name: str) -> RuleSet:
    out = _rule_sets.get(rules_name)
    if out is None:
        raise ValueError(
                f"{rules_name} not a registered RuleSet. Available names are "
                f"{available_rules()}")
    return out

def available_rules():
    return sorted(_rule_sets.keys())

#TODO: clean up expand rules
_expand_rules = [
    # If we want to use these two, we *may* (or may not) need to rewrite to target at the parent of the expression,
    # to prevent them acting on the outermost expression or immediately within a bind (as we expect rewriting
    # in those positions not to help optimization).
    ParsedRule("(rule exp_add_zero (e : Float) e (add 0.0 e))"),
    ParsedRule("(rule exp_mul_one (e : Float) e (mul 1.0 e))"),
]

def _algebraic_rules_helper(rule_suffix:str, arg_type:str, op_suffix:str, c:Callable=float, symtab=None):
    """ Return a list of monomorphic algebraic rules for a single type.
            - c: python type constructor for creating values of the appropriate type, e.g. int/float
    """
    zero = c(0)
    one = c(1)
    r = rule_suffix
    o = op_suffix
    t = arg_type
    return [ParsedRule(r, symtab=symtab) for r in [
        f"(rule add_zero{r} (a : {t}) (add {zero} a) a)",
        f"(rule mul_zero{r} (a : {t}) (mul{o} {zero} a) {zero})", # (if (isinfinite a) NaN 0.0) for float
        f"(rule mul_one{r} (a : {t}) (mul{o} {one} a) a)",
        f"(rule sub_zero{r} (a : {t}) (sub{o} a {zero}) a)",
        f"(rule div_one{r} (a : {t}) (div{o} a {one}) a)",
        f"(rule add_neg_to_sub{r} ((a : {t}) (b : {t})) (add{o} a (mul{o} b {c(-1)})) (sub{o} a b))",
        f"(rule commute_mul{r} ((a : {t}) (b : {t})) (mul{o} a b) (mul{o} b a))",
        f"(rule commute_add{r} ((a : {t}) (b : {t})) (add{o} a b) (add{o} b a))",
        f"(rule sub_self{r} (a : {t}) (sub{o} a a) {zero})",
        f"(rule assoc_add_add{r} ((a : {t}) (b : {t}) (c : {t})) (add{o} (add{o} a b) c) (add{o} (add{o} a c) b))",
        f"(rule assoc_sub_add{r} ((a : {t}) (b : {t}) (c : {t})) (sub{o} (add{o} a b) c) (add{o} (sub{o} a c) b))",
        f"(rule assoc_add_sub{r} ((a : {t}) (b : {t}) (c : {t})) (add{o} (sub{o} a b) c) (sub{o} (add{o} a c) b))",
        f"(rule assoc_sub_sub{r} ((a : {t}) (b : {t}) (c : {t})) (sub{o} (sub{o} a b) c) (sub{o} (sub{o} a c) b))",
        # We seem to be missing (a - b) - c <=> a - (b + c) ??
        f"(rule sub_sub_to_sub_add{r} ((a : {t}) (b : {t}) (c : {t})) (sub{o} a (sub{o} b c)) (sub{o} (add{o} a c) b))",
        f"(rule assoc_mul_mul{r} ((a : {t}) (b : {t}) (c : {t})) (mul{o} (mul{o} a b) c) (mul{o} (mul{o} a c) b))",
        f"(rule mul_by_add{r} ((a : {t}) (b : {t}) (c : {t})) (mul{o} a (add{o} b c)) (add{o} (mul{o} a b) (mul{o} a c)))",
        f"(rule mul_by_sub{r} ((a : {t}) (b : {t}) (c : {t})) (mul{o} a (sub{o} b c)) (sub{o} (mul{o} a b) (mul{o} a c)))",
        f"(rule add_of_muls{r} ((a : {t}) (b : {t}) (c : {t})) (add{o} (mul{o} a b) (mul{o} a c)) (mul{o} a (add{o} b c)))",
        f"(rule sub_of_muls{r} ((a : {t}) (b : {t}) (c : {t})) (sub{o} (mul{o} a b) (mul{o} a c)) (mul{o} a (sub{o} b c)))",
    ]]

def _float_rules(rule_suffix, op_suffix, symtab=None):
    r = rule_suffix
    o = op_suffix
    return [ParsedRule(r, symtab=symtab) for r in [
        # Floating-point-only rules. These need the name suffix to distinguish them
        # from the polymorphic variants in _algebraic_rules.
        f"(rule assoc_div_mul{r} ((a : Float) (b : Float) (c : Float)) (div{o} (mul{o}  a b) c) (mul{o}  (div{o}  a c) b))",
        f"(rule assoc_mul_div{r} ((a : Float) (b : Float) (c : Float)) (mul{o}  (div{o}  a b) c) (div{o}  (mul{o}  a c) b))",
        f"(rule div_by_div{r} ((a : Float) (b : Float) (c : Float)) (div{o}  a (div{o}  b c)) (div{o}  (mul{o}  a c) b))",
        f"(rule add_of_divs{r} ((a : Float) (b : Float) (c : Float)) (add{o}  (div{o}  a c) (div{o}  b c)) (div{o}  (add{o}  a b) c))",
        f"(rule sub_of_divs{r} ((a : Float) (b : Float) (c : Float)) (sub{o}  (div{o}  a c) (div{o}  b c)) (div{o}  (sub{o}  a b) c))",
        f"(rule assoc_div_div{r} ((a : Float) (b : Float) (c : Float)) (div{o}  (div{o}  a b) c) (div{o}  (div{o}  a c) b))",
        f"(rule div_of_div{r} ((a : Float) (b : Float) (c : Float)) (div{o}  (div{o}  a b) c) (div{o}  a (mul{o}  b c)))",
        f"(rule div_by_mul{r} ((a : Float) (b : Float) (c : Float)) (div{o}  a (mul{o}  b c)) (div{o}  (div{o}  a b) c))",
    ]]

_mono_algebraic_rules = (_algebraic_rules_helper("$i", "Integer", "$ii", int, sparser.default_symtab)
    + _algebraic_rules_helper("$f", "Float", "$ff", float, sparser.default_symtab)
    + _float_rules("$f", "$ff", sparser.default_symtab)
)

_algebraic_rules = (
    # Note the argument-type declarations are only the type of value with which the rule is tested;
    # the rule is applicable to any Expression with the correct node_type, no matter what 'Type's.
    _algebraic_rules_helper("", "Float", "", float)
    + _float_rules("", "")
)

# Decorator for classes: creates one instance only
# We are not using utils.singleton because of the rewrites-specific asserts below
# These asserts can't be moved into NamedRule.__init__ because they wouldn't
# apply to instances of ParsedRule
def singleton(cls):
    instance = cls()
    assert isinstance(instance, NamedRule)
    assert instance.name == cls.__name__
    return None  # The value that goes into globals() for this module; so,
    # prevents the rule being directly accessed as rewrites.foo, instead must use rewrites.rule("foo")

class _InlineLetRewrite(Rewrite):
    def __init__(self, node_id, binder_id, rule_obj):
        super().__init__(node_id, rule_obj)
        self._binder_id = binder_id

    def apply_expr(self, exp: Expression):
        # Override to replace the *binding* subtree...
        return exp.replace_subtree(self._binder_id, self._apply_to_subtree_typed)

    def _apply_to_subtree(self, exp):
        # The bound value is computed in the context here, at the root of the binder - there is no recursion yet -
        # so we must make sure that nothing between the binder and the usage site redefines (rebinds) any variables
        # in the bound (to-be-inlined) value. Hence, use capture-avoiding substitution from here down.
        assert exp.op == "let"
        bound_val = exp.second
        assert bound_val.without_stop().op != "lam" # First-order
        assert self._node_id > self._binder_id + 1 + exp.second.num_nodes # not the first child, nor the bound value (no recursion yet)
        assert self._node_id <= self._binder_id + exp.num_nodes
        return exp.replace_subtree(self._node_id - self._binder_id, bound_val)


@singleton
class inline_let(NamedRule):
    def may_yield(self, op: str) -> bool:
        return op == "variable"

    def get_local_rewrites(self, exp, node_id, env):
        if (exp.op == "variable") and (exp.name in env):
            binder_id, bound_val = env[exp.name]
            if bound_val.without_stop().op != "lam": # Preserve compatibility with first-order KSC
                yield _InlineLetRewrite(node_id, binder_id, self)

@singleton
class delete_let(LocalRule):
    def __init__(self):
        super().__init__(valid_op="let")

    def _is_applicable(self, exp):
        return not exp.third.involves_var(exp.first)

    def _apply(self, exp):
        return exp.third


@singleton
class delete_def(LocalRule):
    def __init__(self):
        super().__init__(valid_op="let")

    def _is_applicable(self, exp):
        return exp.second.without_stop().op == "lam" and not exp.third.involves_var(exp.first)

    def _apply(self, exp):
        return exp.third


def delete_unused_defs(expr):
    while True:
        for rewrite in rule("delete_def").get_all_rewrites(expr):
            expr = rewrite.apply(expr)
            break # Only apply first rule, then look for applicable again
        else:
            return expr # No rules found

class _InlineCallRewrite(_InlineLetRewrite):
    def _apply_to_subtree(self, exp):
        assert exp.op == "let"
        # The function lives in the context here, at the root of its binder - there is no recursion yet -
        # so we must make sure that nothing between the binder and the usage site redefines (rebinds) any variables
        # in the function body. Hence, use capture-avoiding substitution from here down.
        func = exp.second
        assert func.op == "lam"
        assert self._node_id > self._binder_id + 1 + exp.second.num_nodes # not the first child, nor the bound value (no recursion yet)
        assert self._node_id <= self._binder_id + exp.num_nodes
        # Preserve the meaning of the free variables in func.right, but not in apply_node.right (use their meaning at the apply node, not the function binder)
        return exp.replace_subtree(self._node_id - self._binder_id,
            lambda apply_node: Assert(apply_node.op == "apply").then_return(EF.Let(func.left, apply_node.right, func.right)),
            func.right)

@singleton
class inline_call(NamedRule):
    def may_yield(self, op: str) -> bool:
        return op == "apply"

    def get_local_rewrites(self, exp, node_id, env):
        if exp.op != "apply": return #note Apply(f_arg, Stop(arg)) can be optimized to let f_arg=Stop(arg) in ...body_of_f....
        assert exp.left.op == "variable" # We have restricted to first-order KSC language
        # OR do we allow e.g. ((if p f g) x)
        # for predicate p, and def-bound functions f, g ?
        if exp.left.name not in env: return
        binder_id, bound_val = env[exp.left.name]
        assert bound_val.without_stop().op == "lam" # First-order.
        if bound_val.op == "lam":
            yield _InlineCallRewrite(node_id, binder_id, self)


@singleton
class cprop_add(LocalRule):
    def __init__(self):
        super().__init__(valid_op="add")

    def _is_applicable(self, exp):
        return exp.left.op == "constant" and exp.right.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(exp.left.value + exp.right.value)

@singleton
class cfold(LocalRule):
    def __init__(self):
        super().__init__(valid_op="apply")

    def _is_applicable(self, exp):
        assert exp.op == "apply" and exp.left.op == "variable" # First order, 
        return (exp.left.name in native_impls) and (
            exp.right.op == "constant" or (
                exp.right.op == "tuple" and all(c.op == "constant" for c in exp.right.children)
            )
        )

    def _apply(self, exp):
        native_impl = native_impls[exp.left.name]
        # Check types match. "declared_type" of Constants is set by Expression.Constant.
        func_type = sparser.default_symtab[exp.left.name]
        if exp.right.op == "constant":
            assert exp.right.type == func_type.lam_arg_type
            constant_args = [exp.right]
        else:
            assert len(list(func_type.lam_arg_type.tuple_elems())) == len(exp.right.children)
            assert all(exp == cst.type for exp,cst in zip(func_type.lam_arg_type.children, exp.right.children))
            constant_args = exp.right.children
        native_result = native_impl(*[c.value for c in constant_args])
        return Expression.Constant(native_result)

@singleton
class cprop_sub(LocalRule):
    def __init__(self):
        super().__init__(valid_op="sub")

    def _is_applicable(self, exp):
        return exp.left.op == "constant" and exp.right.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(exp.left.value - exp.right.value)

@singleton
class cprop_mul(LocalRule):
    def __init__(self):
        super().__init__(valid_op="mul")

    def _is_applicable(self, exp):
        return exp.left.op == "constant" and exp.right.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(exp.left.value * exp.right.value)

@singleton
class cprop_div(LocalRule):
    def __init__(self):
        super().__init__(valid_op="div")

    def _is_applicable(self, exp):
        return exp.left.op == "constant" and exp.right.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(exp.left.value / exp.right.value)

@singleton
class div_by_self(LocalRule):
    def __init__(self):
        super().__init__(valid_op="div")

    def _is_applicable(self, exp):
        return exp.left == exp.right and (exp.left.op != "constant" or exp.left.value != 0.0)

    def _apply(self, exp):
        return Expression.Constant(1.0)


# These are rules that cannot be expressed in KSC syntax as they need side conditions or primitives of the underlying (host) language
_native_simplify_rules = [
    rule("cprop_add"),
    rule("cprop_sub"),
    rule("cprop_mul"),
    rule("cprop_div"),
    rule("inline_let"),
    rule("delete_let"),
    rule("div_by_self"),
]

_simplify_rules = _algebraic_rules + _native_simplify_rules

@singleton
class cprop_log(LocalRule):
    def __init__(self):
        super().__init__(valid_op="log")

    def _is_applicable(self, exp):
        return exp.only_child.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(np.log(exp.only_child.value))

@singleton
class cprop_exp(LocalRule):
    def __init__(self):
        super().__init__(valid_op="exp")

    def _is_applicable(self, exp):
        return exp.only_child.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(np.exp(exp.only_child.value))

_log_exp_rules = [
    # TODO these should all produce if-tests for overflow/underflow/NaN
    ParsedRule("(rule log_of_exp (a : Float) (log (exp a)) a)"),
    ParsedRule("(rule exp_of_log (a : Float) (exp (log a)) a)"),
    rule("cprop_log"),
    rule("cprop_exp"),
    ParsedRule("(rule exp_of_add ((a : Float) (b : Float)) (exp (add a b)) (mul (exp a) (exp b)))"),
    ParsedRule("(rule mul_of_exps ((a : Float) (b : Float)) (mul (exp a) (exp b)) (exp (add a b)))"),
    ParsedRule("(rule log_of_mul ((a : Float) (b : Float)) (log (mul a b)) (add (log a) (log b)))"),
    ParsedRule("(rule add_of_logs ((a : Float) (b : Float)) (add (log a) (log b)) (log (mul a b)))"),
    #TODO (op (exp a) b) <=> (op a (log b)) for op in < > (maybe ==) etc.?
]

@singleton
class lift_if(ReplaceParentRule):
    """ Roughly: <parent>(...if(p,x,y)...) => if(p, <parent>(....x...), <parent>(...y...)) """
    @staticmethod
    def check_applicable(e, parent=None, **_):
        return (parent is not None
                and parent.op != "lam" # Preserve compatibility with first-order KSC
                and not (parent.op == "let" and parent.second.op == "lam")
                # Make sure that it does not lift above the function definitions. If parent is a let binding top-level functions, we cannot lift over that.
                and e.op == "if"
                and not parent.binds_any_in(e.first))

    @staticmethod
    def replace_parent(e, parent_copy_fn, idx_in_parent, parent, **_):
        return EF.If(e.first, parent_copy_fn(e.second), parent_copy_fn(e.third))

@singleton
class cprop_eq(LocalRule):
    def __init__(self):
        super().__init__(valid_op="eq")

    def _is_applicable(self, exp):
        return exp.left.op == "constant" and exp.right.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(exp.left.eval() == exp.right.eval())

@singleton
class cprop_gt(LocalRule):
    def __init__(self):
        super().__init__(valid_op="gt")

    def _is_applicable(self, exp):
        return exp.left.op == "constant" and exp.right.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(exp.left.eval() > exp.right.eval())


@singleton
class cprop_gteq(LocalRule):
    def __init__(self):
        super().__init__(valid_op="gte")

    def _is_applicable(self, exp):
        return exp.left.op == "constant" and exp.right.op == "constant"

    def _apply(self, exp):
        return Expression.Constant(exp.left.eval() >= exp.right.eval())


_if_rules = [
    ParsedRule("(rule if_true ((a : Float) (b : Float)) (if true a b) a)"),
    ParsedRule("(rule if_false ((a : Float) (b : Float)) (if false a b) b)"),
    ParsedRule("(rule if_return_p (p : Bool) (if p true false) p)"),
    ParsedRule("(rule if_both_same ((p : Bool) (a : Float)) (if p a a) a)"),
    ParsedRule("(rule eq_self (a : Float) (eq a a) true)"),
    rule("cprop_eq"),
    rule("cprop_gt"),
    rule("cprop_gteq"),
    rule("lift_if")
]

def is_zero_vec(e):
    return  (e.op == "constant" and e.value == 0.0) or (e.op == "constVec" and is_zero_vec(e.right))

@singleton
class select_of_tuple(LocalRule):
    def __init__(self):
        super().__init__(valid_op="select")

    def _is_applicable(self, exp):
        return exp.left.op == "tuple"

    def _apply(self, exp):
        return exp.left.children[exp.right.value]

@singleton
class sumbuild_other_deltavec(LocalRule):
    def __init__(self):
        super().__init__(valid_op="sumbuild")

    def _is_applicable(self, exp):
        return exp.third.op == "deltaVec" and not exp.third.first.involves_var(exp.second) and not exp.third.second.involves_var(exp.second)

    def _apply(self, exp):
        return EF.DeltaVec(exp.third.first, exp.third.second, EF.Sumbuild(exp.first, exp.second, exp.third.third))

@singleton
class sumbuild_of_tuple(LocalRule):
    def __init__(self):
        super().__init__(valid_op="sumbuild")

    def _is_applicable(self, exp):
        return exp.third.op == "tuple"

    def _apply(self, exp):
        return EF.Tuple(*(EF.Sumbuild(exp.first, exp.second, elem) for elem in exp.third.children))

@singleton
class add_tuples(LocalRule):
    def __init__(self):
        super().__init__(valid_op="add")

    def _is_applicable(self, exp):
        return exp.left.op == "tuple" and exp.right.op == "tuple" and len(exp.left.children) == len(exp.right.children)

    def _apply(self, exp):
        return EF.Tuple(*(a + b for a, b in zip(exp.left.children, exp.right.children)))


@singleton
class mul_tuple_scalar(LocalRule):
    def __init__(self):
        super().__init__(valid_op="mul")

    def _is_applicable(self, exp):
        return exp.left.op == "tuple"

    def _apply(self, exp):
        return EF.Tuple(*(elem * exp.right for elem in exp.left.children))


@singleton
class sumbuild_invariant(LocalRule):
    def __init__(self):
        super().__init__(valid_op="sumbuild")

    def _is_applicable(self, exp):
        return not exp.third.involves_var(exp.second)

    def _apply(self, exp):
        return EF.To_float(exp.first) * exp.third


@singleton
class sumbuild_of_zero_vec(LocalRule):
    def __init__(self):
        super().__init__(valid_op="sumbuild")

    def _is_applicable(self, exp):
        return not exp.third.involves_var(exp.second) and is_zero_vec(exp.third)

    def _apply(self, exp):
        return exp.third

@singleton
class sumbuild_of_sum(LocalRule):
    def __init__(self):
        super().__init__(valid_op="sumbuild")

    def _is_applicable(self, exp):
        return exp.third.op == "add"

    def _apply(self, exp):
        return EF.Sumbuild(exp.first, exp.second, exp.third.left) + EF.Sumbuild(exp.first, exp.second, exp.third.right)

_vector_tuple_rules = [
    # A set of rules to optimize programs with vectors.
    # Note Sumbuild If ... -> If Sumbuild is covered by lift_if rule.
    # We need these rules to optimize machine learning programs.
    # Also some rules involving tuples.
    ParsedRule("(rule size_of_build ((n : Integer) (b : Any)) (size (build n (lam x b))) n)"),
    ParsedRule("(rule index_of_build ((arg : Integer) (sz : Integer) (body : Any)) (index arg (build sz (lam x body))) (let x arg body))"),
    rule("select_of_tuple"),
    # should this be sum_of_build ?
    ParsedRule("(rule mk_sumbuild ((n : Integer) (e : Any)) (sum (build n (lam x e))) (sumbuild n (lam x e)))"),
    rule("sumbuild_other_deltavec"),
    ParsedRule("(rule sumbuild_of_deltavec ((n : Integer) (e : Any)) (sumbuild n (lam x (deltaVec n x e))) (build n (lam x e)))"),
    rule("sumbuild_of_tuple"),
    rule("add_tuples"),
    rule("mul_tuple_scalar"),
    rule("sumbuild_invariant"),
    rule("sumbuild_of_zero_vec"),
    # should this be add_of_deltavecs ?
    ParsedRule("(rule sum_of_deltavecs ((n : Integer) (i : Integer) (e1 : Any) (e2 : Any)) (add (deltaVec n i e1) (deltaVec n i e2)) (deltaVec n i (add e1 e2)))"),
    rule("sumbuild_of_sum"),
    # should this be assoc_sumbuild_build ? - we have this (but not the inverse build_of_sumbuild) because we are trying to push sumbuilds inwards to meet deltaVecs
    ParsedRule("(rule sumbuild_of_build ((o : Integer) (n : Integer) (e : Any)) (sumbuild o (lam oi (build n (lam ni e)))) (build n (lam ni (sumbuild o (lam oi e)))))"),
]

@singleton
class new_bind(ReplaceParentRule):
    """ This acts upon the parent but only to ensure the parent exists and satisfies side conditions.
        Roughly, it's <parent>(...e...) => <parent>(...let x=e in x...) """
    @staticmethod
    def check_applicable(e, parent=None, **_):
        return (parent is not None
                and parent.op != "lam" # Preserve compatibility with first-order KSC
                and parent.op != "let" # these two maybe temporary, but they avoid a potentially-infinite
                and e.op != "let" # sequence of new_bind's, whereas build/lam admit just one.
                and e.num_nodes > 1) # rules out variable + constant

    @staticmethod
    def replace_parent(e, parent_copy_fn, idx_in_parent, parent, **_):
        nv = Expression.new_var(e)
        return parent_copy_fn(EF.Let(nv, e, nv))

@singleton
class lift_bind(ReplaceParentRule):
    """ This lifts a bind over a parent, that is, <parent>(...let x=e in e'...) => let x=e in <parent>(...e'...) """
    @staticmethod
    def check_applicable(e, parent=None, **_):
        return (parent is not None
                and parent.op != "lam" # Preserve compatibility with first-order KSC
                and not (parent.op == "let" and parent.second.op == "lam")
                # Make sure that it does not lift above the function definitions. If parent is a let binding top-level functions, we cannot lift over that.
                and e.op == "let"
                and not parent.binds_any_in(e.second))
    @staticmethod
    def replace_parent(e, parent_copy_fn, idx_in_parent, parent, **_):
        # We know (from check_applicable) that e.second does not actually use any value bound by the parent, so is fine to evaluate outside of it.
        # We also know that the parent cannot (obviously) use any value bound in the child.
        # However, if the parent references another variable with the same name as that bound by the child,
        # we must rename the child-binding to preserve the meaning of the parent.
        if any(sibling.involves_var(e.first.name) for i,sibling in enumerate(parent.children) if i != idx_in_parent):
            nv = Expression.new_var(parent)
            return EF.Let(nv, e.second, parent_copy_fn(e.third.rename_free(e.first.name, nv)))
        return EF.Let(e.first, e.second, parent_copy_fn(e.third))

@singleton
class cse_bind(LocalRule):
    def __init__(self):
        super().__init__(valid_op="let")

    def _is_applicable(self, exp):
        return (exp.third.op == "let" and exp.second == exp.third.second
            and not exp.third.second.involves_var(exp.first)) # Rule out inner `let` using value from outer: let x=x+1 in let x=x+1 in ....

    def _apply(self, exp):
        return EF.Let(exp.first, exp.second,
            exp.third.third if exp.third.first==exp.first # Both let's bind the same name, so trivially remove
                else exp.third.third.rename_free(exp.third.first.name, exp.first))

_binding_rules = [
    rule("new_bind"),
    rule("lift_bind"),
    rule("cse_bind"),
    # should this be inline_var? (inline_let would inline all occurrences at once, impossible if recursive)
    rule("inline_let"),
    rule("delete_let"),
    # Also a "recovery" rule to allow undoing bad let-introductions without climbing over the massive "let x=e in e" hill.
    # Ideally we'd like the model to learn to climb the hill or learn these "shortcuts" but we don't have that facility yet.
    ParsedRule("(rule let_recover (e : Any) (let x e x) e)")
]

simplify_rules = RuleSet(_simplify_rules, "simplify_rules")
binding_rules = RuleSet(_binding_rules, "binding_rules")
if_rules = RuleSet(_if_rules, "if_rules")
log_exp_rules = RuleSet(_log_exp_rules, "log_exp_rules")
build_simplify_rules = RuleSet(uniquify(
    [*_vector_tuple_rules, *_simplify_rules, rule("inline_call")]), "build_simplify_rules")
binding_simplify_rules = RuleSet(
    uniquify([*_simplify_rules, *_binding_rules]), "binding_simplify_rules")
ml_rules = RuleSet(uniquify(
        [*_simplify_rules, *_binding_rules, *_vector_tuple_rules, *_if_rules, rule("inline_call")]),
    "ml_rules")
ml_rules_no_bind = RuleSet(uniquify([
    *_simplify_rules, *_vector_tuple_rules, *_if_rules, rule("inline_call")]), "ml_rules_no_bind")
vector_tuple_rules = RuleSet(uniquify([
    *_vector_tuple_rules, rule("inline_let"), rule("inline_call"), rule("delete_let")
]), "vector_tuple_rules")
monomorphic_rules = RuleSet(_mono_algebraic_rules + [rule("cfold")],
    "monomorphic_rules")

def rewrite_seq_to_exprenvs(
    exprenv: ExprWithEnv,
    rewrite_seq : Iterable[Tuple[int, str]],
    strict:bool=False) -> List[ExprWithEnv]:
    """ Computes costs given initial expression expr and rewrite sequence rewrite_seq.
        rewrite_seq: list of elements [node_id : int, rule_name: str]
        strict: True to raise an exception if a rewrite in the sequence does not apply; False to skip the rewrite """
    def follow_sequence(exprenv):
        yield exprenv
        for (node_id, rule_name) in rewrite_seq:
            rewrites = list(rule(rule_name).get_all_rewrites(exprenv))
            if any(rw.node_id == node_id for rw in rewrites):
                rewrite = single_elem([rw for rw in rewrites if rw.node_id == node_id])
                assert rewrite.rule_name == rule_name
                exprenv = rewrite.apply(exprenv)
                yield exprenv
            else:
                msg = (f"Rule {rule_name} did not apply in location {node_id}. Applicable locations were: "
                    f"{[rw.node_id for rw in rewrites]} in expression: {exprenv.expr}")
                logging.getLogger(__name__).warn(msg)
                if strict:
                    raise ValueError(msg)
    
    return list(follow_sequence(exprenv))
