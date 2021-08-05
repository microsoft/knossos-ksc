from typing import Dict, Iterable, List, Mapping, NamedTuple, Optional, Sequence, Tuple
from functools import reduce
import collections
import itertools
import numpy as np
import operator
import random

from rlo.expression import Expression
from rlo.expression_util import SymtabAndDefs, ExprWithEnv
from rlo import rewrites
from rlo import utils
from ksc.type import Type


def _inplace_add_term_dicts(dict1, dict2):
    for var, coeff in dict2.items():
        dict1.setdefault(var, 0)
        dict1[var] += coeff


def _collect_terms_into_dict(expr: Expression):
    if expr.op == "variable":
        return {expr.name: 1}
    elif expr.op == "constant":
        return {"": int(expr.value)}
    elif expr.op == "add":
        lhs = _collect_terms_into_dict(expr.children[0])
        _inplace_add_term_dicts(lhs, _collect_terms_into_dict(expr.children[1]))
        return lhs
    elif expr.op == "sub":
        lhs = _collect_terms_into_dict(expr.children[0])
        _inplace_add_term_dicts(
            lhs,
            {
                var: -coeff
                for var, coeff in _collect_terms_into_dict(expr.children[1]).items()
            },
        )
        return lhs
    else:
        raise ValueError(
            "Cannot collect terms for expression containing operation {}".format(
                expr.op
            )
        )


def _sum_of_expressions(exprs):
    return reduce(operator.add, exprs)


def _make_variable(name):
    return Expression.Variable(name, type=Type.Integer)


def _sum_of_variables(variable_names):
    return _sum_of_expressions(_make_variable(v) for v in variable_names)


_symtab_and_defs = SymtabAndDefs()


def optimize_by_collecting_terms(expr: Expression) -> ExprWithEnv:
    """ Optimize an expression of the form term0 + term1 + term2 + ... + termN,
        where each term is either a constant or a variable.
        The procedure is to collect up terms in the same variable, then
        collect up variables which have the same resulting coefficient.

        This is guaranteed to return the optimal rewrite for many classes
        of input expressions, for example if each variable appears exactly
        once with coefficient 1. More generally, if there is no coefficient
        greater than 1 that appears exactly once, then this function will
        return the best possible equivalent expression.

        In the general case the optimal rewrite will depend on the rule set.
        For example this function may return
          4 + x1 + x2 + 2 * (x3 + x4 + x5 + x6) + 3 * x7
        But an equivalent expression with lower cost is
          4 + x1 + x2 + x7 + 2 * (x3 + x4 + x5 + x6 + x7)
        However this may not be reachable for some rule sets. """

    subexpressions = []
    variables_by_coefficient: Dict = collections.defaultdict(
        lambda: ([], [])
    )  # values are pairs (positive_terms, negative_terms)
    for var, coeff in _collect_terms_into_dict(expr).items():
        if var == "":  # constant term
            if coeff != 0:
                subexpressions.append(Expression.Constant(coeff))
        elif coeff > 0:
            variables_by_coefficient[coeff][0].append(var)
        elif coeff < 0:
            variables_by_coefficient[-coeff][1].append(var)

    pos_vars_coeff_1, neg_vars_coeff_1 = variables_by_coefficient[1]
    subexpressions += [_make_variable(v) for v in pos_vars_coeff_1]

    for coeff, (pos_vars, neg_vars) in variables_by_coefficient.items():
        if coeff > 1:
            if len(pos_vars) > 0:
                if len(neg_vars) > 0:
                    subexpressions.append(
                        coeff
                        * (_sum_of_variables(pos_vars) - _sum_of_variables(neg_vars))
                    )
                else:
                    subexpressions.append(coeff * _sum_of_variables(pos_vars))
            else:
                subexpressions.append((-coeff) * _sum_of_variables(neg_vars))

    sum_exprs = (
        _sum_of_expressions(subexpressions)
        if len(subexpressions) > 0
        else Expression.Constant(0)
    )
    return _symtab_and_defs.make_toplevel(
        sum_exprs - _sum_of_variables(neg_vars_coeff_1)
        if neg_vars_coeff_1
        else sum_exprs
    )


def try_optimize_by_collecting_terms(expr: Expression) -> Optional[ExprWithEnv]:
    """ Return the optimized form if possible, or None if the input expression
        is not a summation of the appropriate form """
    try:
        return optimize_by_collecting_terms(expr)
    except ValueError:
        return None


def _randomly_bracketed_sum(terms, rng):
    """ Generate a sum of the given terms, where the terms appear in order
        from left to right, but with random bracketing of terms.
        This function generates every possible bracketing with equal probability.
    """
    T = len(terms)
    # Each possible bracketed summation can be written in prefix form, e.g.
    #     + + t1 + t2 t3 + t4 t5
    # This sequence of operators and terms maps to a sequence of boolean values
    # of length (2T-1) which describes the positions of the operators, i.e.
    #    [True True False True False False True False False]
    # Conversely it can be shown that, given a sequence of boolean values of
    # length (2T-1), of which exactly (T-1) values are True, there is exactly
    # one rotation of this sequence which corresponds to a valid summation.
    # So we can generate a random bracketing by first choosing a sequence
    # of booleans at random ...
    is_operator = [False] * (2 * T - 1)
    for x in rng.sample(range(2 * T - 1), T - 1):
        is_operator[x] = True
    # ... and then finding the rotation which corresponds to a valid expression:
    partial_sums = itertools.accumulate((1 if e else -1) for e in is_operator)
    _, min_index = min(
        (partial_sum, ind) for ind, partial_sum in enumerate(partial_sums)
    )
    is_operator = is_operator[min_index + 1 :] + is_operator[: min_index + 1]
    # This sequence is then converted to an Expression:
    term_index = 0
    incomplete_nodes = [None]
    for op in is_operator:
        if op:
            incomplete_nodes.append(None)
        else:
            val = terms[term_index]
            term_index += 1
            while incomplete_nodes[-1] is not None:
                val = incomplete_nodes.pop() + val
            incomplete_nodes[-1] = val
    assert term_index == len(terms)
    return utils.single_elem(incomplete_nodes)


def generate_sum(
    num_terms: int, rng: random.Random, max_constant_term=2, fold=False
) -> ExprWithEnv:
    """ Generate an example expression which is a sum of the specified number of terms.
        There will be 2 or 3 constant terms in random positions. The remaining terms
        are distinct variables. """

    assert num_terms >= 3
    distance_between_constant_terms = rng.randint((num_terms + 1) // 2, num_terms - 1)
    first_constant_term = rng.randint(
        0, num_terms - distance_between_constant_terms - 1
    )
    if rng.random() > 0.5:
        constant_term_indices = [
            first_constant_term,
            first_constant_term + distance_between_constant_terms,
        ]
    else:
        constant_term_indices = [
            first_constant_term,
            first_constant_term + rng.randint(1, distance_between_constant_terms - 1),
            first_constant_term + distance_between_constant_terms,
        ]
    terms = []
    variable_index = 0
    for i in range(num_terms):
        if i in constant_term_indices:
            terms.append(Expression.Constant(rng.randint(1, max_constant_term)))
        else:
            terms.append(_make_variable("x{}".format(variable_index)))
            variable_index += 1
    return _symtab_and_defs.make_toplevel(
        _sum_of_expressions(terms) if fold else _randomly_bracketed_sum(terms, rng)
    )


def generate_sums(
    n: int, min_terms: int, max_terms: int, *, seed=12345, fold=False, choose_from=None
) -> Optional[List[ExprWithEnv]]:
    """ Generate the specified number of example expressions (with no duplicates).
        The constants used in the expressions will normally have a maximum value of 2,
        but this is increased if the number of examples requested is very large.
        If choose_from is not None then the expressions will be randomly sampled from a
        larger set of expressions, where the larger set consists of the expressions that
        would be generated for n=choose_from.
    """

    assert min_terms <= max_terms
    if choose_from is None:
        choose_from = n
    else:
        assert choose_from >= n
    rng = random.Random(seed)
    approx_different_sums = sum(
        i * i * i for i in range(min_terms, max_terms + 1)
    )  # Crude approximation of the total number of examples that it is possible to generate with constants at most 2.
    sums = set()
    sums_list = []
    for num_attempts in itertools.count():
        num_terms = num_attempts % (max_terms - min_terms + 1) + min_terms
        max_constant_term = num_attempts // approx_different_sums + 2
        new_sum = generate_sum(num_terms, rng, max_constant_term, fold=fold)
        if new_sum not in sums:
            sums.add(new_sum)
            sums_list.append(new_sum)
            if len(sums_list) >= choose_from:
                return sums_list if choose_from == n else rng.sample(sums_list, n)
    return None


def _num_consts(e):
    if e.op == "constant":
        return 1
    if not hasattr(e, "_num_consts"):
        e._num_consts = sum(_num_consts(c) for c in e.children)
    return e._num_consts


Subst = Mapping[str, Expression]


def _apply_subst(e: Expression, subst) -> Expression:
    if e.op == "constant":
        return e
    if e.op == "variable":
        return subst[e.name]
    assert e.op == "add"
    return _apply_subst(e.left, subst) + _apply_subst(e.right, subst)


class Step(NamedTuple):
    prev_skel: Expression
    rewritten: Expression  # Rewrite of previous (may not be a skeleton: placeholder vars could be out of order)
    rewritten_as_expr: Expression  # The same rewrite, but turned back into a form equivalent to the original expression (no placeholders)


class SummationsExpert:
    @staticmethod
    def _skel_and_substs(e: Expression) -> Tuple[Expression, Subst]:
        """ Returns a tuple of:
                * the skeleton of an expression: a version of e with maximal subtrees containing no constants replaced by a placeholder variable,
                  where the placeholders are numbered e0, e1 etc. in increasing order left-right.
                * the substitution from variable names to subtrees-containing-no-constants that gives back the original expression.
        """
        subs: Dict[str, Expression] = {}

        def helper(subexp):
            if subexp.op == "constant":
                return subexp
            if _num_consts(subexp) == 0:
                temp = _make_variable(f"e{len(subs)}")
                subs[temp.name] = subexp
                return temp
            assert subexp.op == "add"
            return helper(subexp.left) + helper(subexp.right)

        res = helper(e)
        assert _apply_subst(res, subs) == e
        return res, subs

    def __init__(self, num_time_heads=None):
        # Cache the next step along the best path for *skeletons* only.
        # Here we allow the cache to grow without bound, but we could use any eviction policy.
        self._cache = {}
        self._num_time_heads = num_time_heads

        self._rules = rewrites.RuleSet(
            [rewrites.rule(n) for n in ["assoc_add_add", "commute_add", "cprop_add"]]
        )

    def get_sequence(self, exprenv: ExprWithEnv) -> List[ExprWithEnv]:
        assert exprenv.env is _symtab_and_defs
        return [
            _symtab_and_defs.make_toplevel(e)
            for e in self.get_sequence_expr(exprenv.expr)
        ]

    def get_sequence_expr(self, e: Expression) -> List[Expression]:
        if len(e.children) == 0:
            return []
        l, r = _num_consts(e.left), _num_consts(e.right)
        if l + r <= 1:
            return []
        if l == 0:
            return [
                e.clone_with_new_children([e.left, s])
                for s in self.get_sequence_expr(e.right)
            ]
        if r == 0:
            return [
                e.clone_with_new_children([s, e.right])
                for s in self.get_sequence_expr(e.left)
            ]
        seq = self._search_to_smaller(e)
        return seq + self.get_sequence_expr(seq[-1])

    def _search_to_smaller(self, e: Expression) -> List[Expression]:
        # Return sequence of Expressions, each being one rewrite from the previous (the first one rewrite from e),
        # to an expression whose skeleton is smaller than e
        def possible_next_steps(skel) -> Sequence[Expression]:
            if skel in self._cache:
                # We know the best thing to do with this skeleton, there is only one possibility
                return [self._cache[skel]]
            # Don't yet know which rewrite to do. Each rewrite is a possibility.
            return [
                rw.apply_expr(skel) for rw in self._rules.get_all_rewrites_expr(skel)
            ]

        assert _num_consts(e) > 1

        skel1, substs1 = self._skel_and_substs(e)
        cands: Dict[Expression, Tuple[List[Step], Subst]] = {skel1: ([], substs1)}
        # Breadth-first search to the nearest Expression with a smaller skeleton.
        # 4 steps believed sufficient for any Expr (with <=3 constants).
        for _ in range(4):
            ncands = {}
            for cand_skel, (seq, substs) in cands.items():
                for next_step in possible_next_steps(cand_skel):
                    # next_step is the result of a rewrite, using vars from cand_skel
                    # (not necessarily a skeleton: placeholder vars could be out of order)
                    # Get back the actual non-skeleton expression, and convert to a valid skeleton (with placeholders numbered L-R)
                    next_as_expr = _apply_subst(next_step, substs)
                    nskel, nsubsts = self._skel_and_substs(next_as_expr)
                    ncands[nskel] = (
                        seq + [Step(cand_skel, next_step, next_as_expr)],
                        nsubsts,
                    )
            best = min(ncands, key=lambda skel: skel.num_nodes)
            if best.num_nodes < skel1.num_nodes:
                seq, _ = ncands[best]
                res = []
                # Found sequence. Record each step. (Possibly worth it only for sequences of length>1 or excluding cprop_add's?)
                for skel, nskel, next_as_expr in seq:
                    self._cache[skel] = nskel
                    res.append(next_as_expr)
                return res
            cands = ncands
        raise ValueError(
            "\n".join(
                [
                    f"Could not reduce skeleton of {e} from {skel1}, candidates: {cands.keys()}"
                ]
            )
        )

    def evaluate_all_time_left(self, exprenvs: Iterable[ExprWithEnv]) -> np.ndarray:
        def eval(e):
            seq = self.get_sequence(e)
            cost_seq = [0] + [e.cost() - s.cost() for s in seq]
            if self._num_time_heads is not None:
                cost_seq = cost_seq[: self._num_time_heads]
                cost_seq += cost_seq[-1:] * (self._num_time_heads - len(cost_seq))
            return cost_seq

        data = [eval(exprenv) for exprenv in exprenvs]
        if self._num_time_heads is None:
            res = np.array(data, dtype=object)  # May be jagged
        else:
            res = np.array(data, dtype=float)
            assert len(res.shape) == 2
        return res
