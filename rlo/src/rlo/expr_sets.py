from abc import ABC, abstractmethod
from typing import Any, Iterable, List, Mapping, NamedTuple, Optional, Sequence, Union
from collections import defaultdict
import os

from rlo import best_results
from rlo.best_results import TypeBestCost, best_results_env
from rlo.binding_examples import get_bindgen_train_sequences, get_bindgen_test_sequences
from rlo import experts
from rlo.expression import Expression
from rlo.expression_util import SymtabAndDefs, ExprWithEnv, NamedExprWithEnv
from rlo import rewrites
from rlo import sparser
from rlo.summations import generate_sums, try_optimize_by_collecting_terms
from rlo import tuple_examples
from rlo import utils

TypeFunc = NamedTuple("TypeFunc", [("types", Any), ("arg", Any)])

# it would be better if we could have Literal[float("inf")] but that is
# illegal (Invalid type alias: expression is not a valid type)
TimeBudget = Union[int, float]


class ExpressionSet(ABC):
    """ A factory which provides the list of expressions named by
        train_exprs or test_exprs, together with a function which
        can return the best cost for any expression in the list.
    """

    @abstractmethod
    def named_exprenvs(self) -> Sequence[NamedExprWithEnv]:
        """Sequence of named expressions making up this set."""

    def __len__(self) -> int:
        return len(self.named_exprenvs())

    @property
    def types(self) -> Optional[Mapping[str, TypeFunc]]:
        """Mapping of expression names to (types, arg) named tuple."""

    @staticmethod
    def best_cost_for_expression(
        exprenv: Union[str, ExprWithEnv], rules: str, time_budget: TimeBudget
    ) -> Optional[TypeBestCost]:
        """ Return the best cost for the given expression, or
            None if the best cost is not available
        """
        del time_budget
        return best_results.best_cost_for_exp(exprenv, rules)

    def get_expert(self, rules_name):
        raise NotImplementedError

    def union(self, other):
        """ Returns a union of self and the given ExpressionSet. """
        return UnionExpressionSet([self, other])


class DelayloadExpressionSet(ExpressionSet):
    def __init__(self):
        self._expressions = None

    def _load(self):
        raise NotImplementedError

    def named_exprenvs(self):
        if not self._expressions:
            self._load()
        return self._expressions


class UnionExpressionSet(DelayloadExpressionSet):
    def __init__(self, sources: Sequence[ExpressionSet]):
        super().__init__()
        self._sources = sources

    def _load(self):
        self._expressions = sorted(
            set().union(*[frozenset(src.named_exprenvs()) for src in self._sources])
        )

    def best_cost_for_expression(self, expr, rules, time_budget):
        for src in self._sources:
            res = src.best_cost_for_expression(expr, rules, time_budget)
            if res is not None:
                return res
        return None

    def get_expert(self, rules_name):
        # Assume that the experts are the same type
        exprts = [src.get_expert(rules_name) for src in self._sources]
        assert all(type(exprt) == type(exprts[0]) for exprt in exprts)
        return exprts[0]


class ExpressionSetFromFile(DelayloadExpressionSet):
    """ ExpressionSet which is read from a .ks or .kso file.

    Args:
        file_path: path to file to read from
        take_defs: optional list of def names to take
    """

    def __init__(self, file_path: str, take_defs: Optional[Sequence[str]] = None):
        super().__init__()
        self._file_path = file_path
        self._file_content = utils.read_file(file_path)
        self._best_costs = None
        self._take_defs = take_defs

    def _load(self):
        """Delay loading until required by expressions or get_type."""
        list_of_used_expressions = []

        for func_name, defs in sparser.parse_defs(self._file_content):
            if self._take_defs and func_name not in self._take_defs:
                continue
            defs = rewrites.delete_unused_defs(defs)
            list_of_used_expressions.append(NamedExprWithEnv(func_name, defs))

        if self._take_defs and len(list_of_used_expressions) != len(self._take_defs):
            found_defs = dict(list_of_used_expressions)
            missing = [name for name in self._take_defs if name not in found_defs]
            raise ValueError(
                f"defs {missing} specified in take_defs, but couldn't find in {self._file_path}"
            )

        self._expressions = tuple(list_of_used_expressions)

    def _default_best_costs(self):
        return {
            name: [TypeBestCost(expr.cost(), "original", [])]
            for name, expr in self.named_exprenvs()
        }

    def _load_best_costs(self):
        expr_dict = dict(self.named_exprenvs())
        if self._best_costs is None:
            #  self._best_costs is a dictionary that stores information on best costs achieved, optimization source
            #  and sequences, as a TypeBestCost namedtuple, for all expressions and rule sets. The format is al follows:
            #  {"rule_set_1": {"e1": [TypeBestCost(c11, rlo, seq11), TypeBestCost(c12, rlo, seq12)], "e2": [TypeBestCost(c21, rlo, seq21)]...}...}
            self._best_costs = defaultdict(self._default_best_costs)

            for entry in utils.parse_best_episodes(self._file_path):
                start = expr_dict[entry.func_name]
                new_costs = [
                    exprenv.cost()
                    for exprenv in rewrites.rewrite_seq_to_exprenvs(start, entry.seq)
                ]

                # TODO: consider using CumMinSequence instead of "existing" list
                existing = self._best_costs[entry.rules_name][entry.func_name]

                def new_best_costs():
                    for time_budget in range(max(len(existing), len(new_costs))):
                        has_existing = time_budget < len(existing)
                        has_new = time_budget < len(new_costs)
                        has_both = has_new and has_existing

                        if has_both:
                            new_cost = new_costs[time_budget]
                            existing_cost = existing[time_budget]

                            if new_cost < existing_cost.cost:
                                yield TypeBestCost(
                                    new_cost,
                                    entry.rlo_or_expert,
                                    entry.seq[:time_budget],
                                )
                            else:
                                yield existing_cost

                        elif has_new:
                            yield TypeBestCost(
                                new_costs[time_budget],
                                entry.rlo_or_expert,
                                entry.seq[:time_budget],
                            )

                        elif has_existing:
                            yield existing[time_budget]

                        else:
                            raise ValueError("Impossible!")

                self._best_costs[entry.rules_name][entry.func_name] = list(
                    new_best_costs()
                )

            # Take cumulative minimum over time_left
            for _rules_name, best_costs_by_expr in self._best_costs.items():
                for _func_name, best_costs in best_costs_by_expr.items():
                    best = best_costs[0]
                    new_best_costs = []
                    for type_best_cost in best_costs:
                        best = (
                            best if best.cost < type_best_cost.cost else type_best_cost
                        )
                        new_best_costs.append(best)
                    best_costs[:] = new_best_costs

    def best_cost_for_expression(self, expr_name, rules, time_budget):
        self._load_best_costs()
        if expr_name not in self._best_costs[rules]:
            return None
        existing = self._best_costs[rules][expr_name]
        return existing[time_budget] if time_budget < len(existing) else existing[-1]

    def get_expert(self, rules_name):
        expert_line = utils.single_elem(
            [
                line
                for line in self._file_content.split("\n")
                if line.startswith("; EXPERT")
            ]
        )
        expert_name = expert_line.split(" ")[-1]
        return experts.get_expert(expert_name, rewrites.get_rules(rules_name))


class ExprEnvWrapper(ExpressionSet):
    """ A simple wrapper for a list of ExprWithEnv's. If best costs
        are known then they will be declared in best_results.py
    """

    def __init__(self, exprenvs: Iterable[ExprWithEnv]):
        self._expr = tuple(NamedExprWithEnv(str(e.expr), e) for e in exprenvs)

    def named_exprenvs(self):
        return self._expr

    @classmethod
    def from_typed_expressions(cls, exprs: Iterable[Expression]):
        """ Wraps a collection of Expressions all of whose free variables already have types """
        st = SymtabAndDefs()
        return cls(st.make_toplevel(e) for e in exprs)


def parse_exprs(strs: Iterable[str]) -> List[ExprWithEnv]:
    return [best_results_env.make_toplevel(sparser.parse_expr(s)) for s in strs]


simplify_expressions = parse_exprs(
    [
        "(mul (div x x) x)",
        "(div (mul x x) x)",
        "(add (mul x y) (mul x x))",
        "(add (mul (div 1.0 x) x) (mul 1.0 x))",
        "(mul x (add (div 1.0 x) 1.0))",
        "(let (a (div 1.0 x)) (div a (add 1.0 a)))",
        "(div (mul x x) (mul x x))",
        "(div (mul (mul x x) x) x)",
        "(div (div 1.0 x) (add (div 1.0 x) 1.0))",
        "(div (div 1.0 (mul x x)) (add (div 1.0 (mul x x)) 1.0))",
    ]
)

fewer_simplify_expressions = parse_exprs(
    [
        "(mul (div x x) x)",
        "(div (mul x x) x)",
        "(add (mul (div 1.0 x) x) (mul 1.0 x))",
        "(mul x (add (div 1.0 x) 1.0))",
        "(div (div 1.0 x) (add (div 1.0 x) 1.0))",
    ]
)

all_simplify_expressions = best_results.known_exps("simplify_rules")

original_binding_expressions = parse_exprs(
    [
        "(div (div 1.0 x) (add 1.0 (div 1.0 x)))",
        "(div (div x (add y 1.0)) (add 1.0 (div x (add y 1.0))))",
        "(div (add 1.0 (div 1.0 x)) (add 2.0 (div 1.0 x)))",
        "(div (add 1.0 (add 1.0 (div 1.0 x))) (add 1.0 (div 1.0 x)))",
        "(div (add 1.0 (div 1.0 x)) (add 2.0 (div x (div 1.0 x))))",
        "(div (div x y) (add (add 1.0 (div x y)) (div x y)))",
    ]
)

binding_simplify_8 = parse_exprs(
    [
        "(div (div 1.0 x) (add 1.0 (div 1.0 x)))",
        "(add (div (div 1.0 x) (add (div 1.0 x) 1.0)) (div 1.0 x))",
        "(add (div (div 1.0 x) (add (div 1.0 x) 2.0)) (div 1.0 x))",
        "(mul (div x y) (div x y))",
        "(div (mul (div x y) x) y)",
        "(add (div (mul x y) (add 1.0 (mul x y))) (mul x y))",
        "(add (div 1.0 (add 1.0 (mul x y))) (mul x y))",
        "(div (mul x y) (add 1.0 (mul x y)))",
    ]
)

binding_simplify_expressions = parse_exprs(
    [
        "(div (div 1.0 x) (add 1.0 (div 1.0 x)))",
        "(add (div (div 1.0 x) (add (div 1.0 x) 1.0)) (div 1.0 x))",
        "(add (div (div 1.0 x) (add (div 1.0 x) 2.0)) (div 1.0 x))",
        "(mul (div x y) (div x y))",
        "(div (mul (div x y) x) y)",
        "(add (div (mul x y) (add 1.0 (mul x y))) (mul x y))",
        "(add (div 1.0 (add 1.0 (mul x y))) (mul x y))",
        "(div (mul x y) (add 1.0 (mul x y)))",
        "(mul (div x x) x)",
        "(div (mul x x) x)",
        "(add (mul x y) (mul x x))",
        "(add (mul (div 1.0 x) x) (mul 1.0 x))",
        "(mul x (add (div 1.0 x) 1.0))",
        "(let a (div 1.0 x) (div a (add 1.0 a)))",
        "(div (mul x x) (mul x x))",
        "(div (mul (mul x x) x) x)",
        "(div (div 1.0 x) (add (div 1.0 x) 1.0))",
        "(div (div 1.0 (mul x x)) (add (div 1.0 (mul x x)) 1.0))",
    ]
)


def get_expression_set_for_bindgen(set_name):
    _, spec = set_name.split("_")
    if spec == "test":
        return ExprEnvWrapper.from_typed_expressions(
            seq[0] for seq in get_bindgen_test_sequences()
        )
    num_train_exprs = int(spec)
    return ExprEnvWrapper.from_typed_expressions(
        seq[0] for seq in get_bindgen_train_sequences(num_train_exprs)
    )


def summations(*args, **kwargs):
    class SummationsExpressionSet(ExprEnvWrapper):
        def __init__(self):
            super().__init__(generate_sums(*args, **kwargs))

        def best_cost_for_expression(self, exp, rules, time_budget):
            del time_budget
            if rules == "simplify_rules":
                optimized_expr = try_optimize_by_collecting_terms(
                    sparser.parse_expr(exp)
                )
                if optimized_expr is not None:
                    return TypeBestCost(optimized_expr.cost(), "oracle", None)
            return best_results.best_cost_for_exp(exp, rules)

    return SummationsExpressionSet()


def tuple_train_exprs():
    return ExprEnvWrapper.from_typed_expressions(
        seq[0] for seq in tuple_examples.train_sequences
    )


def tuple_test_exprs():
    return ExprEnvWrapper.from_typed_expressions(
        seq[0] for seq in tuple_examples.test_sequences
    )


def get_expression_set_from_ks(filename, take_defs=None):
    """ Creates an expression set by loading the file with the specified
        filename, if that file can be found.
    """
    paths_to_try = [
        filename,
        os.path.join(os.path.dirname(os.path.abspath(__file__)), filename),
    ]
    for path in paths_to_try:
        if os.path.isfile(path):
            return ExpressionSetFromFile(path, take_defs)
    # If the file cannot be found then fall back to the default
    # implementation of best_cost_for_expression, which returns the
    # oracle cost if available. Only raise an error if we actually
    # require the list of expressions.
    class OracleCostsForMissingFile(ExpressionSet):
        def named_exprenvs(self):
            raise ValueError("Unable to find file at " + " or ".join(paths_to_try))

    return OracleCostsForMissingFile()


def get_expression_set(set_name: str, **kwargs) -> ExpressionSet:
    """ Returns the expression set with the specified name.
        set_name is the value of train_exprs or test_exprs in the scenario
        configuration file, and this should be one of the following:
        - the path of a .ks or .kso file containing the expressions to use;
        - the name of a list of expressions defined above in this file;
        - a function call such as summations(3,3,10) which generates a list
          of expressions using the given arguments.

    kwargs may contain:
        take_defs: optional list of def names to take
    """
    if set_name.startswith("bindgen"):
        return get_expression_set_for_bindgen(set_name)
    if set_name.endswith(".ks") or set_name.endswith(".kso"):
        return get_expression_set_from_ks(set_name, **kwargs)
    expr_list = eval(set_name)
    if isinstance(expr_list, list):
        return ExprEnvWrapper(expr_list)
    elif isinstance(expr_list, ExpressionSet):
        return expr_list
    raise ValueError("Invalid expression set name " + set_name)
