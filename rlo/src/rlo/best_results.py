# mypy: ignore-errors
from typing import NamedTuple, Optional, Any, Union

import numpy

from rlo import binding_examples
from rlo.expression import Expression
from rlo.expression_util import SymtabAndDefs, ExprWithEnv
from rlo import sparser
from rlo import tuple_examples
from ksc.type import Type

#  TypeBestCost - tuple that stores cost of an expression, method with which the cost was
#  obtanied and (optionally) the rewrite sequence with which the cost was obtained.
class TypeBestCost(NamedTuple):
    cost: float
    method: str
    sequence: Optional[Any]


# This file defines _best_results, which:
# For each ruleset (rules_name string),
# stores a list of tuples (seq, test_thoroughness),
# where

# seq:
#  is a sequence of Expressions, each reachable from the previous by
#  one rewrite step; where we believe that, under that ruleset
#  (a) there is no shorter route to the end Expression
#  (b) no other Expression can be reached from the start,
#      with a lower cost than the "best"
#
# test_thoroughness:
#   controls testing, and is one of the following values:
#   "optimal":
#       test every candidate reachable in any number of steps, even in "quick" test
#   "steps":
#       test every candidate reachable in the same length as the sequence,
#       even in "quick" test
#   float F:
#       test up to the length of the sequence, but discard all intermediates whose
#       cost is more than (starting cost + F).
#       E.g. F=0.2 is the cost (at the time of writing) of two inserted "let"s (see
#       cost.py for "let_cost = 0.1")
#       This means better results (requiring hillclimbing) could be missed for
#       some examples (e.g. binding rules),  but we know the amount of hillclimbing
#       required (e.g. the # lets introduced), so this still lets us have a guarantee.
#       In addition, in "quick" test only, these examples may bail out
#       if the search width is still too big.


# This contains the type of every variable free in an S-Expression below
best_results_env = SymtabAndDefs(
    symtab={
        "i": Type.Integer,
        "j": Type.Integer,
        "k": Type.Integer,
        "x": Type.Float,
        "y": Type.Float,
        "z": Type.Float,
        "a": Type.Float,
        "b": Type.Float,
        "p": Type.Bool,
        "u": Type.Tensor(1, Type.Float),
        "v": Type.Tensor(1, Type.Float),
        "w": Type.Tensor(1, Type.Float),
    }
)


def _maybe_parse(expr_or_s_exp: Union[ExprWithEnv, Expression, str]) -> ExprWithEnv:
    if isinstance(expr_or_s_exp, ExprWithEnv):
        return expr_or_s_exp
    if isinstance(expr_or_s_exp, Expression):
        # Expression object - Variables already have types
        return SymtabAndDefs().make_toplevel(expr_or_s_exp)
    # String expression: free-vars all named + typed consistently as per var_map
    expr = sparser.parse_expr(expr_or_s_exp)
    return best_results_env.make_toplevel(expr)


_simplify_sequences = [
    (["(mul 1.0 x)", "x",], "optimal"),
    (["(div x x)", "1.0",], "optimal"),
    (["(mul (div x x) x)", "(mul 1.0 x)", "x",], "optimal"),
    (["(div (mul x x) x)", "(mul (div x x) x)", "(mul 1.0 x)", "x",], "optimal"),
    (["(add (mul x y) (mul x x))", "(mul x (add y x))",], "optimal"),
    (
        [
            "(add (mul (div 1.0 x) x) (mul 1.0 x))",
            "(add (div (mul 1.0 x) x) (mul 1.0 x))",
            "(add (div x x) (mul 1.0 x))",
            "(add 1.0 (mul 1.0 x))",
            "(add 1.0 x)",
        ],
        "optimal",
    ),
    (
        [
            "(mul x (add (div 1.0 x) 1.0))",
            "(add (mul x (div 1.0 x)) (mul x 1.0))",
            "(add (mul (div 1.0 x) x) (mul x 1.0))",
            "(add (div (mul 1.0 x) x) (mul x 1.0))",
            "(add (div x x) (mul x 1.0))",
            "(add 1.0 (mul x 1.0))",
            "(add 1.0 (mul 1.0 x))",
            "(add 1.0 x)",
        ],
        "optimal",
    ),
    (
        [
            "(let (a (div 1.0 x)) (div a (add 1.0 a)))",
            "(let (a (div 1.0 x)) (div a (add 1.0 (div 1.0 x))))",
            "(let (a (div 1.0 x)) (div (div 1.0 x) (add 1.0 (div 1.0 x))))",
            "(div (div 1.0 x) (add 1.0 (div 1.0 x)))",
            "(div 1.0 (mul x (add 1.0 (div 1.0 x))))",
            "(div 1.0 (add (mul x 1.0) (mul x (div 1.0 x))))",
            "(div 1.0 (add (mul 1.0 x) (mul x (div 1.0 x))))",
            "(div 1.0 (add x (mul x (div 1.0 x))))",
            "(div 1.0 (add x (mul (div 1.0 x) x)))",
            "(div 1.0 (add x (div (mul 1.0 x) x)))",
            "(div 1.0 (add x (div x x)))",
            "(div 1.0 (add x 1.0))",
        ],
        "optimal",
    ),
    (["(div (mul x x) (mul x x))", "1.0",], "optimal"),
    (
        [
            "(div (mul (mul x x) x) x)",
            "(mul (div (mul x x) x) x)",
            "(mul (mul (div x x) x) x)",
            "(mul (mul 1.0 x) x)",
            "(mul x x)",
        ],
        "optimal",
    ),
    (
        [
            "(div (div 1.0 x) (add (div 1.0 x) 1.0))",
            "(div 1.0 (mul x (add (div 1.0 x) 1.0)))",
            "(div 1.0 (add (mul x (div 1.0 x)) (mul x 1.0)))",
            "(div 1.0 (add (mul (div 1.0 x) x) (mul x 1.0)))",
            "(div 1.0 (add (div (mul 1.0 x) x) (mul x 1.0)))",
            "(div 1.0 (add (div x x) (mul x 1.0)))",
            "(div 1.0 (add 1.0 (mul x 1.0)))",
            "(div 1.0 (add 1.0 (mul 1.0 x)))",
            "(div 1.0 (add 1.0 x))",
        ],
        "optimal",
    ),
    (
        [
            "(div (div 1.0 (mul x x)) (add (div 1.0 (mul x x)) 1.0))",
            "(div 1.0 (mul (mul x x) (add (div 1.0 (mul x x)) 1.0)))",
            "(div 1.0 (add (mul (mul x x) (div 1.0 (mul x x))) (mul (mul x x) 1.0)))",
            "(div 1.0 (add (mul (div 1.0 (mul x x)) (mul x x)) (mul (mul x x) 1.0)))",
            "(div 1.0 (add (div (mul 1.0 (mul x x)) (mul x x)) (mul (mul x x) 1.0)))",
            "(div 1.0 (add (div (mul x x) (mul x x)) (mul (mul x x) 1.0)))",
            "(div 1.0 (add 1.0 (mul (mul x x) 1.0)))",
            "(div 1.0 (add 1.0 (mul 1.0 (mul x x))))",
            "(div 1.0 (add 1.0 (mul x x)))",
        ],
        "optimal",
    ),
    (
        [
            "(div (div 1.0 x) (add 1.0 (div 1.0 x)))",
            "(div 1.0 (mul x (add 1.0 (div 1.0 x))))",
            "(div 1.0 (add (mul x 1.0) (mul x (div 1.0 x))))",
            "(div 1.0 (add (mul x 1.0) (mul (div 1.0 x) x)))",
            "(div 1.0 (add (mul x 1.0) (div (mul 1.0 x) x)))",
            "(div 1.0 (add (mul x 1.0) (div x x)))",
            "(div 1.0 (add (mul x 1.0) 1.0))",
            "(div 1.0 (add (mul 1.0 x) 1.0))",
            "(div 1.0 (add x 1.0))",
        ],
        "optimal",
    ),
    (
        [
            "(add (div (div 1.0 x) (add (div 1.0 x) 1.0)) (div 1.0 x))",
            "(add (div 1.0 (mul x (add (div 1.0 x) 1.0))) (div 1.0 x))",
            "(add (div 1.0 (add (mul x (div 1.0 x)) (mul x 1.0))) (div 1.0 x))",
            "(add (div 1.0 (add (mul (div 1.0 x) x) (mul x 1.0))) (div 1.0 x))",
            "(add (div 1.0 (add (div (mul 1.0 x) x) (mul x 1.0))) (div 1.0 x))",
            "(add (div 1.0 (add (div x x) (mul x 1.0))) (div 1.0 x))",
            "(add (div 1.0 (add 1.0 (mul x 1.0))) (div 1.0 x))",
            "(add (div 1.0 (add 1.0 (mul 1.0 x))) (div 1.0 x))",
            "(add (div 1.0 (add 1.0 x)) (div 1.0 x))",
        ],
        "optimal",
    ),
    (
        [
            "(div (div x (add y 1.0)) (add 1.0 (div x (add y 1.0))))",
            "(div x (mul (add y 1.0) (add 1.0 (div x (add y 1.0)))))",
            "(div x (add (mul (add y 1.0) 1.0) (mul (add y 1.0) (div x (add y 1.0)))))",
            "(div x (add (mul 1.0 (add y 1.0)) (mul (add y 1.0) (div x (add y 1.0)))))",
            "(div x (add (add y 1.0) (mul (add y 1.0) (div x (add y 1.0)))))",
            "(div x (add (add y 1.0) (mul (div x (add y 1.0)) (add y 1.0))))",
            "(div x (add (add y 1.0) (div (mul x (add y 1.0)) (add y 1.0))))",
            "(div x (add (add y 1.0) (div (mul (add y 1.0) x) (add y 1.0))))",
            "(div x (add (add y 1.0) (mul (div (add y 1.0) (add y 1.0)) x)))",
            "(div x (add (add y 1.0) (mul 1.0 x)))",
            "(div x (add (add y 1.0) x))",
        ],
        "steps",
    ),
    (  # Big example, this takes about 10 minutes for optimal / 8 minutes for best in steps
        [
            """(add
                (div (mul 15.0 a) (mul 2.0 y))
                (div (mul (mul 30.0 b) x) (mul (mul 4.0 y) y)))""",
            """(add
                (mul (div 15.0 (mul 2.0 y)) a)
                (div (mul (mul 30.0 b) x) (mul (mul 4.0 y) y)))""",
            """(add
                (mul (div (div 15.0 2.0) y) a)
                (div (mul (mul 30.0 b) x) (mul (mul 4.0 y) y)))""",
            "(add (mul (div 7.5 y) a) (div (mul (mul 30.0 b) x) (mul (mul 4.0 y) y)))",
            "(add (mul (div 7.5 y) a) (mul (div (mul 30.0 b) (mul (mul 4.0 y) y)) x))",
            "(add (mul (div 7.5 y) a) (mul (div (div (mul 30.0 b) (mul 4.0 y)) y) x))",
            "(add (mul (div 7.5 y) a) (mul (div (mul (div 30.0 (mul 4.0 y)) b) y) x))",
            "(add (mul (div 7.5 y) a) (mul (div (mul (div (div 30.0 4.0) y) b) y) x))",
            "(add (mul (div 7.5 y) a) (mul (div (mul (div 7.5 y) b) y) x))",
            "(add (mul (div 7.5 y) a) (mul (div (mul b (div 7.5 y)) y) x))",
            "(add (mul (div 7.5 y) a) (mul (mul (div b y) (div 7.5 y)) x))",
            "(add (mul (div 7.5 y) a) (mul (mul (div b y) x) (div 7.5 y)))",
            "(add (mul (div 7.5 y) a) (mul (div 7.5 y) (mul (div b y) x)))",
            "(mul (div 7.5 y) (add a (mul (div b y) x)))",
        ],
        0.0,  # No cost increasing rewrites, allow quick test to bail.
    ),
    (  # This slightly bigger version of previous has a shorter sequence to optimum, using the dual (subtract) rule a*x - a*y => a*(x-y) rule
        [
            """(sub
                (div (mul 15.0 a) (mul 2.0 y))
                (mul b (div (mul 15.0 x) (mul (mul 2.0 y) (mul 2.0 y)))))""",
            """(sub
                (mul (div 15.0 (mul 2.0 y)) a)
                (mul b (div (mul 15.0 x) (mul (mul 2.0 y) (mul 2.0 y)))))""",
            """(sub
                (mul (div 15.0 (mul 2.0 y)) a)
                (mul (div (mul 15.0 x) (mul (mul 2.0 y) (mul 2.0 y))) b))""",
            """(sub
                (mul (div 15.0 (mul 2.0 y)) a)
                (mul (div (div (mul 15.0 x) (mul 2.0 y)) (mul 2.0 y)) b))""",
            """(sub
                (mul (div 15.0 (mul 2.0 y)) a)
                (mul (div (mul (div 15.0 (mul 2.0 y)) x) (mul 2.0 y)) b))""",
            """(sub
                (mul (div 15.0 (mul 2.0 y)) a)
                (mul (div (mul x (div 15.0 (mul 2.0 y))) (mul 2.0 y)) b))""",
            """(sub
                (mul (div 15.0 (mul 2.0 y)) a)
                (mul (mul (div x (mul 2.0 y)) (div 15.0 (mul 2.0 y))) b))""",
            """(sub
                (mul (div 15.0 (mul 2.0 y)) a)
                (mul (mul (div x (mul 2.0 y)) b) (div 15.0 (mul 2.0 y))))""",
            """(sub
                (mul (div 15.0 (mul 2.0 y)) a)
                (mul (div 15.0 (mul 2.0 y)) (mul (div x (mul 2.0 y)) b)))""",
            "(mul (div 15.0 (mul 2.0 y)) (sub a (mul (div x (mul 2.0 y)) b)))",
            "(mul (div (div 15.0 2.0) y) (sub a (mul (div x (mul 2.0 y)) b)))",
            "(mul (div 7.5 y) (sub a (mul (div x (mul 2.0 y)) b)))",
        ],
        0.0,
    ),
    (
        # This one uses ...+(x*-1) => ...-x followed by a/x-b/x => (a-b)/x.
        # There is also a longer route delaying the first rule until after using a/x + b/x => (a+b)/x.
        [
            "(add (div (mul 15.0 a) z) (mul (mul -1.0 b) (div (mul 15.0 x) (mul y z))))",
            "(add (div (mul 15.0 a) z) (mul (mul -1.0 (div (mul 15.0 x) (mul y z))) b))",
            "(add (div (mul 15.0 a) z) (mul (mul (div (mul 15.0 x) (mul y z)) -1.0) b))",
            "(add (div (mul 15.0 a) z) (mul (mul (div (mul 15.0 x) (mul y z)) b) -1.0))",
            "(sub (div (mul 15.0 a) z) (mul (div (mul 15.0 x) (mul y z)) b))",
            "(sub (div (mul 15.0 a) z) (div (mul (mul 15.0 x) b) (mul y z)))",
            "(sub (div (mul 15.0 a) z) (div (div (mul (mul 15.0 x) b) y) z))",
            "(div (sub (mul 15.0 a) (div (mul (mul 15.0 x) b) y)) z)",
            "(div (sub (mul 15.0 a) (div (mul (mul x 15.0) b) y)) z)",
            "(div (sub (mul 15.0 a) (div (mul (mul x b) 15.0) y)) z)",
            "(div (sub (mul 15.0 a) (mul (div (mul x b) y) 15.0)) z)",
            "(div (sub (mul 15.0 a) (mul 15.0 (div (mul x b) y))) z)",
            "(div (mul 15.0 (sub a (div (mul x b) y))) z)",
        ],
        0.0,
    ),
]

_binding_sequences = [
    # Note we use "steps" rather than "optimal" because simplistic Expression.new_var
    # can make infinite alpha-convertible expressions
    (seq, "steps" if len(seq[0].nodes) < 12 else 0.2)
    for seqs in binding_examples.binding_sequences()
    for seq in seqs
] + [
    (
        [
            "(div (div 1.0 x) (add 1.0 (div 1.0 x)))",
            "(div (let (var0 (div 1.0 x)) var0) (add 1.0 (div 1.0 x)))",
            "(let (var0 (div 1.0 x)) (div var0 (add 1.0 (div 1.0 x))))",
            """(let (var0 (div 1.0 x))
                    (div var0 (add 1.0 (let (var1 (div 1.0 x)) var1))))""",
            """(let (var0 (div 1.0 x))
                    (div var0 (let (var1 (div 1.0 x)) (add 1.0 var1))))""",
            """(let (var0 (div 1.0 x))
                    (let (var1 (div 1.0 x)) (div var0 (add 1.0 var1))))""",
            "(let (var0 (div 1.0 x)) (div var0 (add 1.0 var0)))",
        ],
        "steps",
    ),  # Optimal only under binding_rules
    (
        [
            "(div (div x (add y 1.0)) (add 1.0 (div x (add y 1.0))))",
            "(div (let (var0 (div x (add y 1.0))) var0) (add 1.0 (div x (add y 1.0))))",
            """(div
                    (let (var0 (div x (add y 1.0))) var0)
                    (add 1.0 (let (var1 (div x (add y 1.0))) var1)))""",
            """(div
                    (let (var0 (div x (add y 1.0))) var0)
                    (let (var1 (div x (add y 1.0))) (add 1.0 var1)))""",
            """(let (var0 (div x (add y 1.0)))
                    (div var0 (let (var1 (div x (add y 1.0))) (add 1.0 var1))))""",
            """(let (var0 (div x (add y 1.0)))
                    (let (var1 (div x (add y 1.0))) (div var0 (add 1.0 var1))))""",
            "(let (var0 (div x (add y 1.0))) (div var0 (add 1.0 var0)))",
        ],
        "steps",
    ),
    (
        [
            "(div (add 1.0 (div 1.0 x)) (add 2.0 (div 1.0 x)))",
            "(div (add 1.0 (let (var0 (div 1.0 x)) var0)) (add 2.0 (div 1.0 x)))",
            "(div (let (var0 (div 1.0 x)) (add 1.0 var0)) (add 2.0 (div 1.0 x)))",
            """(div
                    (let (var0 (div 1.0 x)) (add 1.0 var0))
                    (add 2.0 (let (var1 (div 1.0 x)) var1)))""",
            """(div
                    (let (var0 (div 1.0 x)) (add 1.0 var0))
                    (let (var1 (div 1.0 x)) (add 2.0 var1)))""",
            """(let (var0 (div 1.0 x))
                    (div (add 1.0 var0) (let (var1 (div 1.0 x)) (add 2.0 var1))))""",
            """(let (var0 (div 1.0 x))
                    (let (var1 (div 1.0 x)) (div (add 1.0 var0) (add 2.0 var1))))""",
            "(let (var0 (div 1.0 x)) (div (add 1.0 var0) (add 2.0 var0)))",
        ],
        "steps",
    ),
    (
        [
            "(div (add 1.0 (add 1.0 (div 1.0 x))) (add 1.0 (div 1.0 x)))",
            """(div
                    (add 1.0 (let (var0 (add 1.0 (div 1.0 x))) var0))
                    (add 1.0 (div 1.0 x)))""",
            """(div
                    (let (var0 (add 1.0 (div 1.0 x))) (add 1.0 var0))
                    (add 1.0 (div 1.0 x)))""",
            """(div
                    (let (var0 (add 1.0 (div 1.0 x))) (add 1.0 var0))
                    (let (var1 (add 1.0 (div 1.0 x))) var1))""",
            """(let (var0 (add 1.0 (div 1.0 x)))
                    (div (add 1.0 var0) (let (var1 (add 1.0 (div 1.0 x))) var1)))""",
            """(let (var0 (add 1.0 (div 1.0 x)))
                    (let (var1 (add 1.0 (div 1.0 x))) (div (add 1.0 var0) var1)))""",
            "(let (var0 (add 1.0 (div 1.0 x))) (div (add 1.0 var0) var0))",
        ],
        "steps",
    ),
    (
        [
            "(div (add 1.0 (div 1.0 x)) (add 2.0 (div x (div 1.0 x))))",
            "(div (add 1.0 (let (var0 (div 1.0 x)) var0)) (add 2.0 (div x (div 1.0 x))))",
            "(div (let (var0 (div 1.0 x)) (add 1.0 var0)) (add 2.0 (div x (div 1.0 x))))",
            """(div
                    (let (var0 (div 1.0 x)) (add 1.0 var0))
                    (add 2.0 (div x (let (var1 (div 1.0 x)) var1))))""",
            """(div
                    (let (var0 (div 1.0 x)) (add 1.0 var0))
                    (add 2.0 (let (var1 (div 1.0 x)) (div x var1))))""",
            """(div
                    (let (var0 (div 1.0 x)) (add 1.0 var0))
                    (let (var1 (div 1.0 x)) (add 2.0 (div x var1))))""",
            """(let (var0 (div 1.0 x))
                    (div (add 1.0 var0) (let (var1 (div 1.0 x)) (add 2.0 (div x var1)))))""",
            """(let (var0 (div 1.0 x))
                    (let (var1 (div 1.0 x)) (div (add 1.0 var0) (add 2.0 (div x var1)))))""",
            "(let (var0 (div 1.0 x)) (div (add 1.0 var0) (add 2.0 (div x var0))))",
        ],
        0.2,
    ),  # steps takes too long
    (
        [
            "(div (div x y) (add (add 1.0 (div x y)) (div x y)))",
            "(div (div x y) (add (add 1.0 (let (var0 (div x y)) var0)) (div x y)))",
            "(div (div x y) (add (let (var0 (div x y)) (add 1.0 var0)) (div x y)))",
            """(div
                    (div x y)
                    (add (let (var0 (div x y)) (add 1.0 var0)) (let (var1 (div x y)) var1)))""",
            """(div
                    (div x y)
                    (let (var0 (div x y))
                    (add (add 1.0 var0) (let (var1 (div x y)) var1))))""",
            """(div
                    (div x y)
                    (let (var0 (div x y))
                    (let (var1 (div x y)) (add (add 1.0 var0) var1))))""",
            "(div (div x y) (let (var0 (div x y)) (add (add 1.0 var0) var0)))",
            """(div
                    (let (var1 (div x y)) var1)
                    (let (var0 (div x y)) (add (add 1.0 var0) var0)))""",
            """(let (var1 (div x y))
                    (div var1 (let (var0 (div x y)) (add (add 1.0 var0) var0))))""",
            """(let (var1 (div x y))
                    (let (var0 (div x y)) (div var1 (add (add 1.0 var0) var0))))""",
            "(let (var1 (div x y)) (div var1 (add (add 1.0 var1) var1)))",
        ],
        0.2,
    ),
    (
        [  # Lifting an invariant portion out of a loop
            "(build 100 (lam i (mul i (add j 3))))",
            "(build 100 (lam i (mul i (let z (add j 3) z))))",
            "(build 100 (lam i (let z (add j 3) (mul i z))))",
            "(let z (add j 3) (build 100 (lam i (mul i z))))",
        ],
        "steps",
    ),
]

_binding_simplify_sequences = [
    (seq, 0.2)  # F=0.2: Don't check optimal/steps, too expensive with bigger ruleset
    for seq in [seq for seq, _ in _simplify_sequences]
] + [
    (
        [
            "(add (div (div 1.0 x) (add (div 1.0 x) 2.0)) (div 1.0 x))",
            "(add (div (let (var0 (div 1.0 x)) var0) (add (div 1.0 x) 2.0)) (div 1.0 x))",
            """(add
                (div (let (var0 (div 1.0 x)) var0) (add (let (var1 (div 1.0 x)) var1) 2.0))
                (div 1.0 x))""",
            """(add
                (div (let (var0 (div 1.0 x)) var0) (let (var1 (div 1.0 x)) (add var1 2.0)))
                (div 1.0 x))""",
            """(add
                (let (var0 (div 1.0 x))
                (div var0 (let (var1 (div 1.0 x)) (add var1 2.0))))
                (div 1.0 x))""",
            """(add
                (let (var0 (div 1.0 x))
                (let (var1 (div 1.0 x)) (div var0 (add var1 2.0))))
                (div 1.0 x))""",
            "(add (let (var0 (div 1.0 x)) (div var0 (add var0 2.0))) (div 1.0 x))",
            """(add
                (let (var0 (div 1.0 x)) (div var0 (add var0 2.0)))
                (let (var1 (div 1.0 x)) var1))""",
            """(let (var0 (div 1.0 x))
                (add (div var0 (add var0 2.0)) (let (var1 (div 1.0 x)) var1)))""",
            """(let (var0 (div 1.0 x))
                (let (var1 (div 1.0 x)) (add (div var0 (add var0 2.0)) var1)))""",
            "(let (var0 (div 1.0 x)) (add (div var0 (add var0 2.0)) var0))",
        ],
        0.2,
    ),
    (
        [
            "(div (mul x (add y 1.0)) (add 1.0 (mul x (add y 1.0))))",
            "(div (let (var0 (mul x (add y 1.0))) var0) (add 1.0 (mul x (add y 1.0))))",
            """(let (var0 (mul x (add y 1.0)))
                (div var0 (add 1.0 (mul x (add y 1.0)))))""",
            """(let (var0 (mul x (add y 1.0)))
                (div var0 (add 1.0 (let (var1 (mul x (add y 1.0))) var1))))""",
            """(let (var0 (mul x (add y 1.0)))
                (div var0 (let (var1 (mul x (add y 1.0))) (add 1.0 var1))))""",
            """(let (var0 (mul x (add y 1.0)))
                (let (var1 (mul x (add y 1.0))) (div var0 (add 1.0 var1))))""",
            "(let (var0 (mul x (add y 1.0))) (div var0 (add 1.0 var0)))",
        ],
        0.2,
    ),
    (
        [
            "(add (div x (add y 1.0)) (div 1.0 (add y 1.0)))",
            "(div (add x 1.0) (add y 1.0))",
        ],
        0.2,
    ),
    (
        [
            "(mul (div x y) (div x y))",
            "(mul (let (var0 (div x y)) var0) (div x y))",
            "(let (var0 (div x y)) (mul var0 (div x y)))",
            "(let (var0 (div x y)) (mul var0 (let (var1 (div x y)) var1)))",
            "(let (var0 (div x y)) (let (var1 (div x y)) (mul var0 var1)))",
            "(let (var0 (div x y)) (mul var0 var0))",
        ],
        0.2,
    ),
    (
        [
            "(div (mul (div x y) x) y)",
            "(div (mul x (div x y)) y)",
            "(mul (div x y) (div x y))",
            "(mul (let (var0 (div x y)) var0) (div x y))",
            "(let (var0 (div x y)) (mul var0 (div x y)))",
            "(let (var0 (div x y)) (mul var0 (let (var1 (div x y)) var1)))",
            "(let (var0 (div x y)) (let (var1 (div x y)) (mul var0 var1)))",
            "(let (var0 (div x y)) (mul var0 var0))",
        ],
        0.2,
    ),
    (
        [
            "(add (div (mul x y) (add 1.0 (mul x y))) (mul x y))",
            "(add (div (mul x y) (add 1.0 (let (var0 (mul x y)) var0))) (mul x y))",
            "(add (div (mul x y) (let (var0 (mul x y)) (add 1.0 var0))) (mul x y))",
            "(add (let (var0 (mul x y)) (div (mul x y) (add 1.0 var0))) (mul x y))",
            """(add
                    (let (var0 (mul x y)) (div (let (var1 (mul x y)) var1) (add 1.0 var0)))
                    (mul x y))""",
            """(add
                    (let (var0 (mul x y)) (let (var1 (mul x y)) (div var1 (add 1.0 var0))))
                    (mul x y))""",
            "(add (let (var0 (mul x y)) (div var0 (add 1.0 var0))) (mul x y))",
            "(let (var0 (mul x y)) (add (div var0 (add 1.0 var0)) (mul x y)))",
            """(let (var0 (mul x y))
                (add (div var0 (add 1.0 var0)) (let (var1 (mul x y)) var1)))""",
            """(let (var0 (mul x y))
                (let (var1 (mul x y)) (add (div var0 (add 1.0 var0)) var1)))""",
            "(let (var0 (mul x y)) (add (div var0 (add 1.0 var0)) var0))",
        ],
        0.2,
    ),
    (
        [
            "(add (div 1.0 (add 1.0 (mul x y))) (mul x y))",
            "(add (div 1.0 (add 1.0 (let (var0 (mul x y)) var0))) (mul x y))",
            "(add (div 1.0 (let (var0 (mul x y)) (add 1.0 var0))) (mul x y))",
            "(add (let (var0 (mul x y)) (div 1.0 (add 1.0 var0))) (mul x y))",
            "(let (var0 (mul x y)) (add (div 1.0 (add 1.0 var0)) (mul x y)))",
            """(let (var0 (mul x y))
                (add (div 1.0 (add 1.0 var0)) (let (var1 (mul x y)) var1)))""",
            """(let (var0 (mul x y))
                (let (var1 (mul x y)) (add (div 1.0 (add 1.0 var0)) var1)))""",
            "(let (var0 (mul x y)) (add (div 1.0 (add 1.0 var0)) var0))",
        ],
        0.2,
    ),
    (
        [
            "(div (mul x y) (add 1.0 (mul x y)))",
            "(div (mul x y) (add 1.0 (let (var0 (mul x y)) var0)))",
            "(div (mul x y) (let (var0 (mul x y)) (add 1.0 var0)))",
            "(let (var0 (mul x y)) (div (mul x y) (add 1.0 var0)))",
            "(let (var0 (mul x y)) (div (let (var1 (mul x y)) var1) (add 1.0 var0)))",
            "(let (var0 (mul x y)) (let (var1 (mul x y)) (div var1 (add 1.0 var0))))",
            "(let (var0 (mul x y)) (div var0 (add 1.0 var0)))",
        ],
        0.2,
    ),
]

_log_exp_sequences = [
    (
        [
            "(exp (add (log x) 10.0))",
            "(mul (exp (log x)) (exp 10.0))",
            "(mul x (exp 10.0))",
            f"(mul x {numpy.exp(10.0)})",
        ],
        "optimal",
    ),
]

_if_sequences = [
    (
        [
            "(if (eq (if p (add i 1) (sub i 1)) (if (if p false true) (sub i 1) (add i 1))) y (add y 5.0))",
            "(if (eq (if p (add i 1) (sub i 1)) (if p (if false (sub i 1) (add i 1)) (if true (sub i 1) (add i 1)))) y (add y 5.0))",
            "(if (eq (if p (add i 1) (sub i 1)) (if p (if false (sub i 1) (add i 1)) (sub i 1))) y (add y 5.0))",
            "(if (eq (if p (add i 1) (sub i 1)) (if p (add i 1) (sub i 1))) y (add y 5.0))",
            "(if true y (add y 5.0))",
            "y",
        ],
        "steps",
    ),
    (
        [
            "(gt (if p 3 4) 2)",
            "(if p (gt 3 2) (gt 4 2))",
            "(if p true (gt 4 2))",
            "(if p true true)",
            "true",
        ],
        "optimal",
    ),
]

_vector_tuple_sequences = [
    (seq, "steps")
    for seq in tuple_examples.train_sequences + tuple_examples.test_sequences
] + [
    (
        [
            "(get$5$6 (tuple (mul x x) (mul x y) (mul x z) (mul y y) (mul y z) (mul z z)))",
            "(mul y z)",
        ],
        "optimal",
    ),
    (
        [
            "(let x (build (size v) (lam y (add (index y v) (index y w)))) (build (size x) (lam z (mul (index z x) (index z u)))))",
            # Inline one x (hillclimb), simplify out size
            "(let x (build (size v) (lam y (add (index y v) (index y w)))) (build (size (build (size v) (lam y (add (index y v) (index y w))))) (lam z (mul (index z x) (index z u)))))",
            "(let x (build (size v) (lam y (add (index y v) (index y w)))) (build (size v) (lam z (mul (index z x) (index z u)))))",
            # Inline other x (hillclimb), delete unused let
            "(let x (build (size v) (lam y (add (index y v) (index y w)))) (build (size v) (lam z (mul (index z (build (size v) (lam y (add (index y v) (index y w))))) (index z u)))))",
            "(build (size v) (lam z (mul (index z (build (size v) (lam y (add (index y v) (index y w))))) (index z u))))",
            "(build (size v) (lam z (mul (let y z (add (index y v) (index y w))) (index z u))))",
            # Now inline y
            "(build (size v) (lam z (mul (let y z (add (index z v) (index y w))) (index z u))))",
            "(build (size v) (lam z (mul (let y z (add (index z v) (index z w))) (index z u))))",
            "(build (size v) (lam z (mul (add (index z v) (index z w)) (index z u))))",
        ],
        "optimal",
    ),
]

_build_simplify_sequences = [
    (
        [
            "(let f (lam (x : Integer) (build x (lam y (add y 2)))) (index 5 (f 10)))",
            "(let f (lam (x : Integer) (build x (lam y (add y 2)))) (index 5 (let x 10 (build x (lam y (add y 2))))))",
            "(index 5 (let x 10 (build x (lam y (add y 2)))))",
            "(index 5 (let x 10 (build 10 (lam y (add y 2)))))",
            "(index 5 (build 10 (lam y (add y 2))))",
            "(let y 5 (add y 2))",
            "(let y 5 (add 5 2))",
            "(add 5 2)",
            "7",
        ],
        "optimal",
    ),
    (
        [
            "(let f (lam (x : Vec Integer) (index (index 5 x) x)) (f (build 10 (lam y (add y 1)))))",
            "(let f (lam (x : Vec Integer) (index (index 5 x) x)) (let x (build 10 (lam y (add y 1))) (index (index 5 x) x)))",
            "(let x (build 10 (lam y (add y 1))) (index (index 5 x) x))",
            "(let x (build 10 (lam y (add y 1))) (index (index 5 (build 10 (lam y (add y 1)))) x))",
            "(let x (build 10 (lam y (add y 1))) (index (let y 5 (add y 1)) x))",
            "(let x (build 10 (lam y (add y 1))) (index (let y 5 (add 5 1)) x))",
            "(let x (build 10 (lam y (add y 1))) (index (add 5 1) x))",
            "(let x (build 10 (lam y (add y 1))) (index (add 5 1) (build 10 (lam y (add y 1)))))",
            "(index (add 5 1) (build 10 (lam y (add y 1))))",
            "(let y (add 5 1) (add y 1))",
            "(let y (add 5 1) (add (add 5 1) 1))",
            "(add (add 5 1) 1)",
            "(add 6 1)",
            "7",
        ],
        "optimal",
    ),
]

_best_results = {
    rules_name: [([_maybe_parse(s) for s in seq], test_type) for seq, test_type in seqs]
    for rules_name, seqs in {
        "simplify_rules": _simplify_sequences,
        "binding_rules": _binding_sequences,
        "log_exp_rules": _log_exp_sequences,
        "if_rules": _if_sequences,
        "vector_tuple_rules": _vector_tuple_sequences,
        "build_simplify_rules": _build_simplify_sequences,
        "binding_simplify_rules": _binding_simplify_sequences,
        "ml_rules": _binding_simplify_sequences,
        "ml_rules_no_bind": [],
        "monomorphic_rules": [],
    }.items()
}

_best_results_dict = {
    rules_name: {
        seq[0]: TypeBestCost(seq[-1].cost(), "oracle", None) for seq, _ in list_of_seqs
    }
    for rules_name, list_of_seqs in _best_results.items()
}


def best_cost_for_exp(
    exp: Union[str, Expression, ExprWithEnv], rules_name: str
) -> TypeBestCost:
    """ 
    Get the best cost attainable for a given expression and ruleset.
    Requires entry for rules_name, but returns None for unknown exp.
        exp may be an Expression or S-Expression string; alpha conversion will be applied. """
    # check if exp is an expression or expression name
    is_name = isinstance(exp, str) and not exp.startswith("(") and not exp.endswith(")")
    return None if is_name else _best_results_dict[rules_name].get(_maybe_parse(exp))


def oracle_sequence(exp, rules_name):
    """ Requires entry for rules_name, but returns empty list for unknown exp.
        exp may be an Expression or S-Expression string; alpha conversion will be applied. """
    exp = _maybe_parse(exp)
    seqs = [seq for seq, _ in _best_results[rules_name] if seq[0] == exp]
    assert len(seqs) <= 1
    return seqs[0] if len(seqs) > 0 else None


def known_exps(rules_name):
    return [seq[0] for seq, _ in _best_results[rules_name]]
