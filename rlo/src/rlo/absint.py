# mypy: ignore-errors
"""
An abstract interpreter (or cost model) for Knossos.

Given an expression.Expression object, computes the resulting value and cost of
computation. It is an *abstract* interpreter because it abstracts vector/tensor
values by throwing away element values and keeping only shape and type information.
Since we have functions, like `index`, from tensors to basic types, the interpreter
also supports abstract int, float, and bool values.

The main purpose is to allow computing cost of Knossos programs that depend on
the input size. For example, the expression
    (build 10 (lam i (mul i i)))
has cost = build_malloc_cost (cost of a build) + 10 * 2 (cost of a mul). If
build_malloc_cost = 100, then cost of the expression is 120. The expression
    (build 20 (lam i (mul i i)))
has cost build_malloc_cost + 20 * 2. If build_malloc_cost = 100, then the cost
of the expression is 140.

The reason for concretely evaluating programs that don't involve tensors is so
that we can support edefs that have user-supplied cost functions in the same ks
file. These cost functions can be interpreted on the abstract tensors, and if
the cost only depends on the shape/type, then we will get a concrete cost.

Values: see Value.typeof for the kinds of values supported by the interpreter.

Notes:

- Why don't we need an Any type?
All parametric polymorphic functions are defined in evaluate, and since
we only support ad hoc polymorphism, there needs to be a function defined
with the exact same type in the FunctionEnv.

TODO:
- automatically lift all args to abstract if mixed function doesn't exist and
    abstract one does?
- write preprocessor that creates ExternalFunction from cost/shape defs in ksc
- Just use AbstractInt etc instead of Type.Integer?
    (need to reimplement num_elements for Value)
"""

import collections
from functools import reduce
from math import exp, log
from operator import mul
from typing import Tuple, List, Dict, Callable, Any, Iterable

from rlo.costs import assumed_vector_size, build_malloc_cost, if_epsilon
from ksc.type import Type
from rlo.expression import Expression, EF
from rlo.expression_util import ExprWithEnv
from rlo.utils import singleton


_core_cost = {
    "assumed_vector_size": assumed_vector_size,
    "constant": 0,
    "variable": 0,
    "if": 1,
    "if_epsilon": if_epsilon,
    "let": 0.1,
    "tuple": 0.1,
    "select": 0,
    "apply": 2,
    "build": build_malloc_cost,
    "size": 1,
    "index": 1,
    "assert": 1,
}


@singleton
class AbstractInt:
    """An abstract value of type integer. A singleton class."""

    def __repr__(self):
        return "AbstractInt"

    def _sametype_op(self, other, name):
        if other == AbstractInt():
            return self
        raise ValueError(f"Cannot {name} {self} and {other}")

    def _comparison_op(self, other, name):
        if other == AbstractInt():
            return AbstractBool()
        raise ValueError(f"Cannot {name} {self} and {other}")

    def __add__(self, other):
        return self._sametype_op(other, "add")

    def __sub__(self, other):
        return self._sametype_op(other, "sub")

    def __mul__(self, other):
        return self._sametype_op(other, "mul")

    def __div__(self, other):
        return self._sametype_op(other, "div")

    def __gt__(self, other):
        return self._comparison_op(other, "gt")

    def __ge__(self, other):
        return self._comparison_op(other, "ge")


@singleton
class AbstractFloat:
    """An abstract value of type float. A singleton class."""

    def __repr__(self):
        return "AbstractFloat"

    def _sametype_op(self, other, name):
        if other == AbstractFloat():
            return self
        raise ValueError(f"Cannot {name} {self} and {other}")

    def _comparison_op(self, other, name):
        if other == AbstractFloat():
            return AbstractBool()
        raise ValueError(f"Cannot {name} {self} and {other}")

    def __add__(self, other):
        return self._sametype_op(other, "add")

    def __sub__(self, other):
        return self._sametype_op(other, "sub")

    def __mul__(self, other):
        return self._sametype_op(other, "mul")

    def __div__(self, other):
        return self._sametype_op(other, "div")

    def __gt__(self, other):
        return self._comparison_op(other, "gt")

    def __ge__(self, other):
        return self._comparison_op(other, "ge")


@singleton
class AbstractBool:
    """An abstract value of type bool. A singleton class."""

    def __repr__(self):
        return "AbstractBool"


class AbstractTensor:
    """An abstract rectangular tensor of a fixed shape and element type (but
    with unknown values). E.g. AbstractTensor((2, 3), Type.Float).
    """

    # NOTE: can be optimized if needed by hashconsing.
    def __init__(self, shp, typ: Type):
        assert not typ.is_tensor
        self.shp = shp
        self.typ = typ

    def __repr__(self):
        return f"AbstractTensor({self.shp}, {self.typ})"

    def __eq__(self, other):
        if isinstance(other, AbstractTensor):
            return self.shp == other.shp and self.typ == other.typ
        return False

    @property
    def num_elements(self):
        return reduce(mul, self.shp)

    def _elementwise_op(self, other, name):
        if (
            isinstance(other, AbstractTensor)
            and self.typ == other.typ
            and self.shp == other.shp
        ):
            per_elem_cost = {"add": 1, "sub": 1, "mul": 2, "div": 2,}[name]
            c = self.num_elements * per_elem_cost + _core_cost["build"]
            # TODO malloc cost can be refined since we know size of result
            # Also, why not incur malloc cost at let expressions?
            return self, c
        raise ValueError(f"Cannot {name} {self} and {other}")

    def _comparison_op(self, other, name):
        if (
            isinstance(other, AbstractTensor)
            and self.typ == other.typ
            and self.shp == other.shp
        ):
            return AbstractBool()
        raise ValueError(f"Cannot {name} {self} and {other}")

    def __gt__(self, other):
        return self._comparison_op(other, "gt")

    def __ge__(self, other):
        return self._comparison_op(other, "ge")


class LambdaVal:
    """A function closure: a wrapper around a lambda expression and an
    environment."""

    def __init__(self, lam_expr: Expression, env: Dict[str, Any]):
        assert lam_expr.op == "lam"
        self.lam_expr = lam_expr
        self.env = env

    def __repr__(self):
        return f"LambdaVal({self.lam_expr}, {self.env})"


class ExternalFunction:
    """A function. Consists of a name, argument types, and implementation
    (a python function that returns (value, cost)).
    """

    def __init__(
        self, name: str, arg_types: List[Any], impl: Callable[..., Tuple[Any, float]]
    ):
        self.name = name
        self.arg_types = arg_types
        self.impl = impl

    def __repr__(self):
        return f"ExternalFunction({self.name}, {self.arg_types}, ...)"

    @staticmethod
    def template_function(
        name: str,
        arg_types_list: List[List[Any]],
        impl: Callable[..., Tuple[Any, float]],
    ):
        """Utility function to create ExternalFunctions for a list of different
        arg_types that share a python implementation."""
        return (ExternalFunction(name, arg_types, impl) for arg_types in arg_types_list)


class Value:
    @staticmethod
    def typeof(x):
        """Returns the type of a value (in the abstract interpreter world).

        Used for dispatching function calls.
        """
        typ = type(x)
        if typ in [int, float, bool]:
            return typ
        if x == AbstractInt():
            return Type.Integer
        if x == AbstractFloat():
            return Type.Float
        if x == AbstractBool():
            return Type.Bool
        if isinstance(x, tuple):
            # We don't have an AbstractTuple class, we just use python tuples
            return tuple(Value.typeof(y) for y in x)
        if isinstance(x, AbstractTensor):
            # This is so that we can write a single implementation of sum for
            # all vector shapes and sizes
            return AbstractTensor
        if isinstance(x, LambdaVal):
            return LambdaVal
        if isinstance(x, ExternalFunction):
            return ExternalFunction
        raise ValueError(f"Unsupported value {x} of type {typ}")

    @staticmethod
    def of_type(typ: Type):
        """Creates an abstract value of the given Type type.
        
        Uses assumed_vector_size for the shape of tensors.
        """
        if typ == Type.Integer:
            return AbstractInt()
        elif typ == Type.Float:
            return AbstractFloat()
        elif typ == Type.Bool:
            return AbstractBool()
        elif typ.is_tuple:
            return tuple(Value.of_type(t) for t in typ.tuple_elems())
        elif typ.is_tensor:
            v = Value.of_type(typ.tensor_elem_type)
            if isinstance(v, AbstractTensor):
                return AbstractTensor(
                    (_core_cost["assumed_vector_size"],) + v.shp, v.typ
                )
            return AbstractTensor((_core_cost["assumed_vector_size"],), Value.typeof(v))
        raise ValueError(f"Unknown type {typ}")

    @staticmethod
    def abstract_like(x):
        """Abstract-ify the (potentially concrete) given value."""
        typ = type(x)
        if typ == int:
            return AbstractInt()
        elif typ == float:
            return AbstractFloat()
        elif typ == bool:
            return AbstractBool()
        elif x in [AbstractInt(), AbstractFloat(), AbstractBool()]:
            return x
        elif isinstance(x, AbstractTensor):
            return x
        elif isinstance(x, tuple):
            return tuple(Value.abstract_like(y) for y in x)
        raise ValueError(f"Unknown type {typ}")


class FunctionEnv:
    """A function environment: essentially a dictionary from (name, arg_types) ->
    ExternalFunction."""

    def __init__(self, functions: Iterable[ExternalFunction]):
        self.functions = {(f.name, *f.arg_types): f for f in functions}

    def __iter__(self):
        return iter(self.functions)

    @staticmethod
    def from_list(fns):
        """Creates a FunctionEnv from a list of (ExternalFunction or result of
        ExternalFunction.template_function).
        """

        def flatten(l):
            for el in l:
                if isinstance(el, collections.Iterable):
                    yield from flatten(el)
                else:
                    yield el

        return FunctionEnv(flatten(fns))

    def dispatch(self, name: str, arg_types: List[Any]):
        """Returns the function with name `name` and argument types `arg_types`."""
        return self.functions[(name, *arg_types)]


_builtins = FunctionEnv.from_list(
    [
        # TODO build some implicit casting logic into dispatch, enumerating
        # all types like we do here won't scale:
        ExternalFunction.template_function(
            "add",
            [
                [int, int],
                [float, float],
                [Type.Integer, Type.Integer],
                [Type.Float, Type.Float],
            ],
            lambda x, y: (x + y, 1),
        ),
        ExternalFunction(
            "add",
            [AbstractTensor, AbstractTensor],
            lambda x, y: x._elementwise_op(y, "add"),
        ),
        ExternalFunction.template_function(
            "sub", [[int, int], [float, float]], lambda x, y: (x - y, 1),
        ),
        ExternalFunction.template_function(
            "sub",
            [[int, Type.Integer], [Type.Integer, int], [Type.Integer, Type.Integer],],
            lambda x, y: (AbstractInt(), 1),
        ),
        ExternalFunction.template_function(
            "sub",
            [
                [int, Type.Float],
                [Type.Float, int],
                [float, Type.Float],
                [Type.Float, float],
                [Type.Integer, Type.Float],
                [Type.Float, Type.Integer],
                [Type.Float, Type.Float],
            ],
            lambda x, y: (AbstractFloat(), 1),
        ),
        ExternalFunction(
            "sub",
            [AbstractTensor, AbstractTensor],
            lambda x, y: x._elementwise_op(y, "sub"),
        ),
        ExternalFunction.template_function(
            "mul",
            [
                [int, int],
                [float, float],
                [Type.Integer, Type.Integer],
                [Type.Float, Type.Float],
            ],
            lambda x, y: (x * y, 2),
        ),
        ExternalFunction(
            "mul",
            [AbstractTensor, AbstractTensor],
            lambda x, y: x._elementwise_op(y, "mul"),
        ),
        ExternalFunction("div", [int, int], lambda x, y: (x // y, 2),),
        ExternalFunction("div", [float, float], lambda x, y: (x / y, 2),),
        ExternalFunction(
            "div", [Type.Integer, Type.Integer], lambda x, y: (AbstractInt(), 2),
        ),
        ExternalFunction.template_function(
            "div",
            [
                [Type.Float, Type.Float],
                [Type.Integer, Type.Float],
                [Type.Float, Type.Integer],
            ],
            lambda x, y: (AbstractFloat(), 2),
        ),
        ExternalFunction(
            "div",
            [AbstractTensor, AbstractTensor],
            lambda x, y: x._elementwise_op(y, "div"),
        ),
        ExternalFunction(
            "sum",
            [AbstractTensor],
            lambda x: (
                AbstractFloat()
                if x.typ == Type.Float
                else AbstractInt()
                if x.typ == Type.Integer
                else None,
                x.num_elements - 1,
            ),
        ),
        ExternalFunction.template_function(
            "eq", [[int, int], [float, float], [bool, bool],], lambda x, y: (x == y, 1),
        ),
        ExternalFunction.template_function(
            "eq",
            [
                [Type.Integer, Type.Integer],
                [Type.Float, Type.Float],
                [Type.Bool, Type.Bool],
                [AbstractTensor, AbstractTensor],
            ],
            lambda x, y: (AbstractBool(), 1),
        ),
        ExternalFunction.template_function(
            "gt",
            [
                [int, int],
                [float, float],
                [Type.Integer, Type.Integer],
                [Type.Float, Type.Float],
            ],
            lambda x, y: (x > y, 1),
        ),
        ExternalFunction.template_function(
            "gte",
            [
                [int, int],
                [float, float],
                [Type.Integer, Type.Integer],
                [Type.Float, Type.Float],
            ],
            lambda x, y: (x >= y, 1),
        ),
        ExternalFunction("to_float", [int], lambda x: (float(x), 1)),
        ExternalFunction("to_float", [Type.Integer], lambda x: (AbstractFloat(), 1)),
        ExternalFunction("exp", [float], lambda x: (exp(x), 4)),
        ExternalFunction("exp", [Type.Float], lambda x: (AbstractFloat(), 4)),
        ExternalFunction("log", [float], lambda x: (log(x), 4)),
        ExternalFunction("log", [Type.Float], lambda x: (AbstractFloat(), 4)),
        ExternalFunction.template_function(
            "$ranhashdoub", [[int], [Type.Integer]], lambda x: (AbstractFloat(), 1)
        ),
    ]
)


def evaluate(
    expr: Expression, env: Dict[str, Any], approx_mode: bool = True
) -> Tuple[Any, float]:
    """Interprets a Knossos expression `expr` and returns a (potentially abstract)
    value, and the cost of the evaluation. Performs abstract interpretation by
    using AbstractVal instead of vector values, essentially treating vectors as
    rectangular tensors with unknown values.

    `env`: the environment (map from variable names to values) under which to
    evaluate `expr`.
    `approx_mode`: if true, sacrifice accuracy to return the same result as
    Expression.cost(). For example, if false, then we use concrete values for
    if conditions and build lengths when available.
    """
    if expr.op == "constant":
        return expr.value, _core_cost["constant"]

    if expr.op == "variable":
        if (
            expr.name not in env
            and expr.type.kind == "Lam"
            and ("cost$" + expr.name) in env
        ):
            # External function / edef, where we have been given the cost$ function.
            # The type of the edef has been stored into the variable by type propagation.
            def calc_cost(arg):
                # Evaluate the cost$fn applied to a fresh variable <arg_name> whose value (in the environment) is arg.
                for suf in range(len(env) + 1):
                    arg_name = "arg_" + str(suf)
                    if arg_name not in env:
                        cost_expr = EF.Apply(
                            Expression.Variable("cost$" + expr.name),
                            Expression.Variable(arg_name),
                        )
                        cost_env = {**env, arg_name: arg}
                        res_cost, _ = evaluate(cost_expr, cost_env, approx_mode)
                        return res_cost
                raise AssertionError("Unreachable")

            val = ExternalFunction(
                expr.name,
                None,
                lambda arg: (Value.of_type(expr.type.lam_return_type), calc_cost(arg)),
            )
        else:
            val = env[expr.name]
        return val, _core_cost["variable"]

    if expr.op == "let":
        assert expr.first.op == "variable"
        x = expr.first.name
        v, cost_v = evaluate(expr.second, env, approx_mode)
        res, cost_res = evaluate(expr.third, {**env, x: v}, approx_mode)
        return res, _core_cost["let"] + cost_v + cost_res

    if expr.op == "if":
        cond, cost_cond = evaluate(expr.first, env, approx_mode)
        if not approx_mode and type(cond) == bool:
            if cond:
                res, cost_res = evaluate(expr.second, env, approx_mode)
            else:
                res, cost_res = evaluate(expr.third, env, approx_mode)
            return res, _core_cost["if"] + cost_cond + cost_res
        elif approx_mode or cond == AbstractBool():
            results, costs = zip(
                *(evaluate(e, env, approx_mode) for e in [expr.second, expr.third])
            )
            if results[0] == results[1]:
                res = results[0]
            else:
                # If the branches return different values, then abstract them
                results = [Value.abstract_like(r) for r in results]
                # Check they have the same type and shape
                assert results[0] == results[1]
                res = results[0]
            # Cost = 1 * expensive branch + epsilon * cheap branch
            # Prevents if(True, a, 0) being cheaper than a, for expensive a
            cost_branches = sum(
                c * weight
                for c, weight in zip(sorted(costs), [_core_cost["if_epsilon"], 1])
            )
            return res, _core_cost["if"] + cost_cond + cost_branches
        else:
            raise ValueError(f"If's condition had unexpected type {type(cond)}")

    if expr.op == "lam":
        # NOTE might be able to optimize by substituting the values of env into
        # the function body at closure-creation time?
        assert expr.left.op == "variable"
        # Cost of the lambda is in its application
        return LambdaVal(expr, env.copy()), 0

    # Polymorphic functions like select and tuple are not Functions until
    # FunctionEnv supports parametric polymorphic lookup (needs Type.Any?)

    if expr.op == "select":
        tup, cost_tup = evaluate(expr.left, env, approx_mode)
        idx, cost_idx = evaluate(expr.right, env, approx_mode)
        assert type(idx) == int
        if isinstance(tup, tuple):
            res = tup[idx]
        else:
            raise ValueError(f"select called on type {type(tup)}")
        return res, _core_cost["select"] + cost_tup + cost_idx

    if expr.op == "tuple":
        # Handle nullary tuples (present in gmm_train.kso)
        if len(expr.children) == 0:
            return (), 0.0
        args, costs = zip(*(evaluate(x, env, approx_mode) for x in expr.children))
        res = args
        assert type(res) == tuple
        return res, _core_cost["tuple"] * len(args) + sum(costs)

    if expr.op == "index":
        idx, cost_idx = evaluate(expr.left, env, approx_mode)
        vec, cost_vec = evaluate(expr.right, env, approx_mode)
        c = _core_cost["index"] + cost_idx + cost_vec
        if (isinstance(idx, int) or idx == AbstractInt()) and isinstance(
            vec, AbstractTensor
        ):
            if len(vec.shp) > 1:
                return AbstractTensor(vec.shp[1:], vec.typ), c
            return Value.of_type(vec.typ), c
        raise ValueError(f"index called on unsupported values {idx}, {vec}")

    if expr.op == "size":
        tup, cost_tup = evaluate(expr.only_child, env, approx_mode)
        if isinstance(tup, AbstractTensor):
            return tup.shp[0], _core_cost["size"] + cost_tup
        raise ValueError(f"size called on unsupported value {tup}")

    if expr.op == "stop":
        # Assuming declared_cost for stop is only used for edefs,
        # which is handled under "let" above
        return evaluate(expr.only_child, env, approx_mode)

    if expr.op in ["build", "sumbuild"]:
        # We infer the shape and type of the resulting AbstractTensor
        length, cost_len = evaluate(expr.first, env, approx_mode)
        assert length == AbstractInt() or (type(length) == int and length >= 0)
        # If we don't know what the length is, or we are emulating Expression.cost,
        # use assumed_vector_size
        # NOTE: danger here is that build 2*n has less cost than two build n...
        if length == AbstractInt() or approx_mode:
            length = _core_cost["assumed_vector_size"]
        assert expr.second.op == "variable"
        body, cost_body = evaluate(
            expr.third, {**env, expr.second.name: AbstractInt()}, approx_mode,
        )
        # cost_body is then the cost of computing all the elements
        cost_body = cost_body * length
        if expr.op == "build":
            c = cost_len + cost_body + _core_cost["build"]
            if isinstance(body, AbstractTensor):
                body_shp = body.shp
                body_typ = body.typ
            else:
                body_shp = ()
                body_typ = Value.typeof(Value.abstract_like(body))
            return AbstractTensor((length,) + body_shp, body_typ), c
        else:
            if isinstance(body, AbstractTensor):
                body_elements = body.num_elements
            else:
                body_elements = 1
            cost_add = (length - 1) * body_elements
            c = cost_len + cost_body + cost_add
            return Value.abstract_like(body), c

    if expr.op == "assert":
        cond, cost_cond = evaluate(expr.left, env, approx_mode)
        body, cost_body = evaluate(expr.right, env, approx_mode)
        return body, _core_cost["assert"] + cost_cond + cost_body

    # Everything else is assumed to be a function call/application:
    children = tuple(evaluate(x, env, approx_mode) for x in expr.children)
    args, costs = zip(*children)
    typs = [Value.typeof(a) for a in args]
    if expr.op == "apply":
        fn = args[0]
        args, typs = args[1:], typs[1:]
        c = _core_cost["apply"]
    else:
        # Treat everything else as a built-in
        # NOTE: ideally, these would also be 'apply' expressions
        fn = _builtins.dispatch(expr.op, typs)
        c = 0
    if isinstance(fn, LambdaVal):
        assert len(args) == 1, "Lambda functions expect a single argument"
        res, cost_res = evaluate(
            fn.lam_expr.right, {**fn.env, fn.lam_expr.left.name: args[0]}, approx_mode
        )
    elif isinstance(fn, ExternalFunction):
        res, cost_res = fn.impl(*args)
    else:
        ValueError(f"Expected function value, got {fn} of type {type(fn)}")
    return res, c + sum(costs) + cost_res


def cost(exprenv: ExprWithEnv, inputs: Any = None, approx_mode: bool = True) -> float:
    """Evaluates the cost of the expression `expr`.

    If `expr` evaluates to a lambda, and `inputs` is not None or `approx_mode` is
    true, then returns the cost of evaluating the lambda on the provided inputs.
    (inputs should be supported values e.g. AbstractTensor not numpy arrays.)

    `approx_mode` is a faster/more approximate algorithm that immediately
    abstracts booleans and vector lenghts. Initially, this is intended to provide
    the same result as `Expression.cost` to aid switching over to this cost model.
    """
    env = {}
    for name, entry in exprenv.env.defs.items():
        env[name] = LambdaVal(entry, env)
    v, cost_expr = evaluate(exprenv.expr, env, approx_mode)
    if isinstance(v, LambdaVal):
        if inputs is None:
            arg_type = v.lam_expr.left.type
            inputs = Value.of_type(arg_type)
        _, cost_eval = evaluate(
            v.lam_expr.right, {**v.env, v.lam_expr.left.name: inputs}, approx_mode
        )
    else:
        assert inputs is None, "Only lambda expressions need inputs"
        cost_eval = 0
    return cost_expr + cost_eval


def _get_benchmarks():
    from rlo.expr_sets import get_expression_set

    return get_expression_set("ksc/blas/blas_combined.kso").named_exprenvs()


def _timeit(n=10000):
    from timeit import timeit

    benchmarks = _get_benchmarks()

    def run(baseline=False):
        for _, e in benchmarks:
            if baseline:
                _ = e._compute_cost_type()
            else:
                _ = cost(e)

    print(timeit(lambda: run(baseline=True), number=n))
    print(timeit(lambda: run(baseline=False), number=n))


def _profile():
    import cProfile
    import pstats

    benchmarks = _get_benchmarks()

    def run():
        for _ in range(1000):
            for _, e in benchmarks:
                _ = cost(e)

    cProfile.runctx("run()", globals(), locals(), "absint")
    p = pstats.Stats("absint")
    p.sort_stats("cumulative").print_stats(10)


if __name__ == "__main__":
    _timeit()
    # _profile()
