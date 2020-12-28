from typing import Tuple
from functools import wraps
import numpy
import ksc
from ksc.abstract_value import AbstractValue, ExecutionContext, current_execution_context
from ksc.type import Type, SizeType
from ksc.utils import ShapeType, Shape, TensorShape, make_dims

from . import common
from .common import (
    add,
    sub,
    mul,
    div,
    div_ii,
    div_ff,
    eq,
    lt,
    gt,
    lte,
    gte,
    or_,
    and_,
    abs_,
    max_,
    neg,
    pow,
    to_float_i
)

_built_ins = common._core_built_ins

def check_args_and_get_context(name, args, concrete="concrete"):
    context = None
    if all(not isinstance(arg, AbstractValue) for arg in args):
        # All arguments are concrete. This can happen due to the
        # compilation of the cost function (e.g., mul@ii)
        return concrete
    for i, arg in enumerate(args):
        if not isinstance(arg, AbstractValue) or arg.context is None:
            continue
        ctx = arg.context
        if (context is None or context == concrete) and ctx is not None:
            context = ctx
        assert (ctx is None
                 or ctx == concrete
                 or ctx == context), (f"In the call {name}, expected"
                                      f" {context} for arg#{i+1},"
                                      f" but got {ctx}")
    return context

def _get_data(value):
    if isinstance(value, AbstractValue):
        return value.data
    return value

def _get_edef(defs, name, type, py_name_for_concrete):
    shape_def = defs[f"shape${name}"]
    cost_def = defs[f"cost${name}"]
    @wraps(shape_def)
    def f(*args):
        context = check_args_and_get_context(name, args)
        # assert context is not None, f"In the call to {name}, got no context"
        if context == "concrete":
            d = ksc.backends.abstract.__dict__
            f = d[py_name_for_concrete]
            return AbstractValue.from_data(f(*[_get_data(arg) for arg in args]), context)
        else:
            shape_cost_args = [AbstractValue.in_context(arg, "concrete") for arg in args]
            with ExecutionContext():
                # execute in a new context so that the cost of computing shape and cost
                # is not accumulated
                ks_shape = _get_data(shape_def(*shape_cost_args))
                shape = Shape.from_ks_shape(ks_shape, type)
                cost = _get_data(cost_def(*shape_cost_args))
            exec_ctx = current_execution_context()
            exec_ctx.accumulate_cost(name, context, cost)
            return AbstractValue(shape, type, context=context)
    f.__name__ = name
    f.__qualname__ = f"{name} [edef]"
    return f

def index(i, v):
    st = v.shape_type
    assert st.type.is_tensor, f"Called index on non-tensor {v}"
    exec_ctx = current_execution_context()
    exec_ctx.accumulate_cost(
        "index",
        v.context,
        exec_ctx.config["index_cost"]
    )
    return AbstractValue(st.shape.elem_shape, st.type.tensor_elem_type, context=v.context)

def size(v):
    st = v.shape_type
    assert st.type.is_tensor, f"Called size on non-tensor {v}"
    exec_ctx = current_execution_context()
    exec_ctx.accumulate_cost("size", v.context, exec_ctx.config["size_cost"])
    if len(st.shape.dims) == 1: #TOUNDO: size(vec) returns int
        return AbstractValue.from_data(st.shape.dims[0], v.context)
    else:
        return AbstractValue.from_data(st.shape.dims, v.context)

def _compute_branch_cost(f):
    # evaluate f in a new context
    with ExecutionContext() as ctx:
        out = f()
    return out, ctx.costs[None]

def _compute_build_inner_cost(n, f):
    n = _get_data(n)
    if n is None:
        exec_ctx = current_execution_context()
        n = exec_ctx.config["assumed_vector_size"]

    dims = make_dims(n)

    rank = len(dims)
    nelem = numpy.prod(dims)

    i = AbstractValue(Shape.of_size(rank), SizeType.from_rank(rank))
    el, cost = _compute_branch_cost(lambda: f(i))
    return dims, nelem, el, cost

def build(sz, f):
    context = check_args_and_get_context("build", [sz], concrete=None)
    dims, nelem, el, inner_cost = _compute_build_inner_cost(sz, f)
    exec_ctx = current_execution_context()
    exec_ctx.accumulate_cost(
        "build",
        context,
        exec_ctx.config["build_malloc_cost"] + nelem * inner_cost
    )
    
    rank = len(dims)
    ks_type = Type.Tensor(rank, el.get_type)
    return AbstractValue(TensorShape(dims, el.get_shape), ks_type, context=context)

def sumbuild(sz, f):
    context = check_args_and_get_context("sumbuild", [sz], concrete=None)
    _, nelem, el, inner_cost = _compute_build_inner_cost(sz, f)
    elst = el.shape_type
    exec_ctx = current_execution_context()
    exec_ctx.accumulate_cost(
        "sumbuild",
        context,
        nelem * inner_cost + (nelem - 1) * elst.type.num_elements(assumed_vector_size=exec_ctx.config["assumed_vector_size"])
    )
    return AbstractValue(elst.shape, elst.type, context=context)

def fold(f, s0, xs):
    raise NotImplementedError

def make_tuple(*args):
    context = check_args_and_get_context("tuple", args)
    child_shapes = tuple(arg.shape_type.shape for arg in args)
    child_types = tuple(arg.shape_type.type for arg in args)
    child_data = tuple(_get_data(arg) for arg in args)
    exec_ctx = current_execution_context()
    exec_ctx.accumulate_cost("tuple", context, exec_ctx.config["let_cost"] * len(args))
    return AbstractValue(child_shapes, Type.Tuple(*child_types), child_data, context)

def get_tuple_element(i, tup):
    el_shape = tup.shape_type.shape[i]
    el_type = tup.shape_type.type.children[i]
    tup_data = _get_data(tup)
    el_data = tup_data[i] if isinstance(tup_data, tuple) else None
    exec_ctx = current_execution_context()
    exec_ctx.accumulate_cost("select", tup.context, exec_ctx.config["select_cost"])
    return AbstractValue(el_shape, el_type, el_data, tup.context)

def let(tupled, var, body):
    context = check_args_and_get_context("let", [var])
    exec_ctx = current_execution_context()
    exec_ctx.accumulate_cost("let", context, exec_ctx.config["let_cost"])
    if tupled:
        return body(*var)
    else:
        return body(var)

def if_then_else(cond, then_branch, else_branch):
    context = check_args_and_get_context("if", [cond])
    cond = _get_data(cond)
    exec_ctx = current_execution_context()
    exec_ctx.accumulate_cost("if", context, exec_ctx.config["if_selection_cost"])
    if cond is None:
        # branch is undecidable at compile time
        out1, then_cost = _compute_branch_cost(then_branch)
        out2, else_cost = _compute_branch_cost(else_branch)
        out1 = AbstractValue.abstract_like(out1)
        out2 = AbstractValue.abstract_like(out2)
        assert out1.shape_type == out2.shape_type
        assert out1.context == out2.context
        if_epsilon = exec_ctx.config["if_epsilon"]
        exec_ctx.accumulate_cost("if",
                                 context,
                                 (max(then_cost, else_cost)
                                  + if_epsilon * min(then_cost, else_cost)))
        return out1
    elif cond:
        return then_branch()
    else:
        return else_branch()

def assert_(cond, body):
    assert cond
    return body
