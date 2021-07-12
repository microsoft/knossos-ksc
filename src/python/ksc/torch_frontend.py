from typing import Callable, List, Tuple
from contextlib import contextmanager

import functools
import numpy
import torch
import torch.onnx

torch.set_default_dtype(torch.float32)

from ksc import utils
from ksc.parse_ks import parse_ks_filename
from ksc.compile import (
    build_module_using_pytorch_from_ks,
    build_module_using_pytorch_from_cpp,
)

from ksc.type import Type
from ksc.expr import Expr, Def, EDef, GDef, Rule, Const, Var, Lam, Call, Let, If, Assert
from ksc.expr import StructuredName

from ksc.type_propagate import type_propagate_decls

# Importing prettyprint to get the decorated printers for Expression and Type
import ksc.prettyprint  # pylint: disable=unused-import

# Import the prettyprinter routines we use explicitly in this file
from prettyprinter import cpprint, pformat

# Needed this in order to see the error messages when pprint fails
import warnings

import typing

warnings.filterwarnings("always")

# Background reading
# https://github.com/pytorch/pytorch/blob/master/torch/csrc/jit/OVERVIEW.md
# https://pytorch.org/docs/master/jit.html#interpreting-graphs
# https://github.com/pytorch/pytorch/blob/8fe2a5e91b79e3f9b6f6c632fdf7f39ec3bf4fca/torch/csrc/jit/ir/ir.h

# CallMethod resolution:
# https://github.com/pytorch/pytorch/blob/b6bb644e41b3928b5a515330ad35c8b447fcb876/torch/csrc/jit/serialization/python_print.cpp#L984-L1004


def tail(iter):
    next(iter)
    return iter


# TODO: make this less pythonic, i.e. eschew global state modification
todo_stack = None


def type_from_value(x):
    if isinstance(x, torch.Tensor):
        return Type.Tensor(len(x.size()), from_torch_dtype(x.dtype))

    return Type.fromValue(x)


make_new_var_index = 0


def make_new_var(type=None):
    global make_new_var_index
    name = f"ts2ks${make_new_var_index}"
    make_new_var_index += 1
    return Var(name, type)


def make_arg(input, example_input):
    input_type = from_torch_type(input.type())
    example_input_type = type_from_value(example_input)
    if example_input_type.kind != input_type.kind:
        print(
            f"Warn: example input type {example_input_type} differs from TorchScript input type {input_type}"
        )
    else:
        # input_type will have wrong shape, grab example_input_type
        # TODO: shape propagation, or full abstract interpretation
        input_type = example_input_type

    name = mangled_name(input)
    return Var(name, input_type)


def mangled_name(node):
    return "_" + utils.encode_name(node.debugName())


def from_torch_dtype(t):
    if t == torch.int64:
        return Type.Integer

    if t == torch.float32:
        return Type.Float

    raise NotImplementedError


def from_torch_type(t):
    if type(t) == torch._C.IntType:
        return Type.Integer

    if type(t) == torch._C.FloatType:
        return Type.Float

    if type(t) == torch._C.TensorType:
        if t.scalarType() is not None:
            return Type.Tensor(-1, from_torch_type(t.scalarType()))
        else:
            print("Assuming Tensor type is float")
            return Type.Tensor(-1, Type.Float)

    type_lookup = str(t)

    if "Optional" in type_lookup:
        print("WARNING: Optional argument treated as required:" + type_lookup)

    assert False


def var_or_constant(node):
    val = node.toIValue()
    if val is None:
        return Var(mangled_name(node))
    else:
        return Const(val)


def make_constant(node):

    value = node.outputsAt(0)

    if isinstance(value.type(), torch._C.NoneType):
        val = Call("tuple", [])

    elif isinstance(value.type(), torch._C.BoolType):
        val = Const(value.toIValue())
        assert val.type_ == Type.Bool

    else:
        try:
            possibleLiteral = value.toIValue()
            if possibleLiteral is None:
                literal = 0.0
            elif possibleLiteral == 0:
                literal = 0.0
            else:
                literal = possibleLiteral
        except RuntimeError:  # TODO: do we have to try/except?
            literal = "FUNCTIONCALL"
        val = Const(literal)

    return Var("_" + value.debugName()), val


def make_list(node):
    value = node.outputsAt(0)
    return (
        Var(mangled_name(value)),
        Call("Vec_init", [var_or_constant(i) for i in node.inputs()]),
    )


def make_tuple(node):
    value = node.outputsAt(0)
    return (
        Var(mangled_name(value)),
        Call("tuple", [var_or_constant(i) for i in node.inputs()]),
    )


def make_tensor(node):
    # tensors aren't explicitly modelled in Knossos yet, leave them as identity over a (jagged) list for now
    value = node.outputsAt(0)
    return Var(mangled_name(value)), var_or_constant(node.inputsAt(0))


def make_aten_function(node, value, function_name):
    return (
        Var(mangled_name(value)),
        Call(function_name, [var_or_constant(i) for i in node.inputs()]),
    )


def make_return(node):
    mangled_id = mangled_name(node.inputsAt(0))
    return Var(mangled_id)


def make_callfunction(node):
    value = node.outputsAt(0)
    assert len(list(node.outputs())) == 1  # Assemble into tuple for multiple outputs
    function_name_constant = node.inputsAt(0).node()
    function_name = function_name_constant.s("name")
    return (
        Var(mangled_name(value)),
        Call(function_name, [var_or_constant(i) for i in tail(node.inputs())]),
    )


def make_PythonOp(node):
    value = node.outputsAt(0)
    assert len(list(node.outputs())) == 1  # Assemble into tuple for multiple outputs
    pyname = node.pyname()
    if pyname == "elementwise_apply_hack":
        global todo_stack
        function_name_constant = node.inputsAt(0).node()
        function_name = function_name_constant.s("value")
        print(f"Adding {function_name} to todo {todo_stack}")
        todo_stack.add(function_name)

        var = make_new_var(Type.Float)  # TODO: need to propagate properly
        map_lambda = Lam(var, Call(function_name, [var]))

        return (
            Var(mangled_name(value)),
            Call(
                "map", [map_lambda] + [var_or_constant(i) for i in tail(node.inputs())]
            ),
        )

    raise NotImplementedError(f"PythonOp {pyname}")


def make_lets(bindings, body) -> Expr:
    for (v, rhs) in reversed(bindings):
        body = Let(v, rhs, body)
    return body


def make_loop(make_binds, node):
    # def foofilter_comp(xs: Tensor) -> Tensor:
    #     _0 = annotate(List[Tensor], [])
    #     _1 = ops.prim.min([9223372036854775807, torch.len(xs)])
    #     for n in range(_1):
    #         x = torch.select(xs, 0, n)
    #         if bool(torch.lt(x, 0.)):
    #             _2 = torch.mul(x, -0.125)
    #         else:
    #             _3 = torch.div(1, n)
    #             _2 = torch.mul(torch.pow(x, 2), _3)
    #         _4 = torch.append(_0, _2)
    #     t = torch.tensor(_0, dtype=None, device=None, requires_grad=False)
    #     _5 = torch.mean(torch.mul(torch.sin(t), t), dtype=None)
    #     return _5

    # https://github.com/pytorch/pytorch/blob/master/torch/csrc/jit/OVERVIEW.md#loops
    # %y_1, ..., %y_r = prim::Loop(%max_trip_count, %initial_condition, %x_1, ..., %x_r)
    #                     block0(%i, %a_1, ..., %a_r):
    #                         %b_1, ..., %b_m = some::node(%a_value_from_outer_block, %a_1)
    #                         %iter_condition = some::other_node(%a_2)
    #                         -> (%iter_condition, %b_1, ..., %b_r)

    max_trip_count, initial_condition, *x_nodes = node.inputs()
    y_nodes = tuple(node.outputs())
    (block0,) = node.blocks()
    i, *a_nodes = block0.inputs()
    iter_condition, *b_nodes = block0.outputs()

    # For now, match only the for loop version
    assert make_constant(iter_condition.node())[1] == Const(True)
    assert len(a_nodes) == 0
    assert len(b_nodes) == 0
    assert len(y_nodes) == 0

    body_nodes = list(block0.nodes())
    last = body_nodes[-1]
    if last.kind() == "aten::append":
        # Special case for list comprehensions
        l, item = last.inputs()
        assert l.node().kind() == "prim::ListConstruct"

        binds = make_binds(body_nodes[:-1])
        lam_body = make_lets(binds, var_or_constant(item))

        lam = Lam(Var(mangled_name(i), Type.Integer), lam_body)

        return var_or_constant(l), Call("build", [var_or_constant(max_trip_count), lam])

    print(node)
    assert False


def make_if(make_binds, node):
    def make_branch(block):
        binds = make_binds(block.nodes())
        body = make_return(block.returnNode())
        return make_lets(binds, body)

    identifier = None
    if node.outputsSize() == 0:
        identifier = "_" + "dummy"  # we're likely to need to make them unique
    else:
        identifier = "_" + node.outputsAt(0).debugName()

    inputs_size = sum(1 for _ in node.inputs())

    if inputs_size != 1:
        raise Exception(
            "Only support conditionals with 1 input, this one had: " + str(inputs_size)
        )

    conditional = mangled_name(node.inputsAt(0))

    blocks = list(
        node.blocks()
    )  # TODO: check length exactly 2, only supporting if/else currently. This is enough for BERT example

    success_branch = make_branch(blocks[0])
    failure_branch = make_branch(blocks[1])

    return Var(identifier), If(Var(conditional), success_branch, failure_branch)


def ts2ks_fromgraph(generate_edefs, name, graph, example_inputs):
    def translate_node(make_binds, node) -> Tuple[Var, Expr]:
        lookups: typing.Dict[str, Callable] = {
            "prim::Constant": make_constant,
            "prim::ListConstruct": make_list,
            "prim::TupleConstruct": make_tuple,
            "prim::Return": make_return,
            "prim::CallFunction": make_callfunction,
            "prim::PythonOp": make_PythonOp,
            "prim::Loop": functools.partial(make_loop, make_binds=make_binds),
            "prim::If": functools.partial(make_if, make_binds=make_binds),
        }

        kind = node.kind()
        if kind in lookups:
            return lookups[kind](node=node)

        primfuns = ["prim::min"]
        if kind.startswith("aten::") or kind in primfuns:
            return make_aten_function(node, node.outputsAt(0), kind)

        print("WARNING, unimplmented node kind: " + node.kind())
        return Var("ERR"), Var(node.kind())

    def make_binds(nodes) -> List[Tuple[Var, Expr]]:
        return [translate_node(make_binds, node) for node in nodes]

    all_nodes = list(graph.nodes())

    binds = make_binds(all_nodes)

    args = [
        make_arg(input, example)
        for input, example in zip(graph.inputs(), example_inputs)
        # if (not input.type().str().startswith("__torch__.transformers"))
    ]  # filtering self, TODO: look for better way

    print_count = sum(1 for node in all_nodes if node.kind() == "prim::Print")

    # HACK: if last operation is print, we want that otherwise it's a return value.
    # need to think about interaction between imperative Python and pure Knossos
    if all_nodes[-1].kind() == "prim::Print":
        if print_count > 1:
            print(
                "WARNING: multiple print statements used, only final one currently translated"
            )
        op = translate_node(make_binds, all_nodes[-1])
        return_type = Type.Integer
    else:
        if print_count > 0:
            print(
                "WARNING: print statement currently only supported as final operation"
            )
        return_node = graph.return_node()
        op = translate_node(make_binds, return_node)
        return_type = None  # Infer return type in type propagation from_torch_type(return_node.inputsAt(0).type())

    body = make_lets(binds, op)

    return Def(StructuredName(name), return_type, args, body)


def make_tuple_if_many_args(*args):
    t = tuple(*args)
    if len(t) == 1:
        return t[0]
    else:
        return t


# TODO: make an configuration named tuple rather than passing flags
def ts2ks(output, generate_edefs, function, example_inputs):
    s = ts2ks_fromgraph(generate_edefs, function.name, function.graph, example_inputs)
    output.write(pformat(s))


def torch_from_ks(ks_object):
    if isinstance(ks_object, tuple):
        return tuple(torch_from_ks(ks) for ks in ks_object)

    return torch.from_numpy(numpy.array(ks_object, copy=True))


def torch_to_ks(py_mod, val):
    """
    Return a KS-compatible version of val.
    If val is a scalar, just return it as a float.
    If val is a tensor, we may need to
       (a) make it contiguous
       (b) ensure it's not garbage-collected while we're holding a view to it.
    """
    if isinstance(val, float):
        return val

    if isinstance(val, torch.Tensor):
        assert (
            val.dtype == torch.float32
        ), "TODO: https://github.com/microsoft/knossos-ksc/issues/691"
        if len(val.shape) == 0:
            return val.item()

        val = val.contiguous()  # Get data, or copy if not already contiguous
        if len(val.shape) == 1:
            ks_tensor = py_mod.Tensor_1_Float(val.data_ptr(), *val.shape)
        if len(val.shape) == 2:
            ks_tensor = py_mod.Tensor_2_Float(val.data_ptr(), *val.shape)
        ks_tensor._torch_val = val  # Stash object inside return value to prevent premature garbage collection
        return ks_tensor

    raise NotImplementedError()


@contextmanager
def logging(py_mod, flag=True):
    """
    Turn on verbose logging in Knossos calls
    """
    old_flag = py_mod.logging(flag)
    try:
        yield
    finally:
        py_mod.logging(old_flag)


# Methods for the KscAutogradFunction class -- a new class will be made for each loaded module
# See https://pytorch.org/docs/stable/notes/extending.html
def forward_template(py_mod, ctx, *args):
    py_mod.reset_allocator()
    ks_args = (torch_to_ks(py_mod, x) for x in args)

    # Call it
    outputs = py_mod.entry(*ks_args)

    if ctx is not None:
        ctx.torch_vals = ks_args
        ctx.save_for_backward(*args)

    return torch_from_ks(outputs)


def backward_template(py_mod, generate_lm, ctx, *args):
    ks_args = make_tuple_if_many_args(torch_to_ks(py_mod, x) for x in ctx.saved_tensors)
    ks_grad_args = make_tuple_if_many_args(torch_to_ks(py_mod, x) for x in args)
    rev_entry = py_mod.rev_entry if generate_lm else py_mod.sufrev_entry
    outputs = rev_entry(ks_args, ks_grad_args)
    return torch_from_ks(outputs)


def make_KscAutogradFunction(py_mod, generate_lm):
    # We need to make a new class for every py_mod, as PyTorch requires forward and backward to be
    # staticmethods.  This is not too expensive, as each mod needs to be compiled anyway.
    forward = lambda ctx, args: forward_template(py_mod, ctx, args)
    backward = lambda ctx, args: backward_template(py_mod, generate_lm, ctx, args)
    newclass = type(
        "KscAutogradFunction_" + py_mod.__name__,
        (torch.autograd.Function,),
        {
            "py_mod": py_mod,
            "forward": staticmethod(forward),
            "backward": staticmethod(backward),
            "adapt": staticmethod(lambda x: torch_to_ks(py_mod, x)),
        },
    )
    return newclass()


def ksc_defs_to_module(ksc_defs, entry_def, derivatives_to_generate):
    symtab = dict()
    ksc_dir = utils.get_ksc_dir()
    decls_prelude = list(parse_ks_filename(ksc_dir + "/src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    decls_prelude_aten = list(
        parse_ks_filename(ksc_dir + "/src/runtime/prelude-aten.ks")
    )
    type_propagate_decls(decls_prelude_aten, symtab)

    for ksc_def in ksc_defs:
        cpprint(ksc_def)
        print("")

    type_propagate_decls(ksc_defs, symtab)
    defs_with_derivatives = []
    for ksc_def in ksc_defs:
        defs_with_derivatives += [ksc_def]
        if "sufrev" in derivatives_to_generate:
            defs_with_derivatives += [
                GDef("suffwdpass", ksc_def.name),
                GDef("sufrevpass", ksc_def.name),
                GDef("sufrev", ksc_def.name),
            ]
        if "fwd" in derivatives_to_generate:
            defs_with_derivatives += [
                GDef("fwd", ksc_def.name),
            ]
        if "rev" in derivatives_to_generate:
            defs_with_derivatives += [
                GDef("rev", ksc_def.name),
            ]

    ks_str = "\n".join(map(pformat, defs_with_derivatives))

    return ksc_string_to_module(ks_str, entry_def.name, derivatives_to_generate)


def ksc_string_to_module(ks_str, entry_sn, derivatives_to_generate):
    bindings_to_generate = [("entry", entry_sn)] + [
        (f"{der}_entry", StructuredName((der, entry_sn)))
        for der in derivatives_to_generate
    ]

    return build_module_using_pytorch_from_ks(
        ks_str, bindings_to_generate, use_aten=True
    )


def cpp_string_to_module(cpp_str, entry_name, derivatives_to_generate):
    bindings_to_generate = [("entry", entry_name)] + [
        (f"{der}_entry", f"{der}_{entry_name}") for der in derivatives_to_generate
    ]

    return build_module_using_pytorch_from_cpp(
        cpp_str, bindings_to_generate, use_aten=True,
    )


def ksc_defs_to_autograd_function(ksc_defs, entry_def, generate_lm=True):
    derivatives_to_generate = ["fwd", "rev"] if generate_lm else ["sufrev"]
    mod = ksc_defs_to_module(ksc_defs, entry_def, derivatives_to_generate)
    return make_KscAutogradFunction(mod, generate_lm)


def ksc_string_to_autograd_function(ks_str, entry_sn, generate_lm):
    derivatives_to_generate = ["fwd", "rev"] if generate_lm else ["sufrev"]
    mod = ksc_string_to_module(ks_str, entry_sn, derivatives_to_generate)
    return make_KscAutogradFunction(mod, generate_lm)


def cpp_string_to_autograd_function(cpp_str, entry_name, generate_lm):
    derivatives_to_generate = ["fwd", "rev"] if generate_lm else ["sufrev"]
    mod = cpp_string_to_module(cpp_str, entry_name, derivatives_to_generate)
    return make_KscAutogradFunction(mod, generate_lm)


import inspect


def tsmod2ksmod(module, function_name, example_inputs, generate_lm=True):
    global todo_stack
    todo_stack = {function_name}
    ksc_defs = []
    while len(todo_stack) > 0:
        print(f"tsmod2ksmod: Remaining: {todo_stack}")
        for fn in inspect.getmembers(module, inspect.isfunction):
            fn_name, fn_obj = fn
            if fn_name in todo_stack:
                todo_stack.remove(fn_name)
                print(f"tsmod2ksmod: converting {fn_name}, remaining: {todo_stack}")
                ts_fn = torch.jit.script(fn_obj)
                ts_graph = ts_fn.graph
                ksc_def = ts2ks_fromgraph(False, fn_name, ts_graph, example_inputs)
                ksc_defs.insert(0, ksc_def)

    entry_def = ksc_defs[-1]
    return ksc_defs_to_autograd_function(ksc_defs, entry_def, generate_lm)


def ts2mod(function, example_inputs, generate_lm=True):
    fn = torch.jit.script(function)
    ksc_def = ts2ks_fromgraph(False, fn.name, fn.graph, example_inputs)
    return ksc_defs_to_autograd_function([ksc_def], ksc_def, generate_lm)
