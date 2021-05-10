# %%

from typing import List, Tuple

import functools
import numpy
import torch
import torch.onnx

torch.set_default_dtype(torch.float64)

from ksc import utils
from ksc.parse_ks import parse_ks_filename

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
def make_new_var(type=None,decl=True):
    global make_new_var_index
    name = f"ts2ks${make_new_var_index}"
    make_new_var_index += 1
    return Var(name,type,True), Var(name,type,False)

def make_arg(input, example_input):
    input_type = from_torch_type(input.type())
    example_input_type = type_from_value(example_input)
    if example_input_type.kind != input_type.kind:
        print(f"Warn: example input type {example_input_type} differs from TorchScript input type {input_type}")
    else:
        # input_type will have wrong shape, grab example_input_type
        # TODO: shape propagation, or full abstract interpretation
        input_type = example_input_type

    name = mangled_name(input)
    return Var(name, input_type, decl=True)


def mangled_name(node):
    return "_" + utils.encode_name(node.debugName())


def from_torch_dtype(t):
    if t == torch.int64:
        return Type.Integer

    if t == torch.float64:
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
    return Var(mangled_name(value)), Call("Vec_init", [var_or_constant(i) for i in node.inputs()])


def make_tuple(node):
    value = node.outputsAt(0)
    return Var(mangled_name(value)), Call("tuple", [var_or_constant(i) for i in node.inputs()])


def make_tensor(node):
    # tensors aren't explicitly modelled in Knossos yet, leave them as identity over a (jagged) list for now
    value = node.outputsAt(0)
    return Var(mangled_name(value)), var_or_constant(node.inputsAt(0))


def make_aten_function(node, value, function_name):
    return Var(mangled_name(value)), Call(function_name, [var_or_constant(i) for i in node.inputs()])

def make_return(node):
    mangled_id = mangled_name(node.inputsAt(0))
    return Var(mangled_id)
    

def make_callfunction(node):
    value = node.outputsAt(0)
    assert len(list(node.outputs())) == 1  # Assemble into tuple for multiple outputs
    function_name_constant = node.inputsAt(0).node()
    function_name = function_name_constant.s("name")
    return (Var(mangled_name(value)), Call(function_name, [var_or_constant(i) for i in tail(node.inputs())]))


def make_PythonOp(node):
    value = node.outputsAt(0)
    assert len(list(node.outputs())) == 1 # Assemble into tuple for multiple outputs
    pyname = node.pyname()
    if pyname == "elementwise_apply_hack":
        global todo_stack
        function_name_constant = node.inputsAt(0).node()
        function_name = function_name_constant.s("value")
        print(f"Adding {function_name} to todo {todo_stack}")
        todo_stack.add(function_name)

        vardecl,var = make_new_var(Type.Float) # TODO: need to propagate properly
        map_lambda = Lam(vardecl, Call(function_name, [var]))

        return (Var(mangled_name(value)), 
                Call("map", [map_lambda] + [var_or_constant(i) for i in tail(node.inputs()) ]))

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

        lam = Lam(Var(mangled_name(i), Type.Integer, decl=True), lam_body)

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
        raise Exception("Only support conditionals with 1 input, this one had: " + str(inputs_size))

    conditional = mangled_name(node.inputsAt(0))

    blocks = list(
        node.blocks()
    )  # TODO: check length exactly 2, only supporting if/else currently. This is enough for BERT example

    success_branch = make_branch(blocks[0])
    failure_branch = make_branch(blocks[1])

    return Var(identifier), If(Var(conditional), success_branch, failure_branch)


def ts2ks_fromgraph(generate_edefs, name, graph, example_inputs):
    def translate_node(make_binds, node) -> Tuple[Var, Expr]:
        lookups = {
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
        for input,example in zip(graph.inputs(), example_inputs)
        # if (not input.type().str().startswith("__torch__.transformers"))
    ]  # filtering self, TODO: look for better way

    print_count = sum(1 for node in all_nodes if node.kind() == "prim::Print")

    # HACK: if last operation is print, we want that otherwise it's a return value.
    # need to think about interaction between imperative Python and pure Knossos
    if all_nodes[-1].kind() == "prim::Print":
        if print_count > 1:
            print("WARNING: multiple print statements used, only final one currently translated")
        op = translate_node(make_binds, all_nodes[-1])
        return_type = Type.Integer
    else:
        if print_count > 0:
            print("WARNING: print statement currently only supported as final operation")
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
    if isinstance(val, float):
        return val

    if isinstance(val, torch.Tensor):
        assert val.dtype == torch.float64, "TODO: https://github.com/microsoft/knossos-ksc/issues/691"
        if len(val.shape) == 2:
            return py_mod.Tensor_2_Float(val.data_ptr(), *val.shape)
        if len(val.shape) == 1:
            return py_mod.Tensor_1_Float(val.data_ptr(), *val.shape)
        if len(val.shape) == 0:
            return val.item()

    raise NotImplementedError()

#--------------------------------------------------------------------
#---- Alternative implementation, from PyTorch AST rather than Graph

#See https://github.com/pytorch/pytorch/blob/master/torch/csrc/jit/python/python_tree_views.cpp


from functools import singledispatch

from torch.jit.frontend import get_jit_def
import torch._C._jit_tree_views as ts
# import (
#     ClassDef, Ident, Stmt, Decl, Def, Var,
#     EmptyTypeAnnotation, Param, ExprStmt, Assign,
#     Delete, Return, Raise, Assert, AugAssign, While,
#     For, If, Pass, Break, Continue, Apply, Dots, Select,
#     TrueLiteral, FalseLiteral, NoneLiteral, Starred,
#     ListLiteral, TupleLiteral, DictLiteral, Const,
#     StringLiteral, ListComp, Attribute, BinOp, UnaryOp,
#     SliceExpr, Subscript, TernaryIf, With, WithItem, Property,
#     DictComp,
# )

@singledispatch
def ast2ks_aux(ast, symtab) -> Expr:
    """
    TorchScript AST to Knossos
    """
    raise NotImplementedError(f"ast2ks not implemented for {ast}")

@ast2ks_aux.register(ts.Def)
def _(ast, symtab):
    name = ast.name().name
    # ... a nice line of investigation but needs accessors for these tree_view classes
    # in https://github.com/pytorch/pytorch/blob/4cb534f92ef6f5b2ec99109b0329f93a859ae831/torch/csrc/jit/python/python_tree_views.cpp#L161
    # totally doable, but need to modify pytorch, not trivial

def ast2ks(function):
    ast = get_jit_def(function, function.__name__)
    ks = ast2ks_aux(ast, {})
    return ks

# Methods for the KscAutogradFunction class -- a new class will be made for each loaded module
def forward_template(py_mod, ctx, *args):
    py_mod.reset_allocator()
    ks_args = (torch_to_ks(py_mod, x) for x in args)

    # Call it
    outputs = py_mod.entry(*ks_args)

    # TODO: Use BOG properly
    if ctx is not None:
        ctx.save_for_backward(*args)

    return torch_from_ks(outputs)


def backward_template(py_mod, ctx, *args):
    ks_args = make_tuple_if_many_args(torch_to_ks(py_mod, x) for x in ctx.saved_tensors)
    ks_grad_args = make_tuple_if_many_args(torch_to_ks(py_mod, x) for x in args)
    outputs = py_mod.sufrev_entry(ks_args, ks_grad_args)

    return torch_from_ks(outputs)


def make_KscAutogradFunction(py_mod):
    # We need to make a new class for every py_mod, as PyTorch requires forward and backward to be
    # staticmethods.  This is not too expensive, as each mod needs to be compiled anyway.
    forward = lambda ctx, args: forward_template(py_mod, ctx, args)
    backward = lambda ctx, args: backward_template(py_mod, ctx, args)
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

def ksc_defs_to_module(ksc_defs, entry_def):
    symtab = dict()
    ksc_dir = utils.get_ksc_dir()
    decls_prelude = list(parse_ks_filename(ksc_dir + "/src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    decls_prelude_aten = list(parse_ks_filename(ksc_dir + "/src/runtime/prelude-aten.ks"))
    type_propagate_decls(decls_prelude_aten, symtab)

    for ksc_def in ksc_defs:
        cpprint(ksc_def)
        print("")

    type_propagate_decls(ksc_defs, symtab)
    defs_with_derivatives = []
    for ksc_def in ksc_defs: 
        defs_with_derivatives += [
            ksc_def,
            GDef("suffwdpass", ksc_def.name),
            GDef("sufrevpass", ksc_def.name),
            GDef("sufrev", ksc_def.name)
        ]

    ks_str = '\n'.join(map(pformat, defs_with_derivatives))
    arg_types = [arg.type_ for arg in entry_def.args]
    return_type = entry_def.return_type
    mod = utils.build_module_using_pytorch_from_ks(
            ks_str, entry_def.name, arg_types, return_type=return_type, generate_derivatives=True, use_aten=True)

    return make_KscAutogradFunction(mod)

import inspect

def tsmod2ksmod(module, function_name, example_inputs):
    global todo_stack
    todo_stack = {function_name}
    ksc_defs = []
    while len(todo_stack) > 0:
        print(f"Remaining: {todo_stack}")
        for fn in inspect.getmembers(module, inspect.isfunction):
            fn_name, fn_obj = fn
            if fn_name in todo_stack:
                todo_stack.remove(fn_name)
                print(f"converting {fn_name}, remaining: {todo_stack}")
                ts_fn = torch.jit.script(fn_obj)
                ts_graph = ts_fn.graph
                ksc_def = ts2ks_fromgraph(False, fn_name, ts_graph, example_inputs)
                ksc_defs.insert(0, ksc_def)

    entry_def = ksc_defs[-1]

    return ksc_defs_to_module(ksc_defs, entry_def)

def ts2mod(function, example_inputs):
    if True:
        fn = torch.jit.script(function)
        ksc_def = ts2ks_fromgraph(False, fn.name, fn.graph, example_inputs)
    else:
        ksc_def = ast2ks(function)

    return ksc_defs_to_module([ksc_def], ksc_def)


import time


class time_sampler:
    def __init__(self, minimizing=False):
        self.minimizing = minimizing
        if self.minimizing:
            self.time = 1e10
        else:
            self.time = 0
            self.ncalls = 0

    def duration(self):
        if self.minimizing:
            return self.time
        else:
            return self.time / self.ncalls

    @property
    def us(self):
        return self.duration() * 1e6

    @staticmethod
    def get_time():
        return time.time_ns() * 1e-9

    def mark(self):
        self.start = time_sampler.get_time()

    def record(self):
        delta = time_sampler.get_time() - self.start
        if self.minimizing:
            self.time = min(delta, self.time)
        else:
            self.time += delta
            self.ncalls += 1


# %%

if __name__ == "__xmain__":
    # %%
    import math
    import torch
    import torch.nn.functional as F

    torch.set_default_dtype(torch.float64)

    do_original = False

    if do_original:

        def lltm_forward_py_orig(input, weights, bias, old_h, old_cell):
            X = torch.cat([old_h, input], dim=1)

            # Compute the input, output and candidate cell gates with one MM.
            gate_weights = F.linear(X, weights, bias)

            # Split the combined gate weight matrix into its components.
            gates = gate_weights.chunk(3, dim=1)

            input_gate = torch.sigmoid(gates[0])
            output_gate = torch.sigmoid(gates[1])
            # Here we use an ELU instead of the usual tanh.
            candidate_cell = F.elu(gates[2])

            # Compute the new cell state.
            new_cell = old_cell + candidate_cell * input_gate
            # Compute the new hidden state and output.
            new_h = torch.tanh(new_cell) * output_gate

            return new_h, new_cell

    else:
        # Simpler model to test zero-runtime implementation
        def lltm_forward_py(input, weights, bias, old_h, old_cell):
            X = torch.cat([old_h, input], dim=1)

            # Compute the input, output and candidate cell gates with one MM.
            gate_weights = F.linear(X, weights, bias)

            input_gate = torch.tanh(gate_weights)
            output_gate = torch.tanh(gate_weights)
            candidate_cell = torch.tanh(gate_weights)

            # Compute the new cell state.
            new_cell = old_cell + candidate_cell * input_gate
            # Compute the new hidden state and output.
            new_h = torch.tanh(new_cell) * output_gate

            return new_h, new_cell

    lltm_forward = lltm_forward_py

    class LLTM(torch.nn.Module):
        def __init__(self, input_features, state_size):
            super(LLTM, self).__init__()
            self.input_features = input_features
            self.state_size = state_size
            if do_original:
                # 3 * state_size for input gate, output gate and candidate cell gate.
                # input_features + state_size because we will multiply with [input, h].
                self.weights = torch.nn.Parameter(torch.empty(3 * state_size, input_features + state_size))
                self.bias = torch.nn.Parameter(torch.empty(3 * state_size))
            else:
                self.weights = torch.nn.Parameter(torch.empty(state_size, input_features + state_size))
                self.bias = torch.nn.Parameter(torch.empty(state_size))

            self.reset_parameters()

        def reset_parameters(self):
            stdv = 1.0 / math.sqrt(self.state_size)
            for weight in self.parameters():
                weight.data.uniform_(-stdv, +stdv)

        def forward(self, input, state):
            return lltm_forward(input, self.weights, self.bias, *state)

    # run it...
    batch_size = 16
    input_features = 32
    state_size = 30

    X = torch.randn(batch_size, input_features)
    h = torch.randn(batch_size, state_size)
    C = torch.randn(batch_size, state_size)

    rnn = LLTM(input_features, state_size)

    def myloss(X, h, c):
        new_h, new_C = rnn(X, (h, C))
        return new_h.sum() + new_C.sum()

    def timeit(msg):
        print("Timing: ", msg, lltm_forward)
        forward = time_sampler()
        backward = time_sampler()
        nruns = 50
        for _ in range(nruns):
            forward.mark()
            loss = myloss(X, h, C)
            forward.record()

            backward.mark()
            loss.backward()
            backward.record()

        print(f"Forward: {forward.us:.3f} us | Backward {backward.us:.3f} us")

    timeit("py")

    lltm_forward_ts = torch.jit.script(lltm_forward_py)
    lltm_forward = lltm_forward_ts
    timeit("ts")
    #%%
    example_inputs = (X, rnn.weights, rnn.bias, h, C)
    fn = torch.jit.script(lltm_forward_py)
    # print(fn.graph)
    #     graph(%input.1 : Tensor,
    #       %weights.1 : Tensor,
    #       %bias.1 : Tensor,
    #       %old_h.1 : Tensor,
    #       %old_cell.1 : Tensor):
    #   %30 : Function = prim::Constant[name="elu"]()
    #   %29 : bool = prim::Constant[value=0]()
    #   %28 : float = prim::Constant[value=1.]()
    #   %13 : Function = prim::Constant[name="linear"]()
    #   %8 : int = prim::Constant[value=1]() # <ipython-input-4-ecbf56b83299>:7:38
    #   %16 : int = prim::Constant[value=3]() # <ipython-input-4-ecbf56b83299>:12:31
    #   %19 : int = prim::Constant[value=0]() # <ipython-input-4-ecbf56b83299>:14:37
    #   %26 : int = prim::Constant[value=2]() # <ipython-input-4-ecbf56b83299>:17:33
    #   %7 : Tensor[] = prim::ListConstruct(%old_h.1, %input.1)
    #   %X.1 : Tensor = aten::cat(%7, %8) # <ipython-input-4-ecbf56b83299>:7:8
    #   %gate_weights.1 : Tensor = prim::CallFunction(%13, %X.1, %weights.1, %bias.1) # <ipython-input-4-ecbf56b83299>:10:19
    #   %gates.1 : Tensor[] = aten::chunk(%gate_weights.1, %16, %8) # <ipython-input-4-ecbf56b83299>:12:12
    #   %20 : Tensor = aten::__getitem__(%gates.1, %19) # <ipython-input-4-ecbf56b83299>:14:31
    #   %input_gate.1 : Tensor = aten::sigmoid(%20) # <ipython-input-4-ecbf56b83299>:14:17
    #   %23 : Tensor = aten::__getitem__(%gates.1, %8) # <ipython-input-4-ecbf56b83299>:15:32
    #   %output_gate.1 : Tensor = aten::sigmoid(%23) # <ipython-input-4-ecbf56b83299>:15:18
    #   %27 : Tensor = aten::__getitem__(%gates.1, %26) # <ipython-input-4-ecbf56b83299>:17:27
    #   %candidate_cell.1 : Tensor = prim::CallFunction(%30, %27, %28, %29) # <ipython-input-4-ecbf56b83299>:17:21
    #   %35 : Tensor = aten::mul(%candidate_cell.1, %input_gate.1) # <ipython-input-4-ecbf56b83299>:20:26
    #   %new_cell.1 : Tensor = aten::add(%old_cell.1, %35, %8) # <ipython-input-4-ecbf56b83299>:20:15
    #   %39 : Tensor = aten::tanh(%new_cell.1) # <ipython-input-4-ecbf56b83299>:22:12
    #   %new_h.1 : Tensor = aten::mul(%39, %output_gate.1) # <ipython-input-4-ecbf56b83299>:22:12
    #   %44 : (Tensor, Tensor) = prim::TupleConstruct(%new_h.1, %new_cell.1)
    #   return (%44)

    ks_fun = ts2mod(lltm_forward_py, example_inputs=example_inputs)

    def torch_from_ks(ks_object):
        if isinstance(ks_object, tuple):
            return tuple(torch_from_ks(ks) for ks in ks_object)

        return torch.from_numpy(numpy.array(ks_object, copy=True))

    class KnossosLLTMFunction(torch.autograd.Function):
        @staticmethod
        def forward(ctx, input, weights, bias, old_h, old_cell):
            args = (input, weights, bias, old_h, old_cell)

            ks_fun._py_mod.reset_allocator()
            ks_args = (ks_fun.torch_to_ks(x) for x in args)

            # Call it
            outputs = ks_fun(*ks_args)

            ctx.save_for_backward(*args)

            return torch_from_ks(outputs)

        @staticmethod
        def backward(ctx, grad_h, grad_cell):
            ks_args = tuple(ks_fun.torch_to_ks(x) for x in ctx.saved_tensors)
            grad_args = (grad_h, grad_cell)
            ks_grad_args = tuple(ks_fun.torch_to_ks(x) for x in grad_args)
            outputs = ks_fun.rev(ks_args, ks_grad_args)

            return torch_from_ks(outputs)

    lltm_forward = KnossosLLTMFunction.apply
    timeit("Knossos")

    #%%
    import torch.utils.cpp_extension

    print("Compiling extension ...", end="")
    lltm_cpp = torch.utils.cpp_extension.load(
        name="lltm_cpp", sources=[utils.get_ksc_dir() + "/src/ts2k/ts2ks/lltm.cpp"]
    )
    print("done.")

    class LLTMFunction(torch.autograd.Function):
        @staticmethod
        def forward(ctx, input, weights, bias, old_h, old_cell):
            outputs = lltm_cpp.forward(input, weights, bias, old_h, old_cell)
            new_h, new_cell = outputs[:2]
            variables = outputs[1:] + [weights]
            ctx.save_for_backward(*variables)

            return new_h, new_cell

        @staticmethod
        def backward(ctx, grad_h, grad_cell):
            outputs = lltm_cpp.backward(grad_h.contiguous(), grad_cell.contiguous(), *ctx.saved_tensors)
            d_old_h, d_input, d_weights, d_bias, d_old_cell = outputs
            return d_input, d_weights, d_bias, d_old_h, d_old_cell

    lltm_forward = LLTMFunction.apply
    timeit("native")


#%%

if __name__ == "__xmain__":
    from math import sin

    torch.set_default_dtype(torch.float64)

    def bar(a: int, x: float):
        M = torch.tensor([[1.1, -x], [x + 2.1, 2.2]])
        v = torch.tensor([2.2, 3.3])

        Mv = torch.matmul(M, v)

        b = torch.dot(Mv, v)

        if a < 0:
            t = -0.125 * x
        else:
            t = 1 / 2 * x * float(b)
        return sin(t) * t

    def foofilter(xs: torch.Tensor):
        t = torch.zeros(xs.shape)
        for n, x in enumerate(xs):
            if x < 0:
                t[n] = -0.125 * x
            else:
                t[n] = 1 / (n + 1) * x ** 2

        return torch.mean(torch.sin(t) * t)

    def foofilter_comp(xs: torch.Tensor):
        t = torch.tensor([(-0.125 * x if x < 0.0 else 1 / (n + 1) * x ** 2).item() for n, x in enumerate(xs)])
        return torch.mean(torch.sin(t) * t)

    def foofilter_mask(x: torch.Tensor):
        mask = x < 0
        t = mask * (-0.125 * x) + (1 - mask) * 1 / 2 * x ** 2
        return torch.mean(torch.sin(t) * t)

    x_example = torch.rand((23,))

    fn = torch.jit.script(foofilter_comp)
    print(fn.code)
    # print(fn(x_example))

    # #AWF: TODO: check "training" attribute -- does that enable faster AD?
    # with open("/tmp/t.onnx", "w") as temp:
    #     torch.onnx.export(model=fn,
    #                   args=x_example,
    #                   example_outputs=fn(x_example),
    #                   f=temp,
    #                   verbose=True)
    print(fn.graph)
    ks_str = ts2ks_fromgraph(False, fn.name, fn.graph, (x_example,))
    cpprint(ks_str)

    ks_fun = ts2mod(foofilter_comp, example_inputs=(x_example,))


if __name__ == "__main__":
    print("\n\n*************************\n\n")

    # "Squared Leaky Relu"?
    def squirrel(x: torch.Tensor):
        y = torch.mean(x)
        if y < 0.0:
            t = -0.125 * x
        else:
            t = 1 / 2 * x ** 2
        return torch.mean(torch.sin(t) * t)

    # Compile function and gradients for example input of ones(2,3)
    x_example = torch.ones((2, 3))

    fn = torch.jit.script(squirrel)
    print(fn.code)
    ks_str = ts2ks_fromgraph(False, fn.name, fn.graph, (x_example,))
    cpprint(ks_str)

    # Compile function and gradients for example input of ones(2,3)
    x_example = torch.ones((2, 3))
    ks_fun = ts2mod(squirrel, example_inputs=(x_example,))

    # Call the function at different, interesting inputs
    x = torch.rand((4, 4))  # TODO: check non-square

    ans = squirrel(x)
    print("Python answer = ", ans.numpy())

    ts_squirrel = torch.jit.script(squirrel)
    print("TorchScript answer = ", ts_squirrel(x).numpy())

    kx = ks_fun.torch_to_ks(x)
    ans = ks_fun(kx)
    print("Knossos answer = ", ans)

    # Compute the gradient
    ans = ks_fun.rev(kx, 1.0)
    ansnp = numpy.array(ans, copy=False)
    print("Knossos gradient = \n", ansnp)

    # Compute the gradient using torch
    xtrace = x.clone().detach().requires_grad_(True)
    y = squirrel(xtrace)
    dy = torch.autograd.grad(y, xtrace)
    print("Torch gradient = \n", dy[0].numpy())

    print("Gradient diff = \n", ansnp - dy[0].numpy())

    # print(f"Knossos mem: {ks_fun._py_mod.allocator_top()}/{ks_fun._py_mod.allocator_peak()}")
    import timeit

    def time_ks(n):
        x = torch.rand((n, n))
        ks_fun._py_mod.reset_allocator()
        kx = ks_fun.torch_to_ks(x)
        ans = ks_fun.rev(kx, 1.0)
        # print(numpy.array(ans, copy=False))

    def time_pytorch(n):
        x = torch.rand((n, n))
        x.requires_grad_(True)
        y = ts_squirrel(x)
        dy = torch.autograd.grad(y, x)
        # print(dy)

    size = 4
    ntimes = 10000
    print("time_ks= ", timeit.timeit(lambda: time_ks(size), number=ntimes))
    print("time_pt= ", timeit.timeit(lambda: time_pytorch(size), number=ntimes))

    # Next:
    #  - foofilter


# %%
