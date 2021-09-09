from typing import Callable, List, Tuple, Optional, Union, Set, Dict
from types import ModuleType
from dataclasses import dataclass, field, replace
from contextlib import contextmanager

import functools
import itertools
import inspect
import torch
import torch.onnx

torch.set_default_dtype(torch.float32)

from ksc import utils
from ksc.parse_ks import parse_ks_filename
from ksc.compile import (
    build_module_using_pytorch_from_ks,
    build_module_using_pytorch_from_cpp,
    default_cflags,
    VecSpec,
    VecSpec_None,
    VecSpec_Elementwise,
    VecSpec_VMap,
)

from ksc.type import Type
from ksc.expr import (
    Expr,
    Def,
    EDef,
    GDef,
    Rule,
    Const,
    Var,
    Lam,
    Call,
    Let,
    If,
    Assert,
    make_structured_name,
)
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


def type_from_value(x):
    if isinstance(x, torch.Tensor):
        return Type.Tensor(len(x.size()), from_torch_dtype(x.dtype))

    return Type.fromValue(x)


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


def mangled_name(node):
    return "_" + utils.encode_name(node.debugName())


def var_or_constant(node):
    val = node.toIValue()
    if val is None:
        return Var(mangled_name(node))
    else:
        return Const(val)


class TorchScriptVisitor:
    functions_todo: Set[Union[str, Callable]]
    functions_done: Set[Union[str, Callable]]

    def __init__(self):
        self.functions_todo = set()
        self.functions_done = set()
        pass

    def mark_function_as_needed(self, f):
        print(f"TorchScriptVisitor: Adding {f} to todo {self.functions_todo}")
        if f not in self.functions_done:
            self.functions_todo.add(f)

    def mark_function_as_done(self, f):
        print(f"TorchScriptVisitor: Marking {f} as done")
        if f in self.functions_todo:
            self.functions_todo.remove(f)
        self.functions_done.add(f)

    def make_arg(self, input, example_input):
        input_type = from_torch_type(input.type())
        example_input_type = type_from_value(example_input)
        if example_input_type.kind != input_type.kind:
            print(
                f"TorchScriptVisitor: example input type {example_input_type} differs from TorchScript input type {input_type}"
            )
        else:
            # input_type will have wrong shape, grab example_input_type
            # TODO: shape propagation, or full abstract interpretation
            input_type = example_input_type

        name = mangled_name(input)
        return Var(name, input_type)

    def make_constant(self, node):

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

    def make_list(self, node):
        value = node.outputsAt(0)
        return (
            Var(mangled_name(value)),
            Call("Vec_init", [var_or_constant(i) for i in node.inputs()]),
        )

    def make_tuple(self, node):
        value = node.outputsAt(0)
        return (
            Var(mangled_name(value)),
            Call("tuple", [var_or_constant(i) for i in node.inputs()]),
        )

    def make_tensor(self, node):
        # tensors aren't explicitly modelled in Knossos yet, leave them as identity over a (jagged) list for now
        value = node.outputsAt(0)
        return Var(mangled_name(value)), var_or_constant(node.inputsAt(0))

    def make_aten_function(self, node, value, function_name):
        return (
            Var(mangled_name(value)),
            Call(function_name, [var_or_constant(i) for i in node.inputs()]),
        )

    def make_return(self, node):
        mangled_id = mangled_name(node.inputsAt(0))
        return Var(mangled_id)

    def make_callfunction(self, node):
        value = node.outputsAt(0)
        assert (
            len(list(node.outputs())) == 1
        )  # Assemble into tuple for multiple outputs
        function_name_constant = node.inputsAt(0).node()
        function_name = function_name_constant.s("name")
        self.mark_function_as_needed(function_name)
        return (
            Var(mangled_name(value)),
            Call(function_name, [var_or_constant(i) for i in tail(node.inputs())]),
        )

    def make_PythonOp(self, node):
        pyname = node.pyname()
        raise NotImplementedError(f"PythonOp {pyname}")

    def make_lets(self, bindings, body) -> Expr:
        for (v, rhs) in reversed(bindings):
            body = Let(v, rhs, body)
        return body

    def make_loop(self, node):
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
        assert self.make_constant(iter_condition.node())[1] == Const(True)
        assert len(a_nodes) == 0
        assert len(b_nodes) == 0
        assert len(y_nodes) == 0

        body_nodes = list(block0.nodes())
        last = body_nodes[-1]
        if last.kind() == "aten::append":
            # Special case for list comprehensions
            l, item = last.inputs()
            assert l.node().kind() == "prim::ListConstruct"

            binds = self.make_binds(body_nodes[:-1])
            lam_body = self.make_lets(binds, var_or_constant(item))

            lam = Lam(Var(mangled_name(i), Type.Integer), lam_body)

            return (
                var_or_constant(l),
                Call("build", [var_or_constant(max_trip_count), lam]),
            )

        print(node)
        assert False

    def make_if(self, node):
        def make_branch(block):
            binds = self.make_binds(block.nodes())
            body = self.make_return(block.returnNode())
            return self.make_lets(binds, body)

        identifier = None
        if node.outputsSize() == 0:
            identifier = "_" + "dummy"  # we're likely to need to make them unique
        else:
            identifier = "_" + node.outputsAt(0).debugName()

        inputs_size = sum(1 for _ in node.inputs())

        if inputs_size != 1:
            raise Exception(
                "Only support conditionals with 1 input, this one had: "
                + str(inputs_size)
            )

        conditional = mangled_name(node.inputsAt(0))

        blocks = list(
            node.blocks()
        )  # TODO: check length exactly 2, only supporting if/else currently. This is enough for BERT example

        success_branch = make_branch(blocks[0])
        failure_branch = make_branch(blocks[1])

        return Var(identifier), If(Var(conditional), success_branch, failure_branch)

    def translate_node(self, node) -> Tuple[Var, Expr]:
        lookups: typing.Dict[str, Callable] = {
            "prim::Constant": self.make_constant,
            "prim::ListConstruct": self.make_list,
            "prim::TupleConstruct": self.make_tuple,
            "prim::Return": self.make_return,
            "prim::CallFunction": self.make_callfunction,
            "prim::PythonOp": self.make_PythonOp,
            "prim::Loop": functools.partial(self.make_loop),
            "prim::If": functools.partial(self.make_if),
        }

        kind = node.kind()
        if kind in lookups:
            return lookups[kind](node=node)

        primfuns = ["prim::min"]
        if kind.startswith("aten::") or kind in primfuns:
            return self.make_aten_function(node, node.outputsAt(0), kind)

        print("WARNING, unimplmented node kind: " + node.kind())
        return Var("ERR"), Var(node.kind())

    def make_binds(self, nodes) -> List[Tuple[Var, Expr]]:
        return [self.translate_node(node) for node in nodes]

    def generate_def(self, name, graph, example_inputs):

        all_nodes = list(graph.nodes())

        binds = self.make_binds(all_nodes)

        args = [
            self.make_arg(input, example)
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
            op = self.translate_node(all_nodes[-1])
            return_type = Type.Integer
        else:
            if print_count > 0:
                print(
                    "WARNING: print statement currently only supported as final operation"
                )
            return_node = graph.return_node()
            op = self.translate_node(return_node)
            return_type = None  # Infer return type in type propagation from_torch_type(return_node.inputsAt(0).type())

        body = self.make_lets(binds, op)

        return Def(StructuredName(name), return_type, args, body)

    def generate_defs_recursive(self, module, function_obj, example_inputs):
        self.mark_function_as_needed(function_obj)

        module_fns = dict(inspect.getmembers(module))

        def ksc_defs():
            while self.functions_todo:
                print(f"TorchScriptVisitor: Remaining: {self.functions_todo}")
                todo = next(iter(self.functions_todo))
                if isinstance(todo, str):
                    # String function name, try to find it in the caller's module
                    module_fn_obj = module_fns.get(todo)
                    if module_fn_obj is not None:
                        print(
                            f"TorchScriptVisitor: converting {todo}, remaining: {self.functions_todo}"
                        )
                        if isinstance(module_fn_obj, KscStub):
                            todo_fn = module_fn_obj.raw_f
                        else:
                            todo_fn = module_fn_obj
                    else:
                        raise ValueError(f"Did not find string-named function {todo}")
                else:
                    todo_fn = todo

                self.mark_function_as_done(todo)

                ts_fn = torch.jit.script(todo_fn)
                ts_graph = ts_fn.graph
                ksc_def = self.generate_def(todo_fn.__name__, ts_graph, example_inputs)
                yield ksc_def

        return list(ksc_defs())[::-1]


def make_tuple_if_many_args(*args):
    t = tuple(*args)
    if len(t) == 1:
        return t[0]
    else:
        return t


# TODO: make an configuration named tuple rather than passing flags
def ts2ks(output, generate_edefs, function, example_inputs):
    visitor = TorchScriptVisitor()
    s = visitor.generate_def(function.name, function.graph, example_inputs)
    output.write(pformat(s))


def torch_from_ks(ks_object):
    if isinstance(ks_object, tuple):
        return tuple(torch_from_ks(ks) for ks in ks_object)

    if isinstance(ks_object, float):
        return torch.tensor(ks_object)  # TODO: use torch::Scalar?

    assert isinstance(ks_object, torch.Tensor)  # TODO: strings, etc.

    return ks_object


def torch_to_ks(val):
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

        return val.contiguous()  # Get data, or copy if not already contiguous

    raise NotImplementedError(val)


# Methods for the KscAutogradFunction class -- a new class will be made for each loaded module
# See https://pytorch.org/docs/stable/notes/extending.html
def forward_template(py_mod, ctx, *args):
    py_mod.reset_allocator()
    ks_args = (torch_to_ks(x) for x in args)

    # Call it
    outputs = py_mod.entry(*ks_args)

    if ctx is not None:
        ctx.torch_vals = ks_args
        ctx.save_for_backward(*args)

    return torch_from_ks(outputs)


def backward_template(py_mod, ctx, *args):
    ks_args = make_tuple_if_many_args(torch_to_ks(x) for x in ctx.saved_tensors)
    ks_grad_args = make_tuple_if_many_args(torch_to_ks(x) for x in args)
    outputs = py_mod.entry_vjp(ks_args, ks_grad_args)
    return torch_from_ks(outputs)


class KscAutogradFunction(torch.autograd.Function):
    """
    An instance of torch.autograd.Function representing a Knossos compilation.
    """

    pass


def make_KscAutogradFunction(py_mod) -> KscAutogradFunction:
    # We need to make a new class for every py_mod, as PyTorch requires forward and backward to be
    # staticmethods.  This is not too expensive, as each mod needs to be compiled anyway.
    forward = lambda ctx, args: forward_template(py_mod, ctx, args)
    backward = lambda ctx, args: backward_template(py_mod, ctx, args)
    return type(
        "KscAutogradFunction_" + py_mod.__name__,
        (KscAutogradFunction,),
        {
            "py_mod": py_mod,
            "forward": staticmethod(forward),
            "backward": staticmethod(backward),
            "adapt": staticmethod(lambda x: torch_to_ks(x)),
        },
    )  # type: ignore


def ksc_defs_to_module(
    ksc_defs, entry_def, torch_extension_name, vectorization, generate_lm, gpu=False
):
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
        if generate_lm:
            defs_with_derivatives += [
                GDef("rev", ksc_def.name),
            ]
        else:
            defs_with_derivatives += [
                GDef("suffwdpass", ksc_def.name),
                GDef("sufrevpass", ksc_def.name),
                GDef("sufrev", ksc_def.name),
            ]

    ks_str = "\n".join(map(pformat, defs_with_derivatives))

    return ksc_string_to_module(
        ks_str,
        entry_def.name,
        torch_extension_name,
        vectorization,
        generate_lm,
        extra_cflags=default_cflags,
        gpu=gpu,
    )


def ksc_string_to_module(
    ks_str,
    entry_sn,
    torch_extension_name,
    vectorization,
    generate_lm,
    extra_cflags,
    gpu=False,
):
    der = "rev" if generate_lm else "sufrev"
    bindings_to_generate = [
        ("entry", entry_sn),
        ("entry_vjp", StructuredName((der, entry_sn))),
    ]
    return build_module_using_pytorch_from_ks(
        ks_str,
        bindings_to_generate,
        torch_extension_name,
        vectorization=vectorization,
        use_aten=True,
        extra_cflags=extra_cflags,
        gpu=gpu,
    )


def cpp_string_to_module(
    cpp_str, torch_extension_name, entry_name, entry_vjp_name, extra_cflags
):
    bindings_to_generate = [
        ("entry", entry_name),
        ("entry_vjp", entry_vjp_name),
    ]
    return build_module_using_pytorch_from_cpp(
        cpp_str, bindings_to_generate, torch_extension_name, extra_cflags=extra_cflags,
    )


def ksc_defs_to_autograd_function(
    ksc_defs,
    entry_def,
    torch_extension_name,
    vectorization=False,
    generate_lm=True,
    gpu=False,
) -> KscAutogradFunction:
    mod = ksc_defs_to_module(
        ksc_defs,
        entry_def,
        torch_extension_name,
        vectorization=vectorization,
        generate_lm=generate_lm,
        gpu=gpu,
    )
    return make_KscAutogradFunction(mod)


def ksc_string_to_autograd_function(
    ks_str,
    entry_sn,
    torch_extension_name,
    generate_lm=True,
    extra_cflags=default_cflags,
) -> KscAutogradFunction:
    mod = ksc_string_to_module(
        ks_str,
        entry_sn,
        torch_extension_name,
        vectorization=VecSpec_None(),
        generate_lm=generate_lm,
        extra_cflags=extra_cflags,
    )
    return make_KscAutogradFunction(mod)


def cpp_string_to_autograd_function(
    cpp_str,
    torch_extension_name,
    entry_name="entry",
    entry_vjp_name="entry_vjp",
    extra_cflags=default_cflags,
) -> KscAutogradFunction:
    mod = cpp_string_to_module(
        cpp_str, torch_extension_name, entry_name, entry_vjp_name, extra_cflags
    )
    return make_KscAutogradFunction(mod)


def _tsmod2ksmod(
    module,
    function_obj,
    torch_extension_name,
    example_inputs,
    generate_lm=True,
    vectorization: VecSpec = VecSpec_None(),
    gpu=False,
) -> KscAutogradFunction:
    assert isinstance(example_inputs, tuple)

    # Transform example inputs to match vectorization
    if isinstance(vectorization, VecSpec_VMap):
        assert len(example_inputs) == 1, "not implemented"
        example_inputs = (example_inputs[0][0],)
    elif isinstance(vectorization, VecSpec_Elementwise):
        assert len(example_inputs) == 1, "not implemented"
        example_inputs = (1.1,)
    else:
        assert isinstance(vectorization, VecSpec_None)

    visitor = TorchScriptVisitor()
    ksc_defs = visitor.generate_defs_recursive(module, function_obj, example_inputs)

    entry_def = ksc_defs[-1]
    return ksc_defs_to_autograd_function(
        ksc_defs,
        entry_def,
        torch_extension_name,
        vectorization=vectorization,
        generate_lm=generate_lm,
        gpu=gpu,
    )


@dataclass(frozen=True)
class CompileConfiguration:
    gpu: bool = False


class Lambda(torch.nn.Module):
    """
    Input: A Function
    Returns : A Module that can be used
        inside nn.Sequential
    """

    def __init__(self, func):
        super().__init__()
        self.func = func

    def forward(self, x):
        return self.func(x)


@dataclass
class KscStub:
    raw_f: Callable
    module: ModuleType
    generate_lm: bool
    vectorization: VecSpec
    compiled: Dict[CompileConfiguration, KscAutogradFunction] = field(
        default_factory=dict
    )

    def __call__(self, *args):
        """
        Call with pytorch tensors.
        This calls the KscAutoGradFunction apply method, so is suitable 
        for use in the "forward/backward" pattern for gradient computation. 
        """
        return self.ensure_compiled(args).apply(*args)

    def nnModule(self, example_input):
        """
        Return a torch.nn.Module
        """
        autogradfunction = self.ensure_compiled(example_input)
        return Lambda(autogradfunction.apply)

    def autogradFunction(self, *args):
        """
        Return a torch.autograd.Function
        """
        return self.ensure_compiled(args)

    def _reset_allocator(self, *args):
        self.ensure_compiled(args).py_mod.reset_allocator()

    def _entry(self, *args):
        """
        Directly call the Knossos compiled function.
        Does not wrap torch tensors, or reset memory allocator.
        For test use only
        """
        return self.ensure_compiled(args).py_mod.entry(*args)

    def _entry_vjp(self, *args):
        """
        Directly call the Knossos vjp function.
        Does not wrap torch tensors, or reset memory allocator.
        For test use only
        """
        configuration = CompileConfiguration(gpu=_input_is_gpu(args))
        assert configuration in self.compiled  # TODO: infer call args from vjp args
        return self.compiled[configuration].py_mod.entry_vjp(*args)

    def vjp(self, x, df):
        """
        Compute vector-Jacobian product
        ``` 
           vjp(x,df) = J(x)^T * df
        ```
        """
        f = self.ensure_compiled(x)
        x_ks = torch_to_ks(x)
        df_ks = torch_to_ks(df)
        f.py_mod.reset_allocator()
        out_ks = f.py_mod.entry_vjp(x, df)
        out = torch_from_ks(out_ks)
        f.py_mod.reset_allocator()
        return out

    def compile(
        self, example_inputs, torch_extension_name, configuration=CompileConfiguration()
    ) -> KscAutogradFunction:
        self.compiled[configuration] = _tsmod2ksmod(
            self.module,
            self.raw_f,
            torch_extension_name=torch_extension_name,
            example_inputs=example_inputs,
            generate_lm=self.generate_lm,
            vectorization=self.vectorization,
            gpu=configuration.gpu,
        )

        return self.compiled[configuration]

    def ensure_compiled(self, example_inputs) -> KscAutogradFunction:
        configuration = CompileConfiguration(gpu=_input_is_gpu(example_inputs))
        compiled = self.compiled.get(configuration)
        if compiled is not None:
            return compiled
        print(f"knossos.register: Compiling {self.raw_f.__name__}")
        torch_extension_name = (
            "KscStub_"
            + ("CUDA_" if configuration.gpu else "")
            + str(self.vectorization)
            + "_"
            + ("lm_" if self.generate_lm else "")
            + self.module.__name__
            + "_"
            + self.raw_f.__name__
        )
        if self.generate_lm:
            torch_extension_name += "__generate_lm"
        return self.compile(example_inputs, torch_extension_name, configuration)


def _input_is_gpu(example_inputs):
    tensors = (x for x in example_inputs if isinstance(x, torch.Tensor))
    first_tensor = next(tensors, None)
    if first_tensor is None:
        return False
    gpu = first_tensor.is_cuda
    if not all(t.is_cuda == gpu for t in tensors):
        raise ValueError("Inputs contain a mixture of CUDA and non-CUDA tensors")
    return gpu


def _Vectorization_from_flags(elementwise, vmap):
    assert not (elementwise and vmap)
    if elementwise:
        return VecSpec_Elementwise()
    if vmap:
        return VecSpec_VMap()
    return VecSpec_None()


def _register_core(
    f: Callable, module: ModuleType, generate_lm=False, vectorization=VecSpec_None()
) -> KscStub:

    if isinstance(f, KscStub):
        # TODO: this should just add to the list of configurations required of the function
        # Copy the existing KscStub, setting new flags, and force recompilation
        return replace(
            f, generate_lm=generate_lm, vectorization=vectorization, compiled=dict(),
        )
    else:
        # Create a ksc stub
        return KscStub(
            raw_f=f,
            module=module,
            generate_lm=generate_lm,
            vectorization=vectorization,
            compiled=dict(),
        )


# TODO: In Python 3.9, optional-argument decorators will be much simpler.
# https://docs.python.org/3.9/reference/compound_stmts.html#function
def register_direct(func: Callable, generate_lm=False, elementwise=False, vmap=False):
    frame = inspect.currentframe()
    assert frame
    module = inspect.getmodule(frame.f_back)
    assert module
    vectorization = _Vectorization_from_flags(elementwise, vmap)
    return _register_core(
        func, module, generate_lm=generate_lm, vectorization=vectorization,
    )


def optional_arg_decorator(register):
    # https://stackoverflow.com/a/20966822
    def wrapped_decorator(*args, **kwargs):
        # Grab the caller's module here, as wrapped_decorator may be 1 or 2 deeper
        module = inspect.getmodule(inspect.currentframe().f_back)

        if len(args) == 1 and len(kwargs) == 0 and callable(args[0]):
            return register(args[0], module)

        # we have optional args
        def real_decorator(f):
            return register(f, module, *args, **kwargs)

        return real_decorator

    return wrapped_decorator


@optional_arg_decorator
def register(func: Callable, module: ModuleType, generate_lm=False) -> KscStub:
    """
    Main Knossos entry point.

    The @register decorator transforms a TorchScript function into a
    KscAutogradFunction which implements the function and its 
    derivatives.
    ```
       @knossos.register
       def foo(x : torch.Tensor) -> torch.Tensor:
           return x * sin(x)
    ```
    Endows `foo` with the following behaviours
    ```
        y = foo(x)        # Fast (C++/CUDA/...) computation of f(x)
        vjp(foo, x, dy)   # Fast computation of dot(dy, [df_i/dx_j])
    ```
    The implementation delays compilation until the first call, or 
    when "foo.compile()" is explicitly called.
    """
    return _register_core(func, module, generate_lm=generate_lm,)


@optional_arg_decorator
def vmap(func: Union[Callable, KscStub], module: ModuleType, generate_lm=False):
    """
    Knossos entry point for vmap.
    ```
       @knossos.vmap
       def foo(x : Tensor) -> Tensor:
           return x * sin(x)
    ```
    Where the expected input is a Tensor of rank N, and returns a Tensor of rank M
    Transforms `foo` to have signature Tensor [1+N] -> Tensor [1+M]
    TODO: handle multiple args and return types,
    e.g. transform (Tensor [2], Tensor [N], float) -> (float, Tensor [M])
                to (Tensor [3], Tensor [1+N], Tensor [1]) -> (Tensor [1], Tensor [1+M])
    ```
    The implementation delays compilation until the first call, or
    when "f.compile()" is explicitly called.
    """
    return _register_core(
        func, module, generate_lm=generate_lm, vectorization=VecSpec_VMap(),
    )


@optional_arg_decorator
def elementwise(
    func: Union[Callable, KscStub],
    module: ModuleType,
    generate_lm=False,
    example_element=1.23,
):
    """
    Knossos entry point for elementwise.
    ```
       @knossos.elementwise
       def foo(x : float) -> float:
           return x * sin(x)
    ```
    Transforms `foo` to have signature Tensor -> Tensor, operating elementwise

    For more complex functions, operating on other than float, the `example_element`
    parameter  
    ```
       @knossos.elementwise(example_element = torch.randn(3,4))
       def foo(x : Tensor) -> Tensor:
           return dot(x, sin(x)) * x
    ```
    Transforms `foo` to take tensors of shape (PxQx...xMxN), mapping over the last two
    dimensions.  In this case, the precise sizes of the tensor are not used

    The implementation delays compilation until the first call, or
    when "f.compile()" is explicitly called.
    """
    return _register_core(
        func,
        module,
        generate_lm=generate_lm,
        vectorization=VecSpec_Elementwise(example_element),
    )
