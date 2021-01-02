from typing import List, Tuple

import functools
import torch

from ksc import utils
from ksc.ks_function import KscFunction
from ksc.parse_ks import parse_ks_filename

from ksc.type import Type
from ksc.expr import Expr, Def, EDef, Rule, Const, Var, Lam, Call, Let, If, Assert

from ksc.type_propagate import type_propagate_decls

# Importing prettyprint to get the decorated printers for Expression and Type
import ksc.prettyprint # pylint: disable=unused-import

# Import the prettyprinter routines we use explicitly in this file
from prettyprinter import cpprint, pformat

# Needed this in order to see the error messages when pprint fails
import warnings
warnings.filterwarnings("always")

# Background reading
# https://github.com/pytorch/pytorch/blob/master/torch/csrc/jit/OVERVIEW.md
# https://pytorch.org/docs/master/jit.html#interpreting-graphs
# https://github.com/pytorch/pytorch/blob/8fe2a5e91b79e3f9b6f6c632fdf7f39ec3bf4fca/torch/csrc/jit/ir/ir.h

# Newline constant for s-expr printing
nl = "\n"
tab = "\t"

# CallMethod resolution:
# https://github.com/pytorch/pytorch/blob/b6bb644e41b3928b5a515330ad35c8b447fcb876/torch/csrc/jit/serialization/python_print.cpp#L984-L1004

Type_Tensor = Type.Tensor(-1, Type.Float)  # float vs integer? also determine rank instead of hardcode
symbolLook = {
    "Optional[bool]": Type.Bool,  # Just say optionals are required for now. TODO: don't do this!
}

# We're going to have to break out the data structure at some point, for now, hardcode
# No recursive literals
symbolLook["Tuple[int, Tensor]"] = Type.Tuple(Type.Integer, Type_Tensor)

# hardcoding BertScriptableForQuestionAnswering TODO: getting urgent now to break up the return type and transform
symbolLook[
    "Tuple[Optional[Tensor], Tensor, Tensor, Optional[List[Tensor]], Optional[List[Tensor]]]"
] = Type.Tuple(
        Type_Tensor,
        Type_Tensor,
        Type_Tensor,
        Type.Tensor(1, Type_Tensor),
        Type.Tensor(1, Type_Tensor)
    )

#

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

    return symbolLook[type_lookup]

def type_from_value(x):
    if isinstance(x, torch.Tensor):
        return Type.Tensor(len(x.size()), from_torch_dtype(x.dtype))

    return Type.fromValue(x)

def make_arg(input, example_input):
    input_type = type_from_value(example_input)
    input_type_2 = from_torch_type(input.type())
    assert input_type.kind == input_type_2.kind
    name = mangled_name(input)
    return Var(name, input_type, decl=True)

def make_constant(node):

    value = node.outputsAt(0)

    try:
        possibleLiteral = value.toIValue()
        if possibleLiteral is None:
            literal = 0.0
        elif possibleLiteral == 0:
            literal = 0.0
        else:
            literal = possibleLiteral
    except RuntimeError:
        literal = "FUNCTIONCALL"

    return Var("_" + value.debugName()), Const(literal)


def make_list(node):
    value = node.outputsAt(0)
    return Var(mangled_name(value)), Call("Vec_init", [Var(mangled_name(i)) for i in node.inputs()])


def make_tensor(node):
    # tensors aren't explicitly modelled in Knossos yet, leave them as identity over a (jagged) list for now
    value = node.outputsAt(0)

    return Var(mangled_name(value)), Var(mangled_name(node.inputsAt(0)))


def make_aten_function(node, value, function_name):
    return Var(mangled_name(value)), Call(function_name, [Var(mangled_name(i)) for i in node.inputs()])


def make_return(node):
    mangled_id = mangled_name(node.inputsAt(0))
    return Var(mangled_id)


def make_callfunction(node):
    value = node.outputsAt(0)
    assert len(list(node.outputs())) == 1 # Assemble into tuple for multiple outputs
    function_name_constant = node.inputsAt(0).node()
    function_name = function_name_constant.s("name")
    return (Var(mangled_name(value)), 
            Call(function_name, [Var(mangled_name(i)) for i in node.inputs()]))

def make_lets(bindings, body) -> Expr:
    for (v,rhs) in reversed(bindings):
        body = Let(v, rhs, body)
    return body

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

    return Var(identifier), If(Var(conditional), 
                               success_branch,
                               failure_branch)

def ts2ks_fromgraph(generate_edefs, name, graph, example_inputs):

    def translate_node(make_binds, node) -> Tuple[Var, Expr]:
        lookups = {
            "prim::Constant": make_constant,
            "prim::ListConstruct": make_list,
            "prim::Return": make_return,
            "prim::CallFunction": make_callfunction,
            "prim::If": functools.partial(make_if, make_binds=make_binds),
        }

        kind = node.kind()
        if kind in lookups:
            return lookups[kind](node=node)

        if kind.startswith("aten::"):
            return make_aten_function(node, node.outputsAt(0), kind)

        print("WARNING, unimplmented node kind: " + node.kind())
        return Var("ERR"), Var(node.kind())


    def make_binds(nodes) -> List[Tuple[Var, Expr]]:
        return [
            translate_node(make_binds, node)
            for node in nodes
            if node.kind() != "prim::Print"
        ]

    all_nodes = list(graph.nodes())

    binds = make_binds(all_nodes)

    args = [
        make_arg(input, example)
        for input,example in zip(graph.inputs(), example_inputs)
        if (not input.type().str().startswith("__torch__.transformers"))
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
        return_type = None #Infer return type in type propagation from_torch_type(return_node.inputsAt(0).type())

    body = make_lets(binds, op)

    return Def(name, return_type, args, body)


# TODO: make an configuration named tuple rather than passing flags
def ts2ks(output, generate_edefs, function, example_inputs):
    s = ts2ks_fromgraph(generate_edefs, function.name, function.graph, example_inputs)
    output.write(pformat(s))

def ts2mod(function, example_inputs):
    fn = torch.jit.script(function)
    ksc_def = ts2ks_fromgraph(False, fn.name, fn.graph, example_inputs)

    if True:
        symtab = dict()
        decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
        type_propagate_decls(decls_prelude, symtab)
        decls_prelude_aten = list(parse_ks_filename("src/runtime/prelude-aten.ks"))
        type_propagate_decls(decls_prelude_aten, symtab)

        type_propagate_decls([ksc_def], symtab)

    ks_str = pformat(ksc_def)
    arg_types = [arg.type_ for arg in ksc_def.args]
    return_type = ksc_def.return_type
    mod = utils.generate_and_compile_cpp_from_ks(ks_str, fn.name, arg_types, return_type=return_type, generate_derivatives=True)

    return KscFunction(mod)


if __name__ == "__main__":
    from math import sin
    torch.set_default_dtype(torch.float64)
    
    def bar(a : int, x : float):
        M = torch.tensor([[1.1, -x], [x+2.1, 2.2]])
        v = torch.tensor([2.2,3.3])

        Mv = torch.matmul(M, v)

        b = torch.dot(Mv, v)

        if a < 0:
            t = -0.125*x
        else:
            t = 1/2 * x * float(b)
        return sin(t) * t

    def foo(x : torch.Tensor):
        y = torch.mean(x)
        if y < 0:
            t = -0.125*x
        else:
            t = 1/2 * x ** 2
        return torch.mean(torch.sin(t)*t)

    x = torch.ones((2,3))

    fn = torch.jit.script(foo)
    print(fn.code)
    ks_str = ts2ks_fromgraph(False, fn.name, fn.graph, (x,))
    print(pformat(ks_str))

    
    ks_fun = ts2mod(foo, example_inputs=(x,))

    a,b = 1,12.34

    ans = bar(a,b)
    print(ans)

    ans = ks_fun(a,b)
    print(ans)

    btrace = torch.tensor(b, requires_grad=true)
    y = bar(btrace)
    dy = torch.autograd.grad(y, x)
    print(dy)

    ans = ks_fun.rev((a,b), 1.0)
    print(ans)
