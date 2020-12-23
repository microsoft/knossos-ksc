from typing import List, Tuple

import functools
import torch

from ksc import utils
from ksc.ks_function import KscFunction

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

Type_Tensor = Type.Vec(Type.Vec(Type.Float))  # float vs integer? also determine rank instead of hardcode
symbolLook = {
    "Tensor": Type_Tensor,  # float vs integer? also determine rank instead of hardcode
    "Optional[Tensor]": Type_Tensor,   # Just say optionals are required for now. TODO: don't do this!
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
        Type.Vec(Type_Tensor),
        Type.Vec(Type_Tensor)
    )

#

def mangleDebugName(name):
    return "_" + name.replace(".", "_")

def from_torch_type(t):
    if t == torch._C.IntType.get():
        return Type.Integer

    if t == torch._C.FloatType.get():
        return Type.Float

    type_lookup = str(t)

    if "Optional" in type_lookup:
        print("WARNING: Optional argument treated as required:" + type_lookup)

    return symbolLook[type_lookup]

def make_arg(input):
    input_type = from_torch_type(input.type())
    name = mangleDebugName(input.debugName())
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


def make_print(node):
    mangled_id = mangleDebugName(node.inputsAt(0).debugName())
    return Var("print"), Var(mangled_id)


def make_list(node):
    value = node.outputsAt(0)
    return Var(mangleDebugName(value.debugName())), Call("Vec_init", [Var(mangleDebugName(i.debugName())) for i in node.inputs()])


def make_tensor(node):
    # tensors aren't explicitly modelled in Knossos yet, leave them as identity over a (jagged) list for now
    value = node.outputsAt(0)

    return Var(mangleDebugName(value.debugName())), Var(mangleDebugName(node.inputsAt(0).debugName()))


def make_aten_function(node, value, function_name):
    return Var(mangleDebugName(value.debugName())), Call(function_name, [Var(mangleDebugName(i.debugName())) for i in node.inputs()])


def make_return(node):
    mangled_id = mangleDebugName(node.inputsAt(0).debugName())
    return Var(mangled_id)


def make_callfunction(node):
    value = node.outputsAt(0)
    function_name_constant = node.inputsAt(0).node()
    function_name = function_name_constant.s("name")
    input1 = node.inputsAt(
        1
    )  # TODO: get all inputs instead of just first, 0th is function name itself
    assert len(list(node.inputs())) == 2
    return Var(mangleDebugName(value.debugName())), Call(function_name, mangleDebugName(input1.debugName()))

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

    conditional = mangleDebugName(node.inputsAt(0).debugName())

    blocks = list(
        node.blocks()
    )  # TODO: check length exactly 2, only supporting if/else currently. This is enough for BERT example

    success_branch = make_branch(blocks[0])
    failure_branch = make_branch(blocks[1])

    return Var(identifier), If(Var(conditional), 
                               success_branch,
                               failure_branch)

def ts2ks_fromgraph(generate_edefs, name, graph):

    def translate_node(make_binds, node) -> Tuple[Var, Expr]:
        lookups = {
            "prim::Constant": make_constant,
            "prim::Print": make_print,
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
        make_arg(input)
        for input in graph.inputs()
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
        return_type = from_torch_type(return_node.inputsAt(0).type())

    print(binds)
    body = make_lets(binds, op)

    whole_exp = Def(name, return_type, args, body)

    return pformat(whole_exp)


# TODO: make an configuration named tuple rather than passing flags
def ts2ks(output, generate_edefs, function):
    s = ts2ks_fromgraph(generate_edefs, function.name, function.graph)
    output.write(s)

def ts2mod(function, arg_types, return_type):
    fn = torch.jit.script(function)
    ks_str = ts2ks_fromgraph(False, fn.name, fn.graph)
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

    fn = torch.jit.script(bar)
    ks_str = ts2ks_fromgraph(False, fn.name, fn.graph)
    print(pformat(ks_str))

    ks_bar = ts2mod(bar)

    a,b = 1,12.34

    ans = bar(a,b)
    print(ans)

    ans = ks_bar(a,b)
    print(ans)
