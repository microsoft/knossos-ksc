import torch
import sexpdata
import os
import importlib.util
import sys
import inspect
import argparse

parser = argparse.ArgumentParser(description='Convert TorchScript to Knossos IR')
parser.add_argument('--input_file', required=True, help='TorchScript [file_path].py')
parser.add_argument('--output_file', required=True, help='Knossos IR [file_path].ks')
args = parser.parse_args()

filename = args.output_file

os.makedirs(os.path.dirname(filename), exist_ok=True)
output = open(filename, "w")

# Newline constant for s-expr printing
nl = "\n"
tab = "\t"

input_file_path = args.input_file
module_name = "DynamicLoadedModule"

spec = importlib.util.spec_from_file_location(module_name, input_file_path)
dynamicModule = importlib.util.module_from_spec(spec)
# We deliberately don't make visible via sys.modules[module_name] = module
spec.loader.exec_module(dynamicModule)


# load all TorchScript methods in target modules
# (various limitations, local sibling modules)
dynamicMembers = inspect.getmembers(dynamicModule, predicate=lambda v : type(v) == torch.jit.ScriptFunction)

# https://github.com/pytorch/pytorch/blob/master/torch/csrc/jit/OVERVIEW.md
# https://pytorch.org/docs/master/jit.html#interpreting-graphs
# https://github.com/pytorch/pytorch/blob/8fe2a5e91b79e3f9b6f6c632fdf7f39ec3bf4fca/torch/csrc/jit/ir/ir.h


# CallMethod resolution:
# https://github.com/pytorch/pytorch/blob/b6bb644e41b3928b5a515330ad35c8b447fcb876/torch/csrc/jit/serialization/python_print.cpp#L984-L1004

output.write("#| -------- Graph ----------")
output.write("\n")
output.write("\n")

for (name, member) in dynamicMembers:
    output.write(str(member.graph))
    output.write("\n")
output.write("-------- |#")
output.write("\n")
output.write("\n")

symbolLook = {
    "Tensor": [sexpdata.Symbol("Vec"), sexpdata.Symbol("Float")]  # float vs integer?
}

def make_arg(input):
    # [input.debugName(), " :", input.type()]
    return [
        sexpdata.Symbol(input.debugName()),
        sexpdata.Symbol(":"),
        symbolLook[str(input.type())]
    ]




def managleDebugName(name):
    return "_" + name.replace(".", "_")

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

    return [sexpdata.Symbol("\n"), sexpdata.Symbol("_" + value.debugName()), literal]    

def make_print(node):
    mangled_id = managleDebugName(node.inputsAt(0).debugName())
    return [sexpdata.Symbol("print"), sexpdata.Symbol(mangled_id)]

def make_list_init_inner(values, i, rescue):
    value = values[0]
    rhs = sexpdata.Symbol(rescue) # How do we handled index out of range?
    if (len(values) > 1):
        rhs = make_list_init_inner(values[1:], i + 1, rescue)
    return [sexpdata.Symbol("if"), [sexpdata.Symbol("eq"), sexpdata.Symbol("ni"), i], sexpdata.Symbol(managleDebugName(value.debugName())), rhs]

def make_list_init(values):
    # We may switch to vector literals later: https://github.com/microsoft/knossos-ksc/issues/310
    # in the meantime here's a quick-and-dirty translation to chained if
    # CAUTION: if it goes out of range it uses the first value!
    return make_list_init_inner(values, 0, managleDebugName(values[0].debugName()))

def make_list(node):
    value = node.outputsAt(0)

    list_size = str(sum(1 for _ in node.inputs()))
    return [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol("_" + value.debugName()),
        [
            sexpdata.Symbol("build"),
            sexpdata.Symbol(list_size),
            [
                sexpdata.Symbol("lam"),
                [
                    sexpdata.Symbol("ni"),
                    sexpdata.Symbol(":"),
                    sexpdata.Symbol("Integer")
                ],
                make_list_init(list(node.inputs()))
            ]
        ]
    ]    

def make_tensor(node):
    # tensors aren't explicitly modelled in Knossos yet, leave them as identify over a (jagged) list for now
    value = node.outputsAt(0)

    return [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol(managleDebugName(value.debugName())),
        sexpdata.Symbol(managleDebugName(node.inputsAt(0).debugName()))]

def make_default(node):
    print("TODO:" + node.kind() )
    return sexpdata.Symbol("")


# prim::Constant
# prim::ListConstruct
# prim::ListConstruct
# prim::ListConstruct
# aten::tensor
# prim::CallFunction
# prim::Print

lookups = {
    'prim::Constant': make_constant,
    'prim::Print': make_print,
    'prim::ListConstruct': make_list,
    'aten::tensor': make_tensor
}


def make_node(node):
    return lookups.get(node.kind(), make_default)(node)


def ts2ks(function):
    name = sexpdata.Symbol(function.name)

    args = [make_arg(item) for item in function.graph.inputs() ]

    stuff = list(function.graph.nodes())[0:-1]

    binds = [make_node(node) for node in stuff]
    op = make_node(list(function.graph.nodes())[-1])

    body = [sexpdata.Symbol("\n"), sexpdata.Symbol("let"), binds, sexpdata.Symbol("\n"), op]

    whole_exp = [sexpdata.Symbol('def'), name, sexpdata.Symbol("Integer"), args, body]

    output.write(sexpdata.dumps(whole_exp))

for (name, member) in dynamicMembers:
    ts2ks(member)
    output.write("\n\n")
    #print(name)