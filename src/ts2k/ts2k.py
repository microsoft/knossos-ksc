import torch
import sexpdata
import os
import importlib.util
import sys
import inspect
import argparse
from pathlib import Path

parser = argparse.ArgumentParser(description="Convert TorchScript to Knossos IR")
parser.add_argument("--input_file", required=True, help="TorchScript [file_path].py")
parser.add_argument("--output_file", required=True, help="Knossos IR [file_path].ks")
parser.add_argument(
    "--generate_edef",
    action="store_true",
    help="Generate edef, otherwise default to pass-through",
)
args = parser.parse_args()

input_file_path = args.input_file
output_file_path = args.output_file
generate_edef = args.generate_edef

input_directory = os.path.dirname(input_file_path)

output_directory = os.path.dirname(output_file_path)
if output_directory != "":
    os.makedirs(output_directory, exist_ok=True)
output = open(output_file_path, "w")

# Newline constant for s-expr printing
nl = "\n"
tab = "\t"

from contextlib import contextmanager

# https://stackoverflow.com/a/41904558/35544
# submodule_search_locations doesn't work for this
@contextmanager
def add_to_path(p):
    import sys
    old_path = sys.path
    old_modules = sys.modules
    sys.modules = old_modules.copy()
    sys.path = sys.path[:]
    sys.path.insert(0, p)
    try:
        yield
    finally:
        sys.path = old_path
        sys.modules = old_modules


module_name = "DynamicLoadedModule"

with add_to_path(input_directory):
    spec = importlib.util.spec_from_file_location(module_name, Path(input_file_path))
    dynamicModule = importlib.util.module_from_spec(spec)
    # We deliberately don't make visible via sys.modules[module_name] = module
    spec.loader.exec_module(dynamicModule)

add_edef = "(edef addATEN (Vec (Vec Float)) ((Vec (Vec Float)) Float))"


# load all TorchScript methods in target modules
# (various limitations, local sibling modules)
dynamicMembers = inspect.getmembers(
    dynamicModule, predicate=lambda v: type(v) == torch.jit.ScriptFunction
)

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
    "Tensor": [
        sexpdata.Symbol("Vec"),
        [sexpdata.Symbol("Vec"), sexpdata.Symbol("Float")],
    ],  # float vs integer? also determine rank instead of hardcode
    "int" : [
        sexpdata.Symbol("Integer")
    ]
}


def mangleDebugName(name):
    return "_" + name.replace(".", "_")


def make_arg(input):
    return [
        sexpdata.Symbol(mangleDebugName(input.debugName())),
        sexpdata.Symbol(":"),
        symbolLook[str(input.type())],
    ]


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
    mangled_id = mangleDebugName(node.inputsAt(0).debugName())
    return sexpdata.Symbol("print"), sexpdata.Symbol(mangled_id)


def make_list_init_inner(values, i, rescue):
    value = values[0]
    rhs = [sexpdata.Symbol("assert"), sexpdata.Symbol("false"), sexpdata.Symbol(rescue)]
    if len(values) > 1:
        rhs = make_list_init_inner(values[1:], i + 1, rescue)
    return [
        sexpdata.Symbol("if"),
        [sexpdata.Symbol("eq"), sexpdata.Symbol("ni"), i],
        sexpdata.Symbol(mangleDebugName(value.debugName())),
        rhs,
    ]


def make_list_init(values):
    # We may switch to vector literals later: https://github.com/microsoft/knossos-ksc/issues/310
    # in the meantime here's a quick-and-dirty translation to chained if
    # CAUTION: if it goes out of range it uses the first value!
    return make_list_init_inner(values, 0, mangleDebugName(values[0].debugName()))


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
                    sexpdata.Symbol("Integer"),
                ],
                make_list_init(list(node.inputs())),
            ],
        ],
    ]


def make_tensor(node):
    # tensors aren't explicitly modelled in Knossos yet, leave them as identify over a (jagged) list for now
    value = node.outputsAt(0)

    return [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol(mangleDebugName(value.debugName())),
        sexpdata.Symbol(mangleDebugName(node.inputsAt(0).debugName())),
    ]


def make_add(node):
    value = node.outputsAt(0)
    if generate_edef:
        return [
            sexpdata.Symbol("\n"),
            sexpdata.Symbol(mangleDebugName(value.debugName())),
            [
                sexpdata.Symbol("addATEN"),
                sexpdata.Symbol(mangleDebugName(node.inputsAt(0).debugName())),
                sexpdata.Symbol(mangleDebugName(node.inputsAt(1).debugName())),
            ],
        ]
    else:
        print(
            "WARNING: aten::add just returns unmodified tensor, consider using --generate_edef"
        )
        return [
            sexpdata.Symbol("\n"),
            sexpdata.Symbol(mangleDebugName(value.debugName())),
            sexpdata.Symbol(mangleDebugName(node.inputsAt(0).debugName())),
        ]


def make_return(node):
    mangled_id = mangleDebugName(node.inputsAt(0).debugName())
    return sexpdata.Symbol(mangled_id)


def make_callfunction(node):
    value = node.outputsAt(0)
    function_name_constant = node.inputsAt(0).node()
    input1 = node.inputsAt(
        1
    )  # TODO: get all inputs instead of just first, 0th is function name itself
    function_name = function_name_constant.s("name")
    return [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol(mangleDebugName(value.debugName())),
        [  # get a function call - brackets are escaped by sexpdata
            sexpdata.Symbol(function_name),
            sexpdata.Symbol(mangleDebugName(input1.debugName())),
        ],
    ]


def make_default(node):
    print("WARNING, unimplmented node kind:" + node.kind())
    return sexpdata.Symbol("")


# prim::Constant
# prim::ListConstruct
# prim::ListConstruct
# prim::ListConstruct
# aten::tensor
# prim::CallFunction
# prim::Print

lookups = {
    "prim::Constant": make_constant,
    "prim::Print": make_print,
    "prim::ListConstruct": make_list,
    "aten::tensor": make_tensor,
    "aten::add": make_add,
    "prim::Return": make_return,
    "prim::CallFunction": make_callfunction,
}


def translate_node(node):
    return lookups.get(node.kind(), make_default)(node)


if generate_edef:
    output.write(add_edef)
    output.write("\n")
    output.write("\n")


def ts2ks(function):
    name = sexpdata.Symbol(function.name)

    args = [make_arg(item) for item in function.graph.inputs()]

    all_nodes = list(function.graph.nodes())

    binds = [translate_node(node) for node in all_nodes if node.kind() != "prim::Print"]
    print_count = sum(1 for node in all_nodes if node.kind() == "prim::Print")

    # HACK: if last operation is print, we want that otherwise it's a return value.
    # need to think about interaction between imperative Python and pure Knossos
    if list(function.graph.nodes())[-1].kind() == "prim::Print":
        if print_count > 1:
            print(
                "WARNING: multiple print statements used, only final one currently translated"
            )
        op = translate_node(list(function.graph.nodes())[-1])
        return_type = sexpdata.Symbol("Integer")
    else:
        if print_count > 0:
            print(
                "WARNING: print statement currently only supported as final operation"
            )
        return_node = function.graph.return_node()
        op = translate_node(return_node)
        return_type = symbolLook[str(return_node.inputsAt(0).type())]

    body = [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol("let"),
        binds,
        sexpdata.Symbol("\n"),
        op,
    ]

    whole_exp = [sexpdata.Symbol("def"), name, return_type, args, body]

    output.write(sexpdata.dumps(whole_exp))


for (name, member) in dynamicMembers:
    ts2ks(member)
    output.write("\n\n")
    # print(name)
