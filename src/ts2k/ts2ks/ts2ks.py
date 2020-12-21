import sexpdata
import functools
import torch

from ksc.expr import Expr
from ksc import utils

# Background reading
# https://github.com/pytorch/pytorch/blob/master/torch/csrc/jit/OVERVIEW.md
# https://pytorch.org/docs/master/jit.html#interpreting-graphs
# https://github.com/pytorch/pytorch/blob/8fe2a5e91b79e3f9b6f6c632fdf7f39ec3bf4fca/torch/csrc/jit/ir/ir.h

# Newline constant for s-expr printing
nl = "\n"
tab = "\t"

add_edef = "(edef addATEN (Vec (Vec Float)) ((Vec (Vec Float)) Float))"

# CallMethod resolution:
# https://github.com/pytorch/pytorch/blob/b6bb644e41b3928b5a515330ad35c8b447fcb876/torch/csrc/jit/serialization/python_print.cpp#L984-L1004

symbolLook = {
    "Tensor": [
        sexpdata.Symbol("Vec"),
        [sexpdata.Symbol("Vec"), sexpdata.Symbol("Float")],
    ],  # float vs integer? also determine rank instead of hardcode
    "int": [sexpdata.Symbol("Integer")],
    "float": [sexpdata.Symbol("Float")],
    "Optional[Tensor]": [  # Just say optionals are required for now. TODO: don't do this!
        sexpdata.Symbol("Vec"),
        [sexpdata.Symbol("Vec"), sexpdata.Symbol("Float")],
    ],  # float vs integer? also determine rank instead of hardcode
    "Optional[bool]": [  # Just say optionals are required for now. TODO: don't do this!
        sexpdata.Symbol("Bool")
    ],
}

# We're going to have to break out the data structure at some point, for now, hardcode
# No recursive literals
symbolLook["Tuple[int, Tensor]"] = [
    sexpdata.Symbol("Tuple"),
    symbolLook["int"],
    symbolLook["Tensor"],
]

# hardcoding BertScriptableForQuestionAnswering TODO: getting urgent now to break up the return type and transform
symbolLook[
    "Tuple[Optional[Tensor], Tensor, Tensor, Optional[List[Tensor]], Optional[List[Tensor]]]"
] = [
    sexpdata.Symbol("Tuple"),
    symbolLook["Tensor"],
    symbolLook["Tensor"],
    sexpdata.Symbol("Vec"),
    [symbolLook["Tensor"]],
    sexpdata.Symbol("Vec"),
    [symbolLook["Tensor"]],
]
#


def mangleDebugName(name):
    return "_" + name.replace(".", "_")


def make_arg(input):
    type_lookup = str(input.type())
    if "Optional" in type_lookup:
        print("WARNING: treated as required:" + type_lookup)
    return [
        sexpdata.Symbol(mangleDebugName(input.debugName())),
        sexpdata.Symbol(":"),
        symbolLook[type_lookup],
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

    list_size = sum(1 for _ in node.inputs())
    if list_size == 0:
        return (
            []
        )  # may not be very practical on the Knossos side, some of the TorchScript workarounds currently mutates in place
    else:
        return [
            sexpdata.Symbol("\n"),
            sexpdata.Symbol("_" + value.debugName()),
            [
                sexpdata.Symbol("build"),
                sexpdata.Symbol(str(list_size)),
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
    # tensors aren't explicitly modelled in Knossos yet, leave them as identity over a (jagged) list for now
    value = node.outputsAt(0)

    return [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol(mangleDebugName(value.debugName())),
        sexpdata.Symbol(mangleDebugName(node.inputsAt(0).debugName())),
    ]


def make_aten_function(node, value, function_name):
    return [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol(mangleDebugName(value.debugName())),
        [
            sexpdata.Symbol(function_name),
            sexpdata.Symbol(mangleDebugName(node.inputsAt(0).debugName())),
            sexpdata.Symbol(mangleDebugName(node.inputsAt(1).debugName())),
        ],
    ]


def make_builtin_function(node, value, function_name):
    return [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol(mangleDebugName(value.debugName())),
        [
            sexpdata.Symbol(function_name),
            sexpdata.Symbol(
                mangleDebugName(node.inputsAt(0).debugName())
            ),  # Assum arity 2, enoygh for now
            sexpdata.Symbol(mangleDebugName(node.inputsAt(1).debugName())),
        ],
    ]


def make_add(generate_edef, node):
    value = node.outputsAt(0)
    if generate_edef:
        return make_aten_function(node, value, "addATEN")
    else:
        print(
            "WARNING: aten::add built-in just returns unmodified tensor, consider using --generate_edef"
        )
        return [
            sexpdata.Symbol("\n"),
            sexpdata.Symbol(mangleDebugName(value.debugName())),
            sexpdata.Symbol(mangleDebugName(node.inputsAt(0).debugName())),
        ]


def make_lt(generate_edef, node):
    value = node.outputsAt(0)
    if generate_edef:
        return make_aten_function(node, value, "ltATEN")
    else:
        return make_builtin_function(node, value, "lt")  # Assume floats, enough for now


def make_mul(generate_edef, node):
    value = node.outputsAt(0)
    if generate_edef:
        return make_aten_function(node, value, "mulATEN")
    else:
        return make_builtin_function(
            node, value, "mul"
        )  # Assume floats, enough for now


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


def make_if(make_binds, node):
    def make_branch(block):
        binds = make_binds(block.nodes())
        if len(binds) > 0:
            return [sexpdata.Symbol("let")] + binds + [make_return(block.returnNode())]
        else:
            return make_return(block.returnNode())

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

    return [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol(identifier),
        [
            sexpdata.Symbol("if"),
            sexpdata.Symbol(conditional),
            sexpdata.Symbol("\n\t"),
            success_branch,
            sexpdata.Symbol("\n\t"),
            failure_branch,
            sexpdata.Symbol("\n"),
        ],
    ]


def make_default(node):
    print("WARNING, unimplmented node kind: " + node.kind())
    return sexpdata.Symbol("")


def write_edefs(output):
    output.write(add_edef)
    output.write("\n")
    output.write("\n")


def ts2ks_fromgraph(generate_edefs, name, graph):

    def translate_node(make_binds, node):
        lookups = {
            "prim::Constant": make_constant,
            "prim::Print": make_print,
            "prim::ListConstruct": make_list,
            "aten::tensor": make_tensor,
            "aten::add": functools.partial(make_add, generate_edef=generate_edefs),
            "aten::lt": functools.partial(make_lt, generate_edef=generate_edefs),
            "aten::mul": functools.partial(make_mul, generate_edef=generate_edefs),
            "prim::Return": make_return,
            "prim::CallFunction": make_callfunction,
            "prim::If": functools.partial(make_if, make_binds=make_binds),
        }

        return lookups.get(node.kind(), make_default)(node=node)

    def make_binds(nodes):
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
        return_type = sexpdata.Symbol("Integer")
    else:
        if print_count > 0:
            print(
                "WARNING: print statement currently only supported as final operation"
            )
        return_node = graph.return_node()
        op = translate_node(make_binds, return_node)
        return_type = symbolLook[str(return_node.inputsAt(0).type())]

    body = [
        sexpdata.Symbol("\n"),
        sexpdata.Symbol("let"),
        binds,
        sexpdata.Symbol("\n"),
        op,
    ]

    name_as_symbol = sexpdata.Symbol(name)

    whole_exp = [sexpdata.Symbol("def"), name_as_symbol, return_type, args, body]

    return sexpdata.dumps(whole_exp)


# TODO: make an configuration named tuple rather than passing flags
def ts2ks(output, generate_edefs, function):
    s = ts2ks_fromgraph(generate_edefs, function.name, function.graph)
    output.write(s)

def ts2mod(function, arg_types):
    fn = torch.jit.script(function)
    ks_str = ts2ks_fromgraph(False, fn.name, fn.graph)
    mod = utils.generate_and_compile_cpp_from_ks(ks_str, fn.name, arg_types)
    return mod.main
