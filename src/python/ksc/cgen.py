from ksc import utils
from ksc.type import Type


scalar_type_to_cpp_map = {
    "Integer": "ks::Integer",
    "Float": "ks::Float",
    "Bool": "ks::Bool",
    "String": "std::string",
}


def ks_cpp_type(t):
    if t.is_scalar:
        return scalar_type_to_cpp_map[t.kind]
    elif t.is_tuple:
        return (
            "ks::Tuple<"
            + ", ".join(ks_cpp_type(child) for child in t.tuple_elems())
            + ">"
        )
    elif t.is_tensor:
        return f"ks::tensor<{t.tensor_rank}, {ks_cpp_type(t.tensor_elem_type)}>"
    else:
        raise ValueError(f'Unable to generate C++ type for "{t}"')


def entry_point_cpp_type(t, use_torch):
    if t.is_scalar:
        return scalar_type_to_cpp_map[t.kind]
    elif t.is_tuple:
        return (
            "ks::Tuple<"
            + ", ".join(entry_point_cpp_type(child) for child in t.tuple_elems())
            + ">"
        )
    elif t.is_tensor:
        if use_torch:
            if t.tensor_elem_type != Type.Float:
                raise ValueError(
                    f'Entry point signatures may only use tensors with floating-point elements (not "{t}")'
                )
            return "torch::Tensor"
        else:
            raise ValueError(f'Tensors in entry points are not supported "{t}"')
    else:
        raise ValueError(f'Unable to generate C++ type for "{t}"')


def generate_cpp_entry_points(bindings_to_generate, decls, use_torch=False):
    decls_by_name = {decl.name: decl for decl in decls}

    def lookup_decl(structured_name):
        if structured_name not in decls_by_name:
            raise ValueError(f"No ks definition found for binding: {structured_name}")
        return decls_by_name[structured_name]

    cpp_entry_points = "".join(
        generate_cpp_entry_point(
            binding_name, lookup_decl(structured_name), use_torch=use_torch
        )
        for binding_name, structured_name in bindings_to_generate
    )

    entry_point_header = (
        "knossos-entry-points-torch.h" if use_torch else "knossos-entry-points.h"
    )

    return f"""
#include "{entry_point_header}"

namespace ks {{
namespace entry_points {{
namespace generated {{
{cpp_entry_points}
}}
}}
}}
"""


def generate_cpp_entry_point(cpp_function_name, decl, use_torch):
    return_type = decl.return_type
    arg_types = [arg.type_ for arg in decl.args]
    if len(arg_types) == 1 and arg_types[0].is_tuple:
        arg_types = arg_types[0].children

    def arg_name(i):
        return f"arg{i}"

    def ks_arg_name(i):
        return f"ks_{arg_name(i)}"

    args = ", ".join(
        f"{entry_point_cpp_type(arg_type, use_torch)} {arg_name(i)}"
        for i, arg_type in enumerate(arg_types)
    )
    cpp_declaration = (
        f"{entry_point_cpp_type(return_type, use_torch)} {cpp_function_name}({args})"
    )

    convert_arguments = "\n".join(
        f"    auto {ks_arg_name(i)} = convert_argument<{ks_cpp_type(arg_type)}>({arg_name(i)});"
        for i, arg_type in enumerate(arg_types)
    )

    ks_function_name = utils.encode_name(decl.name.mangled())
    arg_list = ", ".join(["&g_alloc"] + [ks_arg_name(i) for i in range(len(arg_types))])
    cpp_call = f"""
    auto ks_ret = ks::{ks_function_name}({arg_list});
    auto ret = convert_return_value<{entry_point_cpp_type(return_type, use_torch)}>(ks_ret);
    """

    args_streamed = ' << ", " '.join(
        f" << {arg_name(i)}" for i in range(len(arg_types))
    )

    return f"""
{cpp_declaration} {{
    if (g_logging) {{
        std::cerr << "{ks_function_name}("{args_streamed} << ") =" << std::endl;
    }}
{convert_arguments}
{cpp_call}
    if (g_logging) {{
        std::cerr << ret << std::endl;
    }}
    return ret;
}}
"""
