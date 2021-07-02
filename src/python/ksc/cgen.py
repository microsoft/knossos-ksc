from ksc import utils


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


def generate_cpp_entry_points(bindings_to_generate, decls):
    decls_by_name = {decl.name: decl for decl in decls}

    def lookup_decl(structured_name):
        if structured_name not in decls_by_name:
            raise ValueError(f"No ks definition found for binding: {structured_name}")
        return decls_by_name[structured_name]

    cpp_entry_points = "".join(
        generate_cpp_entry_point(binding_name, lookup_decl(structured_name))
        for binding_name, structured_name in bindings_to_generate
    )

    return f"""
#include "knossos-entry-points.h"

namespace ks {{
namespace entry_points {{
namespace generated {{
{cpp_entry_points}
}}
}}
}}
"""


def generate_cpp_entry_point(cpp_function_name, decl):
    return_type = decl.return_type
    arg_types = [arg.type_ for arg in decl.args]
    if len(arg_types) == 1 and arg_types[0].is_tuple:
        arg_types = arg_types[0].children
    arg_names = [f"arg{i}" for i in range(len(arg_types))]

    args = ", ".join(
        f"{ks_cpp_type(arg_type)} {arg_name}"
        for arg_type, arg_name in zip(arg_types, arg_names)
    )
    cpp_declaration = f"{ks_cpp_type(return_type)} {cpp_function_name}({args})"

    ks_function_name = utils.encode_name(decl.name.mangled())
    arg_list = ", ".join(["&g_alloc"] + arg_names)
    cpp_call = f"ks::{ks_function_name}({arg_list})"
    args_streamed = ' << ", " '.join(f" << {arg}" for arg in arg_names)

    return f"""
{cpp_declaration} {{
    if (g_logging) {{
        std::cerr << "{ks_function_name}("{args_streamed} << ") =" << std::endl;
        auto ret = {cpp_call};
        std::cerr << ret << std::endl;
        return ret;
    }} else {{
        return {cpp_call};
    }}
}}
"""
