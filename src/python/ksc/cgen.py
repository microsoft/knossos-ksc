from typing import Any
from enum import Enum
from dataclasses import dataclass, field

from ksc import utils
from ksc.type import Type
from ksc.expr import Def


class VecSpec:
    """
    Options for vectorization of knossos-registered functions.

    Suppose a knossos function
       def f(x : Tensor) -> Tensor 

    is called with x a tensor of size [PxMxN].  
    
    The VecSpec_* derived class decides how f is mapped over this argument as follows:
    
     None: f is compiled to take rank 3 tensors.
     Elementwise: f is compiled to take floats (rank 0), and is computed elementwise.
     VMap: f is compiled to take rank 2 tensors, and mapped over the first dimension.
    """

    pass


@dataclass
class VecSpec_None(VecSpec):
    pass

    def str(self):
        return "VSnone"


@dataclass
class VecSpec_Elementwise(VecSpec):
    example_element: Any = field(default=1.1)

    def str(self):
        return "VSelem"


@dataclass
class VecSpec_VMap(VecSpec):
    dims_to_strip: int = field(default=1)

    def str(self):
        return "VSvmap"


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
            "std::tuple<"
            + ", ".join(
                entry_point_cpp_type(child, use_torch) for child in t.tuple_elems()
            )
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


def generate_cpp_entry_points(
    bindings_to_generate, decls, vectorization, use_torch=False
):
    decls_by_name = {decl.name: decl for decl in decls}

    def lookup_decl(structured_name):
        if structured_name not in decls_by_name:
            raise ValueError(f"No ks definition found for binding: {structured_name}")
        return decls_by_name[structured_name]

    cpp_declarations, cpp_definitions = zip(
        *(
            generate_cpp_entry_point(
                binding_name,
                lookup_decl(structured_name),
                vectorization=vectorization,
                use_torch=use_torch,
            )
            for binding_name, structured_name in bindings_to_generate
        )
    )

    entry_point_header = (
        "knossos-entry-points-torch.h" if use_torch else "knossos-entry-points.h"
    )

    return (
        f"""
#include "knossos-types.h"

namespace ks {{
namespace entry_points {{
namespace generated {{
{"".join(cpp_declarations)}
}}
}}
}}
""",
        f"""
#include "{entry_point_header}"

namespace ks {{
namespace entry_points {{
namespace generated {{
{"".join(cpp_definitions)}
}}
}}
}}

#include "knossos-entry-points.cpp"
""",
    )


def arg_types_of_decl(decl):
    arg_types = [arg.type_ for arg in decl.args]
    if len(arg_types) == 1 and arg_types[0].is_tuple:
        return arg_types[0].children  # undo one-argification to match ksc cgen
    else:
        return arg_types


def generate_cpp_entry_point(
    cpp_function_name: str, decl: Def, vectorization: VecSpec, use_torch: bool
):
    if isinstance(vectorization, VecSpec_Elementwise):
        if not use_torch:
            raise ValueError("Elementwise operations only available when using torch")
        return generate_cpp_elementwise_entry_point(cpp_function_name, decl)
    if isinstance(vectorization, VecSpec_VMap):
        if not use_torch:
            raise ValueError("VMap only available when using torch")
        return generate_cpp_vmap_entry_point(cpp_function_name, decl)

    arg_types = arg_types_of_decl(decl)
    num_args = len(arg_types)

    def join_args(sep, callable):
        return sep.join(callable(i) for i in range(num_args))

    ks_function_name = utils.encode_name(decl.name.mangled())

    cpp_arg_types = [entry_point_cpp_type(t, use_torch) for t in arg_types]
    cpp_return_type = entry_point_cpp_type(decl.return_type, use_torch)

    # torch::Tensor entry_my_kernel(torch::Tensor arg0, ..., torch::Tensor arg7)
    cpp_function = f"{cpp_return_type} {cpp_function_name}({join_args(', ', lambda i: f'{cpp_arg_types[i]} arg{i}')})"

    cpp_declaration = f"{cpp_function};\n"

    cpp = f"""
{cpp_function} {{
"""

    # auto ks_arg0 = convert_argument<ks::tensor<Dim, Float>>(arg0);
    # ...
    # auto ks_arg7 = convert_argument<ks::tensor<Dim, Float>>(arg7);
    for i in range(num_args):
        cpp += f"    auto ks_arg{i} = convert_argument<{ks_cpp_type(arg_types[i])}>(arg{i});\n"

    # auto ks_ret = ks::my_kernel(&g_alloc, ks_arg0, ..., ks_arg7);
    cpp += f"""
    auto ks_ret = ks::{ks_function_name}(&g_alloc {join_args("", lambda i: f", ks_arg{i}")});
"""

    # convert return value and return
    cpp += f"""
    return convert_return_value<{cpp_return_type}>(ks_ret);
}}
"""
    return cpp_declaration, cpp


def generate_cpp_elementwise_entry_point(cpp_function_name, decl):
    arg_types = arg_types_of_decl(decl)
    if not all(a == Type.Float for a in arg_types):
        raise ValueError(
            "Elementwise operations only available for floating-point element type"
        )
    num_args = len(arg_types)

    def join_args(sep, callable):
        return sep.join(callable(i) for i in range(num_args))

    ks_function_name = utils.encode_name(decl.name.mangled())

    # torch::Tensor entry_my_kernel(torch::Tensor arg0, ..., torch::Tensor arg7)
    cpp_function = f"torch::Tensor {cpp_function_name}({join_args(', ', lambda i: f'torch::Tensor arg{i}')})"

    cpp_declaration = f"{cpp_function};\n"

    cpp = f"""
{cpp_function} {{
"""

    # auto* arg_data0 = arg0.data_ptr<float>();
    # ...
    # auto* arg_data7 = arg7.data_ptr<float>();
    for i in range(num_args):
        cpp += f"""
    KS_ASSERT(arg{i}.is_contiguous());
    KS_ASSERT(arg{i}.scalar_type() == scalar_type_of_Float);
    auto* arg_data{i} = arg{i}.data_ptr<float>();
"""
    # ret_data[i] = ks::my_op(&g_alloc, arg_data0[i], arg_data1[i]);
    cpp += f"""
    auto ret = torch::empty_like(arg0);
    auto* ret_data = ret.data_ptr<float>();
    for (int i = 0, ne = arg0.numel(); i != ne; ++i) {{
        ret_data[i] = ks::{ks_function_name}(&g_alloc {join_args("", lambda i: f", arg_data{i}[i]")});
    }}
    return ret;
}}
"""
    return cpp_declaration, cpp


def generate_cpp_vmap_entry_point(cpp_function_name, decl):
    raise NotImplementedError("vmap")
