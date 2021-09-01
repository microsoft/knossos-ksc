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
    bindings_to_generate, decls, vectorization, use_torch=False, gpu=False
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
                gpu=gpu,
            )
            for binding_name, structured_name in bindings_to_generate
        )
    )

    entry_point_header = (
        "knossos-entry-points-torch-cuda.cuh"
        if gpu
        else "knossos-entry-points-torch.h"
        if use_torch
        else "knossos-entry-points.h"
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
    cpp_function_name: str,
    decl: Def,
    vectorization: VecSpec,
    use_torch: bool,
    gpu: bool,
):
    if isinstance(vectorization, VecSpec_Elementwise):
        if not use_torch:
            raise ValueError("Elementwise operations only available when using torch")
        if gpu:
            return generate_cpp_cuda_entry_point(cpp_function_name, decl)
        else:
            return generate_cpp_elementwise_entry_point(cpp_function_name, decl)
    if gpu:
        raise ValueError(
            "Only elementwise operations can be compiled for GPU"
        )  # TODO: could also support vmap
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

    # auto ks_arg0 = convert_to_ks_viewing_tensordata<ks::tensor<Dim, Float>>(arg0);
    # ...
    # auto ks_arg7 = convert_to_ks_viewing_tensordata<ks::tensor<Dim, Float>>(arg7);
    for i in range(num_args):
        cpp += f"    auto ks_arg{i} = convert_to_ks_viewing_tensordata<{ks_cpp_type(arg_types[i])}>(arg{i});\n"

    # auto ks_ret = ks::my_kernel(&g_alloc, ks_arg0, ..., ks_arg7);
    cpp += f"""
    auto ks_ret = ks::{ks_function_name}(&g_alloc {join_args("", lambda i: f", ks_arg{i}")});
"""

    # convert return value and return
    cpp += f"""
    return convert_from_ks<{cpp_return_type}>(ks_ret);
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


def generate_cpp_cuda_entry_point(cpp_function_name, decl):
    arg_types = arg_types_of_decl(decl)
    if not all(a == Type.Float for a in arg_types):
        raise ValueError(
            "Elementwise operations only available for floating-point element type"
        )
    num_args = len(arg_types)
    if num_args != 1 and num_args != 2:
        raise ValueError("CUDA entry points must have 1 or 2 arguments")
    map_function_name = "map_gpu" if num_args == 1 else "map2_gpu"

    def join_args(sep, callable):
        return sep.join(callable(i) for i in range(num_args))

    ks_function_name = utils.encode_name(decl.name.mangled())

    cpp_function = f"torch::Tensor {cpp_function_name}({join_args(', ', lambda i: f'torch::Tensor arg{i}')})"

    cpp_declaration = f"{cpp_function};\n"

    cpp = f"""
    struct functor_{cpp_function_name}
    {{
        template<typename scalar_t>
        inline __device__ scalar_t operator()({join_args(', ', lambda i: f'scalar_t arg{i}')}) {{
            return {ks_function_name}(nullptr, {join_args(', ', lambda i: f'arg{i}')});
        }}
    }};
    {cpp_function} {{
        return {map_function_name}({join_args(', ', lambda i: f'arg{i}')}, functor_{cpp_function_name}{{}});
    }}
"""
    return cpp_declaration, cpp


def generate_cpp_vmap_entry_point(cpp_function_name, decl):
    def add_vmap_dimension(t: Type):
        if t.is_scalar:
            return Type.Tensor(1, t)
        if t.is_tensor:
            return Type.Tensor(t.tensor_rank + 1, t.tensor_elem_type)

        # Not clear how to to the remainder -- let's see a use case before deciding
        raise ValueError("Vmap understands only tensors for now")

    in_arg_types = arg_types_of_decl(decl)

    # Add a "vmap dimension" to each arg
    arg_types = tuple(add_vmap_dimension(a) for a in in_arg_types)
    ks_types = tuple(ks_cpp_type(a) for a in arg_types)

    num_args = len(arg_types)

    def join_args(callable, sep=", "):
        return sep.join(callable(k) for k in range(num_args))

    def concat_args(callable):
        return "".join(callable(k) for k in range(num_args))

    ks_name = utils.encode_name(decl.name.mangled())

    # torch::Tensor entry_my_kernel(torch::Tensor arg0, ..., torch::Tensor arg7)
    cpp_function = f"""
torch::Tensor {cpp_function_name}({join_args(lambda k: f'torch::Tensor arg{k}')})
    """

    cpp_declaration = f"{cpp_function};\n"

    cpp = f"""
{cpp_function} {{
    int64_t n = arg0.size(0);
"""

    # auto ks_arg0 = convert_to_ks_viewing_tensordata<ks::tensor<2,float>>(arg0)
    # ...
    # auto ks_arg7 = convert_to_ks_viewing_tensordata<ks::tensor<1,int>>(arg7)
    for k in range(num_args):
        cpp += f"""
    KS_ASSERT(arg{k}.is_contiguous());
    KS_ASSERT(arg{k}.scalar_type() == scalar_type_of_Float);
    KS_ASSERT(arg{k}.size(0) == n);

    auto ks_arg{k} = convert_to_ks_viewing_tensordata<{ks_types[k]}>(arg{k});
"""

    # Difficulty: depending on the rank of ks_ret, ks_ret[i] returns either
    #   Rank 1: a reference to the float at  index i
    #   Rank 2: an rvalue representing a view on subtensor i.
    # inplace add must act differently on each, so we switch on the return dimension here

    ks_return_type = add_vmap_dimension(decl.return_type)
    ks_return_dim = ks_return_type.tensor_rank
    if ks_return_dim == 1:
        cpp += f"""
    // Create Torch return value
    auto ret = torch::zeros({{n}});
    ks::Float* ret_ptr = ret.data_ptr<ks::Float>();

    KS_MARK(&g_alloc, mark);
    for (int i = 0; i != n; ++i) {{
        ret_ptr[i] = ks::{ks_name}(&g_alloc {concat_args(lambda k: f", ks_arg{k}[i]")});
        // We have copied the return value, can reset allocator
        KS_RESET(&g_alloc, mark);
    }}

    return ret;
}}
"""
    else:
        ks_sizes = ", ".join([f"size{d}" for d in range(ks_return_dim - 1)])
        cpp += f"""
    KS_ASSERT(n > 0); // TODO: Zero-size tensors

    // Make the first call to determine output size
    auto ret0 = ks::{ks_name}(&g_alloc {concat_args(lambda k: f", ks_arg{k}[0]")});

    // Create empty Torch return value
    auto [{ks_sizes}] = ret0.size();
    auto ret = torch::empty({{n, {ks_sizes}}});
    
    // And wrap it in ks - this is a view of the torch data, so convert_argument, not convert_return_value
    auto ks_ret = convert_to_ks_viewing_tensordata<ks::tensor<{ks_return_dim}, Float>>(ret);

    // Place 0th value in the output
    auto ks_ret0 = ks_ret[0];
    inplace_copy(&ks_ret0, ret0); // This would update a temporary in the 1D case

    // And then place the rest
    KS_MARK(&g_alloc, mark);
    for (int i = 1; i != n; ++i) {{
        auto val = ks::{ks_name}(&g_alloc {concat_args(lambda k: f", ks_arg{k}[i]")});
        auto ks_ret_view = ks_ret[i];
        inplace_copy(&ks_ret_view, val);
        // We have copied the return value, can reset allocator
        KS_RESET(&g_alloc, mark);
    }}

    return ret;
}}
"""
    print(cpp)
    return cpp_declaration, cpp
