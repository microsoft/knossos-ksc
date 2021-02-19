from dataclasses import dataclass
from typing import Tuple, Union
from collections import namedtuple
import itertools

import importlib.util
import os
import numpy as np
import subprocess
import sysconfig
import sys
from tempfile import NamedTemporaryFile

from ksc.type import Type, tangent_type

class KRecord:
    """
    A smoother namedtuple -- like https://pythonhosted.org/pyrecord but using the existing class syntax.
    Like a 3.7 dataclass, but don't need to decorate each derived class

    Derive a class from KRecord, declare its fields, and use keyword args in __init__

    def MyClass(KRecord):
        cost: float
        names: List[String]

        def __init__(cost, names):
            super().__init__(cost=cost, names=names)

    And now you have a nice little record class.

    Construct a MyClass:
        a = MyClass(1.3, ["fred", "conor", "una"])

    Compare two MyClasses
        if a == b: ...
    
    Etc
    """

    def __init__(self, **args):
        for (nt,v) in args.items():
            # assert nt in self.__annotations__  # <- This check will fail for chains of derived classes -- only the deepest has __annotations__ ready yet.
            setattr(self, nt, v)

    def __eq__(self, that):
        if type(self) != type(that):
            return False

        for nt in self.__annotations__:
            if getattr(self, nt) != getattr(that,nt):
                return False
        return True




def ensure_list_of_lists(l):
    """return input, wrapped in a singleton list if its first element is not a list

       ensure_list_of_lists([])    = []
       ensure_list_of_lists([1])   = [[1]]
       ensure_list_of_lists([[1]]) = [[1]]
       ensure_list_of_lists([[1,2]])        = [[1, 2]]
       ensure_list_of_lists([[1,2], [3,4]]) = [[1, 2], [3, 4]]
    """

    if not isinstance(l, list):
        raise ValueError("Expect a list")
    if len(l) < 1:  # Empty list is empty list
        return l
    if not isinstance(l[0], list):
        return [l]
    else:
        return l

def paren(s):
    return "(" + s + ")"

PYTHON_MODULE_NAME = "ks_mod"

def import_module_from_path(module_name, path):
    # These three lines are for loading a module from a file in Python 3.5+
    # https://bugs.python.org/issue21436
    spec = importlib.util.spec_from_file_location(module_name, path)
    py_out = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(py_out)
    return py_out

def translate_and_import(source_file_name, *args):
    from ksc.translate import translate
    py_out = translate(*args, source_file_name, with_main=False)
    with NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(f"# AUTOGEN from {source_file_name} via ksc.utils.translate_and_import")
        f.write(py_out)

    print(f.name)
    return import_module_from_path(PYTHON_MODULE_NAME, f.name)

def subprocess_run(cmd, env=None):
    return subprocess.run(cmd, stdout=subprocess.PIPE, env=env).stdout.decode().strip("\n")

def get_ksc_paths():
    if "KSC_RUNTIME_DIR" in os.environ:
        ksc_runtime_dir = os.environ["KSC_RUNTIME_DIR"]
    else:
        ksc_runtime_dir = "./src/runtime"

    if "KSC_PATH" in os.environ:
        ksc_path = os.environ["KSC_PATH"]
    else:
        ksc_path = "./build/bin/ksc"
    
    return ksc_path,ksc_runtime_dir


def generate_cpp_from_ks(ks_str, generate_derivatives = False):
    ksc_path,_ksc_runtime_dir = get_ksc_paths()

    with NamedTemporaryFile(mode="w", suffix=".ks", delete=False) as fks:
        fks.write(ks_str)
    try:
        with NamedTemporaryFile(mode="w", suffix=".kso", delete=False) as fkso:
            with NamedTemporaryFile(mode="w", suffix=".cpp", delete=False) as fcpp:
                print("generate_cpp_from_ks:", ksc_path, fks.name)
                e = subprocess.run([
                    ksc_path,
                    "--generate-cpp" if generate_derivatives else "--generate-cpp-without-diffs",
                    "--ks-source-file", "src/runtime/prelude.ks",
                    "--ks-source-file", fks.name,
                    "--ks-output-file", fkso.name,
                    "--cpp-output-file", fcpp.name
                ], capture_output=True, check=True)
                print(e.stdout.decode('ascii'))
                print(e.stderr.decode('ascii'))
    except subprocess.CalledProcessError as e:
        print(f"files {fks.name} {fkso.name} {fcpp.name}")
        print(f"ks_str=\n{ks_str}")
        print(e.output.decode('ascii'))
        print(e.stderr.decode('ascii'))
        raise


    # Read from CPP back to string
    with open(fcpp.name) as f:
        out = f.read()

    # only delete these file if no error
    os.unlink(fks.name)
    os.unlink(fcpp.name)
    os.unlink(fkso.name)

    return out

def build_py_module_from_cpp(cpp_str, pybind11_path):
    _ksc_path,ksc_runtime_dir = get_ksc_paths()

    with NamedTemporaryFile(mode="w", suffix=".cpp", delete=False) as fcpp:
        fcpp.write(cpp_str)

    extension_suffix = sysconfig.get_config_var('EXT_SUFFIX')
    if extension_suffix is None:
        extension_suffix = sysconfig.get_config_var('SO')

    with NamedTemporaryFile(mode="w", suffix=extension_suffix, delete=False) as fpymod:
        pass
    module_path = fpymod.name
    module_name = os.path.basename(module_path).split(".")[0]
    python_includes = subprocess_run(
        [sys.executable, "-m", "pybind11", "--includes"],
        env={"PYTHONPATH": "extern/pybind11"}
    )
    try:
        cmd = (f"g++ -I{ksc_runtime_dir} -I{pybind11_path}/include "
               + python_includes
               + " -Wall -Wno-unused-variable -Wno-unused-but-set-variable"
                 " -fmax-errors=1"
                 " -std=c++17"
                 " -O3"
                 " -fPIC"
                 " -shared"
                 f" -DPYTHON_MODULE_NAME={module_name}"
                 f" -o {module_path} "
               + fcpp.name)
        print(cmd)
        subprocess.run(cmd, shell=True, capture_output=True, check=True)
    except subprocess.CalledProcessError as e:
        print(f"cpp_file={fcpp.name}")
        print(cmd)
        print(e.output.decode('utf-8'))
        print(e.stderr.decode('utf-8'))

        raise
    
    os.unlink(fcpp.name)
    return module_name, module_path

def make_tuple_if_many(types):
    if isinstance(types, list):
        if len(types) > 1:
            return Type.Tuple(*types)
        else:
            return types[0]
    else:
        return types

def mangleType(ty):
    return ty.shortstr()

def mangleTypes(tys):
    return "".join(mangleType(ty) for ty in tys)


def encode_name(s : str) -> str:
    # TODO: this could be faster
    return s.\
        replace('@',"$a").\
        replace(',',"$_").\
        replace('.',"$o").\
        replace('[',"$6").\
        replace(']',"$9").\
        replace('<',"$d").\
        replace('>',"$b").\
        replace('*',"$x").\
        replace(':',"$8")

def generate_and_compile_cpp_from_ks(ks_str, name_to_call, arg_types, return_type=None, generate_derivatives=False, pybind11_path="extern/pybind11"):

    generated_cpp_source = generate_cpp_from_ks(ks_str, generate_derivatives=generate_derivatives)

    cpp_str = """
#include <cstdint>

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/operators.h>

namespace py = pybind11;
""" + generated_cpp_source + """

int ks::main(ks::allocator *) { return 0; };

ks::allocator g_alloc{ 1'000'000'000 };

/* TODO: delete this comment -- leaving for now to sync diffs  
template<typename T>
void declare_vec(py::module &m, std::string typestr) {{
  using Class = ks::vec<T>;
  std::string pyclass_name = std::string("vec_") + typestr;
  py::class_<Class>(m, pyclass_name.c_str(), py::module_local())
    .def(py::init<>())
    .def(py::init([](std::vector<T> const& v) {{ return ks::vec<T>(&g_alloc, v); }}))
    .def("__getitem__", [](const ks::vec<T> &a, const int &b) {{
	return a[b];
      }})
    .def("__len__", [](const ks::vec<T> &a) {{ return a.size(); }});
}} */

template<typename T>
void declare_tensor_2(py::module &m, char const* name) {
  // Wrap ks_tensor<Dim, T> to point to supplied python memory
  py::class_<ks::tensor<2, T>>(m, name, py::buffer_protocol(), py::module_local())
    .def(py::init([](std::uintptr_t v, size_t m, size_t n) {
        if (v < 1u<<24) {
            // probably a misplaced size
            throw std::domain_error("generate_and_compile_cpp_from_ks: probable misplaced size");
        }
        ks::tensor_dimension<2>::index_type size {m,n};
        return ks::tensor<2, T>(size, reinterpret_cast<T*>(v)); // Reference to caller's data 
    }))
    // And describe buffer shape to Python
    // Returned tensors will be living on g_alloc, so will become invalid after allocator_reset()
    .def_buffer([](ks::tensor<2, T> &t) -> py::buffer_info {
        return py::buffer_info(
            t.data(),                               /* Pointer to buffer */
            sizeof(T),                              /* Size of one scalar */
            py::format_descriptor<T>::format(),     /* Python struct-style format descriptor */
            2,                                      /* Number of dimensions */
            { ks::get_dimension<0>(t.size()), ks::get_dimension<1>(t.size()) },         /* Buffer dimensions */
            { sizeof(T) * ks::get_dimension<0>(t.size()),             /* Strides (in bytes) for each index */
               sizeof(T) }
        );
    })
    ;
}

// Convert functor to one which takes a first argument g_alloc 
template<typename RetType, typename... ParamTypes>
auto with_ks_allocator(RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f](ParamTypes... params) { return f(&g_alloc, params...); };
}
"""

    args_str = mangleTypes(arg_types)
    name_str = encode_name(f"{name_to_call}@{args_str}")
    declarations = f"""
     m.def("main", with_ks_allocator(&ks::{name_str}));
    """

    if generate_derivatives:
        darg_types = [tangent_type(ty) for ty in arg_types]
        args_tuple_str = mangleType(make_tuple_if_many(arg_types))
        dargs_tuple_str = mangleType(make_tuple_if_many(darg_types))
        dreturn_type_str = mangleType(tangent_type(return_type))

        fwd_name = encode_name(f"fwd${name_to_call}@{args_tuple_str}")
        declarations += f"""
          m.def("fwd_entry", with_ks_allocator(&ks::{fwd_name}));
        """

        rev_name = encode_name(f"rev${name_to_call}@{args_tuple_str}")
        declarations += f"""
          m.def("rev_entry", with_ks_allocator(&ks::{rev_name}));
        """

    cpp_str += """
PYBIND11_MODULE(PYTHON_MODULE_NAME, m) {
    m.def("reset_allocator", []{g_alloc.reset();});
    m.def("allocator_top", []{ return g_alloc.mark();});
    m.def("allocator_peak", []{ return g_alloc.peak();});

    declare_tensor_2<double>(m, "Tensor_2_Float");
    declare_tensor_2<int>(m, "Tensor_2_Integer");

""" + declarations + """
}
"""
    pybindcpppath = "obj/pybind.cpp"
    print(f"Saving to {pybindcpppath}")    
    os.makedirs(os.path.dirname(pybindcpppath), exist_ok=True)
    with open(pybindcpppath, "w") as fcpp:
        fcpp.write(cpp_str)

    module_name, module_path = build_py_module_from_cpp(cpp_str, pybind11_path)
    return import_module_from_path(module_name, module_path)

def ndgrid_inds(sz):
    """
    Return a sequnce of tuples of indices as if generated by nested comprehensions.
    Example:
        ndgrid_inds((ni,nj))
    Returns the same sequence as
        [(i,j) for i in range(ni) for j in range(nj)]

    The iterates are always tuples so
        ndgrid_inds(4)
    returns
        [(0,), (1,), (2,), (3,)] 

    """

    return itertools.product(*map(range, sz))

