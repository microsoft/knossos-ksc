from dataclasses import dataclass
from typing import Tuple, Union
from collections import namedtuple
import importlib.util
import os
import numpy as np
import subprocess
import sys
from tempfile import NamedTemporaryFile

from ksc.type import Type

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

def generate_cpp_from_ks(ks_str):
    if "KSC_PATH" in os.environ:
        ksc_path = os.environ["KSC_PATH"]
    else:
        ksc_path = "./build/bin/ksc"
    with NamedTemporaryFile(mode="w", suffix=".ks", delete=False) as fks:
        fks.write(ks_str)
    with NamedTemporaryFile(mode="w", suffix=".kso", delete=False) as fkso:
        pass
    with NamedTemporaryFile(mode="w", suffix=".cpp", delete=False) as fcpp:
        pass
    try:
        subprocess.check_call([
            ksc_path,
            "--generate-cpp-without-diffs",
            "--ks-source-file", fks.name,
            "--ks-output-file", fkso.name,
            "--cpp-output-file", fcpp.name
        ])
    except subprocess.CalledProcessError:
        print(f"ks_str={ks_str}")
        raise
    finally:
        os.unlink(fks.name)
    with open(fcpp.name) as f:
        out = f.read()
    # only delete these file if no error
    os.unlink(fcpp.name)
    os.unlink(fkso.name)
    return out

def build_py_module_from_cpp(cpp_str, pybind11_path):
    if "KSC_RUNTIME_DIR" in os.environ:
        ksc_runtime_dir = os.environ["KSC_RUNTIME_DIR"]
    else:
        ksc_runtime_dir = "./src/runtime"

    with NamedTemporaryFile(mode="w", suffix=".cpp", delete=False) as fcpp:
        fcpp.write(cpp_str)

    extension_suffix = subprocess_run(['python3-config', '--extension-suffix'])

    with NamedTemporaryFile(mode="w", suffix=extension_suffix, delete=False) as fpymod:
        pass
    module_path = fpymod.name
    module_name = os.path.basename(module_path).split(".")[0]
    python_includes = subprocess_run(
        [sys.executable, "-m", "pybind11", "--includes"],
        env={"PYTHONPATH": "pybind11"}
    )
    try:
        cmd = (f"g++-7 -I{ksc_runtime_dir} -I{pybind11_path}/include "
               + python_includes
               + " -Wall"
                 " -std=c++17"
                 " -O3"
                 " -fPIC"
                 " -shared"
                 f" -DPYTHON_MODULE_NAME={module_name}"
                 f" -o {module_path} "
               + fcpp.name)
        print(cmd)
        subprocess.check_call(cmd, shell=True)
    except subprocess.CalledProcessError:
        print(f"cpp_str={cpp_str}")
        raise
    finally:
        os.unlink(fcpp.name)
    return module_name, module_path

def arg_type_strings(types):
    return "".join(t.shortstr() for t in types)

def generate_and_compile_cpp_from_ks(ks_str, name_to_call, arg_types, pybind11_path="pybind11"):

    cpp_str = """
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/operators.h>

namespace py = pybind11;

{generated_cpp_source}

int ks::main(ks::allocator *) {{ return 0; }};

ks::allocator g_alloc{{ 1'000'000'000 }};

/* template<typename T>
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

template<typename RetType, typename... ParamTypes>
auto withGlobalAllocator(RetType(*f)(ks::allocator*, ParamTypes...)) {{
  return [f](ParamTypes... params) {{ return f(&g_alloc, params...); }};
}}

PYBIND11_MODULE(PYTHON_MODULE_NAME, m) {{
  m.def("main", withGlobalAllocator(&ks::{name_to_call}));
}}
""".format(
        generated_cpp_source=generate_cpp_from_ks(ks_str),
        name_to_call=(name_to_call + "@" + arg_type_strings(arg_types)).replace("@", "$a")
    )
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
    if len(sz) == 1:
        yield from ((i,) for i in range(sz[0]))
    else:
        for i in range(sz[0]):
            for rest in ndgrid_inds(sz[1:]):
                yield (i,) + rest

