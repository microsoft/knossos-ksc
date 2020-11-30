from collections import namedtuple
import importlib.util
import os
import numpy as np
import subprocess
import sys
from tempfile import NamedTemporaryFile

from ksc.type import Type

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

ShapeType = namedtuple("ShapeType", ["shape", "type"])

PYTHON_MODULE_NAME = "ks_mod"

def import_module_from_path(module_name, path):
    # These three lines are for loading a module from a file in Python 3.5+
    # https://bugs.python.org/issue21436
    spec = importlib.util.spec_from_file_location(module_name, path)
    py_out = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(py_out)
    return py_out

def translate_and_import(*args):
    from ksc.translate import translate
    py_out = translate(*args, with_main=False)
    with NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(py_out)
    print(f.name)
    return import_module_from_path(PYTHON_MODULE_NAME, f.name)

def subprocess_run(cmd, env=None):
    return subprocess.run(cmd, stdout=subprocess.PIPE, env=env).stdout.decode().strip("\n")

def generate_cpp_from_ks(ks_str):
    if "KSC_PATH" in os.environ:
        ksc_path = os.environ["KSC_PATH"]
    else:
        ksc_path = "./ksc"
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

def shape_type_from_object(o):
    # import here to avoid circular dependencies
    from ksc.abstract_value import AbstractValue
    if hasattr(o, "shape") and hasattr(o, "dtype"):
        # numpy array-like object
        if np.issubdtype(o.dtype, np.floating):
            el_type = Type.Float
        elif np.issubdtype(o.dtype, np.integer):
            el_type = Type.Integer
        elif np.issubdtype(o.dtype, np.bool_):
            el_type = Type.Bool
        else:
            raise ValueError(f"Cannot handle element type {o.dtype}")
        vec_type = el_type
        for _ in range(o.ndim):
            vec_type = Type.Vec(vec_type)
        return ShapeType(o.shape, vec_type)
    elif isinstance(o, AbstractValue):
        return o.shape_type
    elif hasattr(o, "data") and o.data is not None:
        # value node
        return shape_type_from_object(o.data)
    elif isinstance(o, list):
        s0, t0 = shape_type_from_object(o[0])
        assert all(shape_type_from_object(e) == (s0, t0) for e in o)
        return ShapeType((len(o),) + s0, Type.Vec(t0))
    elif isinstance(o, tuple):
        ss, ts = zip(*[shape_type_from_object(e) for e in o])
        return ShapeType(tuple(ss), Type.Tuple(*ts))
    elif isinstance(o, bool):
        return ShapeType((), Type.Bool)
    elif isinstance(o, int):
        return ShapeType((), Type.Integer)
    elif isinstance(o, float):
        return ShapeType((), Type.Float)
    else:
        raise ValueError(f"Cannot handle object {o}")
