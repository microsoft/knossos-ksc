import atexit
from dataclasses import dataclass
from typing import Tuple, Union
from collections import namedtuple
import itertools
import os
import atexit

import importlib.util
import numpy as np
import subprocess
import sysconfig
import sys
from tempfile import NamedTemporaryFile
from tempfile import gettempdir

from ksc.type import Type, tangent_type, make_tuple_if_many

from torch.utils.cpp_extension import load, load_inline


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
        for (nt, v) in args.items():
            # assert nt in self.__annotations__  # <- This check will fail for chains of derived classes -- only the deepest has __annotations__ ready yet.
            setattr(self, nt, v)

    def __eq__(self, that):
        if type(self) != type(that):
            return False

        for nt in self.__annotations__:
            if getattr(self, nt) != getattr(that, nt):
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


def single_elem(l):
    assert len(l) == 1
    return l[0]


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


def get_ksc_dir():
    if "KSC_RUNTIME_DIR" in os.environ:
        ksc_runtime_dir = os.environ["KSC_RUNTIME_DIR"]
        ksc_src = os.path.dirname(ksc_runtime_dir)
        return os.path.dirname(ksc_src)

    d = os.path.dirname(__file__)  # src/python/ksc
    d = os.path.dirname(d)  # src/python
    d = os.path.dirname(d)  # src
    return os.path.dirname(d)


def get_ksc_paths():
    if "KSC_RUNTIME_DIR" in os.environ:
        ksc_runtime_dir = os.environ["KSC_RUNTIME_DIR"]
    else:
        ksc_runtime_dir = get_ksc_dir() + "/src/runtime"

    if "KSC_PATH" in os.environ:
        ksc_path = os.environ["KSC_PATH"]
    else:
        ksc_path = get_ksc_dir() + "/build/bin/ksc"

    return ksc_path, ksc_runtime_dir


def generate_cpp_from_ks(ks_str, generate_derivatives=False, use_aten=False):
    ksc_path, ksc_runtime_dir = get_ksc_paths()

    with NamedTemporaryFile(mode="w", suffix=".ks", delete=False) as fks:
        fks.write(ks_str)
    try:
        with NamedTemporaryFile(mode="w", suffix=".kso", delete=False) as fkso:
            with NamedTemporaryFile(mode="w", suffix=".cpp", delete=False) as fcpp:
                print("generate_cpp_from_ks:", ksc_path, fks.name)
                ksc_command = [
                    ksc_path,
                    "--generate-cpp" if generate_derivatives else "--generate-cpp-without-diffs",
                    "--ks-source-file",
                    ksc_runtime_dir + "/prelude.ks",
                    *(("--ks-source-file", ksc_runtime_dir + "/prelude-aten.ks") if use_aten else ()),
                    "--ks-source-file",
                    fks.name,
                    "--ks-output-file",
                    fkso.name,
                    "--cpp-output-file",
                    fcpp.name,
                ]
                e = subprocess.run(ksc_command, capture_output=True, check=True,)
                print(e.stdout.decode("ascii"))
                print(e.stderr.decode("ascii"))
    except subprocess.CalledProcessError as e:
        print(f"Command failed:\n{' '.join(ksc_command)}")
        print(f"files {fks.name} {fkso.name} {fcpp.name}")
        print(f"ks_str=\n{ks_str}")
        print(e.output.decode("ascii"))
        print(e.stderr.decode("ascii"))
        exit(-1)  # To clean up error reporting while debugging
        raise

    # Read from CPP back to string
    with open(fcpp.name) as f:
        out = f.read()

    # only delete these file if no error
    @atexit.register
    def _():
        print("ksc.utils.generate_cpp_from_ks: Deleting", fks.name, fcpp.name, fkso.name)
        os.unlink(fks.name)
        os.unlink(fcpp.name)
        os.unlink(fkso.name)

    return out


def build_py_module_from_cpp(cpp_str, profiling=False, use_aten=False):
    _ksc_path, ksc_runtime_dir = get_ksc_paths()
    pybind11_path = get_ksc_dir() + "/extern/pybind11"

    with NamedTemporaryFile(mode="w", suffix=".cpp", delete=False) as fcpp:
        fcpp.write(cpp_str)

    extension_suffix = sysconfig.get_config_var("EXT_SUFFIX")
    if extension_suffix is None:
        extension_suffix = sysconfig.get_config_var("SO")

    with NamedTemporaryFile(mode="w", suffix=extension_suffix, delete=False) as fpymod:
        pass
    module_path = fpymod.name
    module_name = os.path.basename(module_path).split(".")[0]
    python_includes = subprocess_run(
        [sys.executable, "-m", "pybind11", "--includes"], env={"PYTHONPATH": pybind11_path}
    )
    try:
        cmd = (
            f"g++ -I{ksc_runtime_dir} -I{pybind11_path}/include "
            + python_includes
            + " -Wall -Wno-unused-variable -Wno-unused-but-set-variable"
            " -fmax-errors=1"
            " -std=c++17"
            " -O3"
            " -fPIC"
            + (" -g -pg -O3" if profiling else "")
            + " -shared"
            + (" -DKS_INCLUDE_ATEN" if use_aten else "")
            + f" -DPYTHON_MODULE_NAME={module_name}"
            f" -o {module_path} " + fcpp.name
        )
        print(cmd)
        subprocess.run(cmd, shell=True, capture_output=True, check=True)
    except subprocess.CalledProcessError as e:
        print(f"cpp_file={fcpp.name}")
        print(cmd)
        print(e.output.decode("utf-8"))
        print(e.stderr.decode("utf-8"))

        raise

    os.unlink(fcpp.name)
    return module_name, module_path


def mangleType(ty):
    return ty.shortstr()


def mangleTypes(tys):
    return "".join(mangleType(ty) for ty in tys)


def encode_name(s: str) -> str:
    # TODO: this could be faster
    return (
        s.replace("@", "$a")
        .replace(",", "$_")
        .replace(".", "$o")
        .replace("[", "$6")
        .replace("]", "$9")
        .replace("<", "$d")
        .replace(">", "$b")
        .replace("*", "$x")
        .replace(":", "$8")
    )


def __make_cpp_str(ks_str, name_to_call, python_module_name, arg_types, return_type, generate_derivatives, use_aten):
    generated_cpp_source = generate_cpp_from_ks(ks_str, generate_derivatives=generate_derivatives, use_aten=use_aten)

    cpp_str = f"""
    #include "knossos-pybind.h"
    {generated_cpp_source}

    """

    args_str = mangleTypes(arg_types)
    name = name_to_call.mangle_without_type()
    name_str = encode_name(f"{name}@{args_str}")
    declarations = f"""
     m.def("entry", with_ks_allocator(&ks::{name_str}));
    """

    if generate_derivatives:
        derivatives_to_generate = [
            #'fwd',
            #'rev',
            "sufrev"
        ]
        darg_types = [tangent_type(ty) for ty in arg_types]
        args_tuple_str = mangleType(make_tuple_if_many(arg_types))
        dargs_tuple_str = mangleType(make_tuple_if_many(darg_types))
        dreturn_type_str = mangleType(tangent_type(return_type))

        for der in derivatives_to_generate:
            der_name = encode_name(f"{der}${name}@{args_str}")
            declarations += f"""
            m.def("{der}_entry", with_ks_allocator(&ks::{der_name}));
            """

    cpp_str += (
        """
PYBIND11_MODULE("""
        + python_module_name
        + """, m) {
    m.def("reset_allocator", []{ g_alloc.reset();});
    m.def("allocator_top", []{ return g_alloc.mark();});
    m.def("allocator_peak", []{ return g_alloc.peak();});

    declare_tensor_1<double>(m, "Tensor_1_Float");
    declare_tensor_2<double>(m, "Tensor_2_Float");
    declare_tensor_2<int>(m, "Tensor_2_Integer");

"""
        + declarations
        + """
}
"""
    )

    return cpp_str


def generate_and_compile_cpp_from_ks(
    ks_str, name_to_call, arg_types, return_type=None, generate_derivatives=False, use_aten=False
):

    cpp_str = __make_cpp_str(
        ks_str, name_to_call, "PYTHON_MODULE_NAME", arg_types, return_type, generate_derivatives, use_aten
    )

    cpp_fname = gettempdir() + "/ksc-pybind.cpp"  # TODO temp name, but I want to solve a GC problem with temp names
    print(f"Saving to {cpp_fname}")
    with open(cpp_fname, "w") as fcpp:
        fcpp.write(cpp_str)

    module_name, module_path = build_py_module_from_cpp(cpp_str, use_aten=use_aten)
    return import_module_from_path(module_name, module_path)


def build_module_using_pytorch_from_ks(
    ks_str, name_to_call, arg_types, return_type=None, generate_derivatives=False, use_aten=False
):
    """Uses PyTorch C++ extension mechanism to build and load a module"""
    cpp_str = __make_cpp_str(
        ks_str, name_to_call, "TORCH_EXTENSION_NAME", arg_types, return_type, generate_derivatives, use_aten
    )

    __ksc_path, ksc_runtime_dir = get_ksc_paths()

    cflags = [
        "-DKS_INCLUDE_ATEN" if use_aten else "",
    ]

    # I don't like this assumption about Windows -> cl but it matches what PyTorch is currently doing:
    # https://github.com/pytorch/pytorch/blob/ad8d1b2aaaf2ba28c51b1cb38f86311749eff755/torch/utils/cpp_extension.py#L1374-L1378
    # We're making a guess here if people recognifigure their C++ compiler on Windows it's because they're using non-MSVC
    # otherwise we need to inspect the end of the path path for cl[.exe].

    cpp_compiler = os.environ.get("CXX")
    if cpp_compiler == None and sys.platform == "win32":
        cflags.append("/std:c++17")
    else:
        cflags.append("-std=c++17")

    verbose = True

    # https://pytorch.org/docs/stable/cpp_extension.html
    module = load_inline(
        name="dynamic_ksc_cpp",
        cpp_sources=[cpp_str],
        extra_include_paths=[ksc_runtime_dir],
        extra_cflags=cflags,
        verbose=verbose,
    )

    return module


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


def singleton(cls):
    """ Simple decorator that makes a single instance of a class.
        @singleton
        class Foo:
            def do_foo(self):
                .....
        Foo.do_foo()
    """
    return cls()
