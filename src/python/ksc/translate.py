"""Tool for translating Knossos .ks file into Python .py file.
"""
import sys
import argparse
from collections import namedtuple

import ksc.backends
from ksc.backends import specs
from ksc.type import Type
from ksc.expr import Def, EDef, Rule, Const, Var, Lam, Call, Let, If
from ksc.parse_ks import parse_ks_file

def handle_let(let_var, let_expr, body, indent=4):
    joiner = ("\n" + (" " * indent))
    lambda_expr = joiner.join([
        f"let(var={let_expr},",
        f"    body=lambda {let_var}:",
        f"      {body}",
        ")"
    ])
    return lambda_expr

_global_prime_list = [2, 3, 5, 7, 11, 13]
_global_prime_index = 0

def get_type(arg_type):
    return arg_type.kind
    
def get_shape_dtype(arg_type):
    global _global_prime_index
    kind = get_type(arg_type)
    if kind == "Integer":
        return (), "np.int32"
    elif kind == "Float":
        return (), "np.float32"
    elif kind == "Vec":
        child_shape, child_type = get_shape_dtype(Type.Index(arg_type))
        dim = _global_prime_list[_global_prime_index % len(_global_prime_list)]
        _global_prime_index += 1
        return (dim,) + child_shape, child_type
    else:
        raise ValueError("Found {} in a Vec!".format(kind))

def is_tensor(arg_type):
    type = get_type(arg_type)
    if type != "Vec":
        return False
    child_type = Type.Index(arg_type)
    if child_type.kind == "Vec":
        return is_tensor(child_type)
    elif child_type.kind in ["Integer", "Float"]:
        return True
    else:
        return False

def type_to_sample(arg_type):
    type = get_type(arg_type)
    if type == "Integer":
        return "3"
    if type == "Bool":
        return "True"
    if type == "Float":
        return "np.random.uniform()"
    if type == "Vec":
        if is_tensor(arg_type):
            shape, dtype = get_shape_dtype(arg_type)
            return "np.random.uniform(0, 1, {}).astype({})".format(shape, dtype)
        else:
            return "[{} for _ in range(2)]".format(type_to_sample(Type.Index(arg_type)))
    elif type == "Tuple":
        return "({})".format(",\n  ".join([type_to_sample(t) for t in arg_type]))
    assert False, arg_type

def args_to_sample_values(args):
    arg_types = []
    for arg in args:
        sample = type_to_sample(arg.type)
        arg_types.append((arg.name, sample))
    return arg_types

def _value_to_str(name):
    return name if isinstance(name, str) else name.value()

_Def = namedtuple("_Def", ["name", "str", "sample_args"])
_EDef = namedtuple("_EDef", ["name", "py_name", "return_type"])

class Translator:
    def __init__(self, backend):
        self._backend = backend
        self._edefs = {}
        self._defs = []
        self._built_ins = ksc.backends.__dict__[backend]._built_ins

    @property
    def backend(self):
        return self._backend

    def normalize_def_name(self, name):
        specialized = specs.specialized_functions()
        if name in specialized:
            return specialized[name]
        if name in self._built_ins or name.startswith("get$"):
            return name
        if name in ["or", "and", "max", "abs", "assert"]:
            # need to special case them because they conflict with python functions
            return name + "_"
        return name.replace("$", "_")

    # Convert KS expression to python code string
    def handle_body(self, ex, indent=2):
        
        # symbols e.g. sin -> sin
        if isinstance(ex, Var):
            return ex.name
        
        # numeric literal e.g. 5.4 -> 5.4
        if isinstance(ex, Const):
            return repr(ex.value)
        
        joiner = ("\n" + (" " * indent))
        
        # (let ((var expr) ...) body)
        if isinstance(ex, Let):
            return handle_let(
                ex.var,
                self.handle_body(ex.rhs, indent+2),
                self.handle_body(ex.body, indent+2),
                indent=indent+2 # inner most indent
            )

        # Lambda e.g. (lam (var : type) body)
        if isinstance(ex, Lam):
            var_name = ex.arg.name
            body = self.handle_body(ex.body, indent+2)
            return joiner.join([
                f"(lambda {var_name}:",
                f"  {body}",
                ")"])

        # If-then-else e.g. (if c e1 e2)
        if isinstance(ex, If):
            # need to special case if because "if" is a python keyword
            cond = self.handle_body(ex.cond, indent+2)
            then_branch = self.handle_body(ex.t_body, indent+2)
            else_branch = self.handle_body(ex.f_body, indent+2)
            return joiner.join([
                f"if_then_else({cond},",
                f"             lambda: {then_branch},",
                f"             lambda: {else_branch})"
            ])

        # All others are function call e.g. (f e1 .. eN)
        assert isinstance(ex, Call), ex

        name = ex.name
        args = ex.args

        # Recursively handle args
        args_handled = []
        for i, arg in enumerate(args):
            try:
                args_handled.append(self.handle_body(arg, indent+2))
            except:
                raise ValueError("In {}, failed to handle"
                                " argument #{}, {}".format(name,
                                                            i + 1,
                                                            arg))
        
        # (get$m$n t)
        if name.startswith("get$"):
            index = int(name.split("$")[1]) - 1
            return "get_tuple_element({index}, {tuple_name})".format(tuple_name=args_handled[0], index=index)

        # (tuple e1 .. eN)
        if name == "tuple":
            return "make_tuple({tuple_args})".format(tuple_args=", ".join(args_handled))

        # other function calls     
        func_name = self.normalize_def_name(name)
        return "{func_name}({args})".format(func_name=func_name,
                                            args=", ".join(args_handled))


    def handle_def(self, ex):
        name = ex.name
        args = ex.args
        arg_names = [arg.name for arg in args]
        return _Def(name, """
def {name}({args}):
  return {body}
""".format(name=self.normalize_def_name(name),
           args=", ".join(arg_names),
           body=self.handle_body(ex.body)),
                   args_to_sample_values(args))

    def parse_defs(self, string_or_stream):
        defs_to_process = []
        for tld in parse_ks_file(string_or_stream):
            if isinstance(tld, EDef):
                name = tld.name
                py_name = self.normalize_def_name(name)
                if py_name in self._built_ins:
                    # if it is built-in no need to edef
                    print("translate: no need to emit edef for builtin ", tld, file=sys.stderr)                    
                    continue
                edef = _EDef(name, py_name, tld.return_type)
                self._edefs[name] = edef
            elif isinstance(tld, Def):
                name = tld.name
                if name.startswith("cost$") or name.startswith("shape$"):
                    # delay so that we read edefs first
                    defs_to_process.append(tld)
                else:
                    self._defs.append(self.handle_def(tld))
            else:
                print("translate: ignoring unrecognized definition type ", tld, file=sys.stderr)
        for tld in defs_to_process:
            self._defs.append(self.handle_def(tld))

    def make_main(self):
        main = self._defs[-1]
        return """

def main():
  {sample_args}
  print({main}({main_args}))

if __name__ == "__main__":
  main()
""".format(
        sample_args="\n  ".join("{} = {}".format(k, v) for k, v in main.sample_args),
        main=main.name,
        main_args=", ".join([k for k, _ in main.sample_args])
)

    def translate(self, ks_str):
        self.parse_defs(ks_str)
        if self.backend == "abstract":
            # in abstract backend, edefs are generated by _get_edef function.
            # py_name in this case maintains the type in a python-friendly
            # mangled format. The generated function will dynamically switch
            # between "concrete" execution and "abstract" execution depending
            # on the arguments. The concrete execution uses the edefs defined
            # in the common backend. The abstract execution uses the cost$ and
            # shape$ functions defined by the user. Keeping the name mangled
            # is necessary for calling the right version of the generated edef.
            imports = sorted(self._built_ins)
            edefs = [f'{edef.py_name} = ksc.backends.abstract._get_edef('
                     f'defs, "{edef.name}", '
                     f'{edef.return_type.__repr__()}, '
                     f'py_name_for_concrete="{self.normalize_def_name(edef.name)}")'
                     for edef in self._edefs.values()]
        else:
            # in most backends, edefs are imported from backend
            imports = sorted(set(self._built_ins
                                 + [edef.py_name for edef in self._edefs.values()]))
            edefs = []

        return '''import numpy as np
import ksc
from ksc.type import Type
from ksc.backends.{backend} import (
  {imports}
)
{defs}

defs={{
  {defs_map}
}}

{edefs}
'''.format(backend=self.backend,
           imports=",\n  ".join(imports),
           defs="\n".join([d.str for d in self._defs]),
           defs_map=",\n  ".join([f'"{d.name}": {self.normalize_def_name(d.name)}' for d in self._defs]),
           edefs="\n".join(edefs)
    )

def translate(ks_str, backend, with_main=True):
    translator = Translator(backend)
    ks_str = translator.translate(ks_str)

    if with_main:
        ks_str += translator.make_main()

    return ks_str

def main():
    parser = argparse.ArgumentParser(prog="python -m ksc.translate", description=__doc__)
    parser.add_argument("input_ks_file", type=str)
    parser.add_argument("--backend", choices=["common", "jax", "jax_input_last"], default="common")
    args = parser.parse_args()

    with open(args.input_ks_file) as f:
        print(translate(f.read(), args.backend))

if __name__ == "__main__":
    sys.exit(main())
