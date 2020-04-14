"""Tool for translating Knossos .ks file into Python .py file.
"""
import argparse
from collections import namedtuple
from functools import wraps
import sys

import sexpdata

import ksc.backends
from ksc.type import Type
from ksc.backends import specs

def _convert_to_type(se, allow_implicit_tuple=False):
    """ Converts an S-Expression representing a type, like (Vec Float) or (Tuple Float (Vec Float)),
        into a Type object, e.g. Type.Vec(Type.Float) or Type.Tuple(Type.Float, Type.Vec(Type.Float)).

        If allow_implicit_tuple is true, also converts a list of types into a Tuple, e.g.
        (Float (Vec Float)) becomes Type.Tuple(Type.Float, Type.Vec(Type.Float)), i.e. as if
        the S-Expression began with an extra "Tuple".
    """
    while isinstance(se, list) and len(se)==1:
        se=se[0] # Discard ((pointless)) brackets
    if isinstance(se, sexpdata.Symbol):
        return Type(se.value())
    if isinstance(se, list) and len(se)>0:
        if isinstance(se[0], sexpdata.Symbol):
            sym = se[0].value()
            children = [_convert_to_type(s) for s in se[1:]]
            if sym == "Vec" and len(se)==2:
                return Type.Vec(*children)
            if sym == "Tuple":
                return Type.Tuple(*children)
            # Fall through in case it's a list of types with allow_implicit_tuple.
        if allow_implicit_tuple:
            return Type.Tuple(*[_convert_to_type(s) for s in se])
    raise ValueError("Did not know how to parse type {}".format(se))

def _parse_to_s_exp(string_or_stream):
    return sexpdata.Parser(string_or_stream, nil=None, true="True", false="False").parse()

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

def get_var_name(s_exp):
    if isinstance(s_exp, sexpdata.Symbol):
        return s_exp.value()
    else:
        return s_exp[0].value()

def handle_let(let_var_names, let_exprs, body, indent=4):
    joiner = ("\n" + (" " * indent))
    let_var = let_var_names[-1]
    let_expr = let_exprs[-1]
    lambda_expr = joiner.join([
        f"let(var={let_expr},",
        f"    body=lambda {let_var}:",
        f"      {body}",
        ")"
    ])
    if len(let_var_names) > 1:
        return handle_let(let_var_names[:-1],
                              let_exprs[:-1],
                              lambda_expr,
                              indent=indent-2)
    else:
        return lambda_expr

_global_prime_list = [2, 3, 5, 7, 11, 13]
_global_prime_index = 0

def get_type(arg_type):
    return arg_type[0].value() if isinstance(arg_type, list) else arg_type.value()
def get_shape_dtype(arg_type):
    global _global_prime_index
    type = get_type(arg_type)
    if type == "Integer":
        return (), "np.int32"
    elif type == "Float":
        return (), "np.float32"
    elif type == "Vec":
        child_shape, child_type = get_shape_dtype(arg_type[1])
        dim = _global_prime_list[_global_prime_index % len(_global_prime_list)]
        _global_prime_index += 1
        return (dim,) + child_shape, child_type
    else:
        raise ValueError("Found {} in a Vec!".format(type))

def is_tensor(arg_type):
    type = get_type(arg_type)
    if type != "Vec":
        return False
    child_type = get_type(arg_type[1])
    if child_type == "Vec":
        return is_tensor(arg_type[1])
    elif child_type in ["Integer", "Float"]:
        return True
    else:
        return False

def type_to_sample(arg_type):
    type = get_type(arg_type)
    if type == "Integer":
        return "3"
    elif type == "Float":
        return "np.random.uniform()"
    elif type == "Vec":
        if is_tensor(arg_type):
            shape, dtype = get_shape_dtype(arg_type)
            return "np.random.uniform(0, 1, {}).astype({})".format(shape, dtype)
        else:
            return "[{} for _ in range(2)]".format(type_to_sample(arg_type[1]))
    elif type == "Tuple":
        return "({})".format(",\n  ".join([type_to_sample(t) for t in arg_type[1:]]))

def args_to_sample_values(args):
    arg_types = []
    for name, _, arg_type in args:
        name = _value_to_str(name)
        arg_types.append((name, type_to_sample(arg_type)))
    return arg_types

def _value_to_str(name):
    return name if isinstance(name, str) else name.value()

Def = namedtuple("Def", ["name", "str", "sample_args"])
EDef = namedtuple("EDef", ["name", "py_name", "return_type"])

class Translator:
    def __init__(self, backend):
        self._backend = backend
        self._edefs = {}
        self._defs = []
        self._built_ins = ksc.backends.__dict__[backend]._built_ins

    @property
    def backend(self):
        return self._backend

    # Reserved word constants
    sym_if = sexpdata.Symbol("if")
    sym_let = sexpdata.Symbol("let")
    sym_lam = sexpdata.Symbol("lam")
    sym_tuple = sexpdata.Symbol("tuple")

    def normalize_def_name(self, name):
        specialized = specs.specialized_functions()
        if name in specialized:
            return specialized[name]
        if name in self._built_ins or name.startswith("get$"):
            return name
        if name in ["or", "and", "max", "abs"]:
            # need to special case them because they conflict with python functions
           return name + "_"
        return name.replace("$", "_")

    # Convert s-exp for a KS expression to python code string
    def handle_body(self, s_exp, indent=2):
        
        # symbols e.g. sin -> sin
        if isinstance(s_exp, sexpdata.Symbol):
            return s_exp.value()
        
        # numeric literal e.g. 5.4 -> 5.4
        if isinstance(s_exp, (int, float)):
            return str(s_exp)
        
        # remaining forms are lists
        assert isinstance(s_exp, list) 
        assert len(s_exp) > 0    # One might imagine allowing empty tuples, but not allowed in ksc
        head = s_exp[0]

        # Nested exp e.g. ((sin 5)) -> (sin 5)
        if len(s_exp) == 1:
            return self.handle_body(head)

        # List-of-const literals (1 2 3 4)
        # TODO: ksc does not allow this, so either add in ksc, or rm here
        #  Ideally it is best not to add such sugar unless it significantly 
        #  improves efficiency/readability. This feels the wrong side, as it 
        #  just replaces (tuple 1 2 3) with (1 2 3)  
        if isinstance(head, (int, float)):
            assert all(isinstance(se, type(head)) for se in s_exp)
            return [v for v in s_exp]

        joiner = ("\n" + (" " * indent))
        
        # (let ((var expr) ...) body)
        if head == Translator.sym_let:
            let_list = ensure_list_of_lists(s_exp[1])
            let_var_names = [se[0].value() for se in let_list]
            let_exprs = [se[1] for se in let_list]
            return handle_let(
                let_var_names,
                [self.handle_body(se, indent+2) for se in let_exprs],
                self.handle_body(s_exp[2], indent + 2 * len(let_var_names)),
                indent=indent + 2 * len(let_var_names) # inner most indent
            )

        # (tuple e1 .. eN)
        elif head == Translator.sym_tuple:
            tuple_args = [self.handle_body(se, indent+2) for se in s_exp[1:]]
            return "make_tuple({tuple_args})".format(tuple_args=", ".join(tuple_args))

        # (get$m$n t)
        elif head.value().startswith("get$"):
            index = int(head.value().split("$")[1]) - 1
            tuple_name = self.handle_body(s_exp[1], indent+2)
            return "get_tuple_element({index}, {tuple_name})".format(tuple_name=tuple_name, index=index)
        
        # Lambda e.g. (lam (var : type) body)
        elif head == Translator.sym_lam:
            var_name = get_var_name(s_exp[1])
            body = self.handle_body(s_exp[2], indent+2)
            return joiner.join([
                f"(lambda {var_name}:",
                f"  {body}",
                ")"])

        # If-then-else e.g. (if c e1 e2)
        elif head == Translator.sym_if:
            # need to special case if because "if" is a python keyword
            cond, then_branch, else_branch = [self.handle_body(se, indent+2) for se in s_exp[1:]]
            return joiner.join([
                f"if_then_else({cond},",
                f"             lambda: {then_branch},",
                f"             lambda: {else_branch})"
            ])

        # All others are function call e.g. (f e1 .. eN)
        func_name = self.normalize_def_name(_value_to_str(head))
        args = s_exp[1:]
        args_handled = []
        for i, se in enumerate(args):
            try:
                args_handled.append(self.handle_body(se, indent+2))
            except:
                raise ValueError("In {}, failed to handle"
                                 " argument #{}, {}".format(func_name,
                                                            i + 1,
                                                            se))
        return "{func_name}({args})".format(func_name=func_name,
                                            args=", ".join([arg for arg in args_handled]))

    def handle_def(self, s_exp):
        name, _return_type, args, body = s_exp[1:]
        name = _value_to_str(name)
        args = ensure_list_of_lists(args)
        arg_names = [se[0].value() for se in args]
        return Def(name, """
def {name}({args}):
  return {body}
""".format(name=self.normalize_def_name(name),
           args=", ".join(arg_names),
           body=self.handle_body(body)),
                   args_to_sample_values(args))

    def parse_defs(self, string_or_stream):
        defs_to_process = []
        for s_exp in _parse_to_s_exp(string_or_stream):
            def_or_edef = s_exp[0].value()
            if def_or_edef == "edef":
                name = _value_to_str(s_exp[1])
                py_name = self.normalize_def_name(name)
                if py_name in self._built_ins:
                    # if it is built-in no need to edef
                    continue
                edef = EDef(name,
                            py_name,
                            _convert_to_type(s_exp[2]))
                self._edefs[name] = edef
            elif def_or_edef == "def":
                name = _value_to_str(s_exp[1])
                if name.startswith("cost$") or name.startswith("shape$"):
                    # delay so that we read edefs first
                    defs_to_process.append(s_exp)
                else:
                    self._defs.append(self.handle_def(s_exp))
            else:
                print("translate: ignoring unrecognized definition type ", s_exp[0], file=sys.stderr)
        for s_exp in defs_to_process:
            self._defs.append(self.handle_def(s_exp))

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
