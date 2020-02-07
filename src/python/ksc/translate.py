"""Tool for translating Knossos .ks file into Python .py file.
"""
import argparse
from collections import namedtuple
from functools import wraps
import sys

import sexpdata

from ksc.backends import specs

_built_in_edefs = [
    "add",
    "sub",
    "mul",
    "div_ii",
    "div_ff",
    "eq",
    "lt",
    "gt",
    "lte",
    "gte",
    "or_",
    "and_",
    "abs_",
    "max_",
    "neg",
    "to_float_i",
    "build",
    "sumbuild",
    "size",
    "index",
    "fold",
]

def _parse_to_s_exp(string_or_stream):
    return sexpdata.Parser(string_or_stream, nil=None, true="True", false="False").parse()

def get_var_name(s_exp):
    if isinstance(s_exp, sexpdata.Symbol):
        return s_exp.value()
    else:
        return s_exp[0].value()

def _let_to_lambda(let_var_names, let_exprs, body, indent=4):
    joiner = ("\n" + (" " * indent))
    let_var = let_var_names[-1]
    let_expr = let_exprs[-1]
    lambda_expr = joiner.join([
        f"(lambda {let_var}:",
        f"  {body}",
        f")({let_expr})"
    ])
    if len(let_var_names) > 1:
        return _let_to_lambda(let_var_names[:-1],
                              let_exprs[:-1],
                              lambda_expr,
                              indent=indent-2)
    else:
        return lambda_expr

def get_specialized_func_name(name):
    specialized = specs.specialized_functions()
    if name in specialized:
        return specialized[name]
    return name.split("@")[0]

def handle_body(s_exp, indent=2):
    if isinstance(s_exp, sexpdata.Symbol):
        return s_exp.value()
    if isinstance(s_exp, (int, float)):
        return str(s_exp)
    if isinstance(s_exp[0], (int, float)):
        assert all(isinstance(se, type(s_exp[0])) for se in s_exp)
        return [v for v in s_exp]
    joiner = ("\n" + (" " * indent))
    func_name = get_specialized_func_name(_value_to_str(s_exp[0]))
    if func_name == "let":
        let_list = s_exp[1]
        let_var_names = [se[0].value() for se in let_list]
        let_exprs = [se[1] for se in let_list]
        return _let_to_lambda(
            let_var_names,
            [handle_body(se, indent+2) for se in let_exprs],
            handle_body(s_exp[2], indent + 2 * len(let_var_names)),
            indent=indent + 2 * len(let_var_names) # inner most indent
        )
    elif func_name == "tuple":
        tuple_args = [handle_body(se) for se in s_exp[1:]]
        return "({tuple_args})".format(tuple_args=", ".join(tuple_args))
    elif func_name.startswith("get"):
        index = int(func_name.split("$")[1]) - 1
        tuple_name = s_exp[1].value()
        return "{tuple_name}[{index}]".format(tuple_name=tuple_name, index=index)
    elif func_name == "lam":
        var_name = get_var_name(s_exp[1])
        body = handle_body(s_exp[2], indent=indent+2)
        return joiner.join([
            f"(lambda {var_name}:",
            f"  {body}",
            ")"])
    elif func_name == "if":
        # need to special case if because "if" is a python keyword
        cond, then_branch, else_branch = [handle_body(se) for se in s_exp[1:]]
        return joiner.join([f"({then_branch} if {cond} else",
                            f" {else_branch})"])
    elif func_name in ["or", "and", "max", "abs"]:
        # need to special case them because they conflict with python functions
        func_name = func_name + "_"

    args = s_exp[1:]
    args_handled = []
    for i, se in enumerate(args):
        try:
            args_handled.append(handle_body(se))
        except:
            raise ValueError("In {}, failed to handle"
                                " argument #{}, {}".format(func_name,
                                                            i + 1,
                                                            se))
    return "{func_name}({args})".format(func_name=func_name,
                                        args=", ".join([arg for arg in args_handled]))

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

Def = namedtuple("Def", ["name", "str", "sample_args"])

def _value_to_str(name):
    return name if isinstance(name, str) else name.value()

def handle_def(s_exp):
    name, _, args, body = s_exp[1:]
    name = _value_to_str(name)
    arg_names = [se[0].value() for se in args]
    return Def(name,
               """def {name}({args}):
  return {body}
""".format(name=name.split("@")[0], # ignore the type info in the name
           args=", ".join(arg_names),
           body=handle_body(body)),
               args_to_sample_values(args))

def to_def_or_edef(s_exp):
    if s_exp[0].value() == "edef":
        name = _value_to_str(s_exp[1])
        return get_specialized_func_name(name), None
    elif s_exp[0].value() == "def":
        return None, handle_def(s_exp)
    else:
        return None, None

def parse_defs(string_or_stream):
    se_list = _parse_to_s_exp(string_or_stream)
    edefs, defs = zip(*[to_def_or_edef(se) for se in se_list])
    return list(filter(None, edefs)), list(filter(None, defs))

def translate(ks_str, backend, with_main=True):
    edefs, defs = parse_defs(ks_str)
    def_names, def_strs, samples = zip(*defs)
    main_samples = samples[-1]
    main_name = def_names[-1]

    # include built-in functions
    edefs = sorted(set(_built_in_edefs + edefs))
    # For popart, also import _run_model
    if backend == 'popart':
        edefs.append('_run_model')

    # For popart, wrap the main call in _run_model (popart boilerplate)
    main_call = '{main}({main_args})'.format(
        main=main_name,
        main_args=", ".join([k for k, _ in main_samples])
    )
    if backend == 'popart':
        main_call = f'_run_model({main_name})'

    ks_str = '''import numpy as np
from ksc.backends.{backend} import ({edefs}
)

{defs}

defs={{
  {defs_map}
}}
'''.format(backend=backend,
           edefs=",\n  ".join(edefs),
           defs="\n".join(def_strs),
           defs_map=",\n  ".join([f'"{d.name}": {d.name.split("@")[0]}' for d in defs])
    )

    if with_main:
        ks_str += """

def main():
  {sample_args}
  print({main_call})

if __name__ == "__main__":
  main()
""".format(
        sample_args="\n  ".join("{} = {}".format(k, v) for k, v in main_samples),
        main_call=main_call
    )

    return ks_str

def main():
    parser = argparse.ArgumentParser(prog="python -m ksc.translate", description=__doc__)
    parser.add_argument("input_ks_file", type=str)
    parser.add_argument("--backend", choices=["common", "jax", "jax_input_last", "popart"], default="common")
    args = parser.parse_args()

    with open(args.input_ks_file) as f:
        print(translate(f.read(), args.backend))

if __name__ == "__main__":
    sys.exit(main())
