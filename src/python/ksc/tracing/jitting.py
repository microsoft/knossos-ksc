from collections import namedtuple
from functools import wraps
import inspect

import numpy as np

from ksc.ks_function import KsFunction
from ksc.type import Type
from ksc.tracing import node
from ksc.tracing.function import Trace, TraceableFunction
from ksc import utils
from ksc.utils import ShapeType

_jitted = {}

# from
# https://stackoverflow.com/questions/47192626/deceptively-simple-implementation-of-topological-sorting-in-python
def topological_sort(final_node):
    result = []
    seen = set()

    def recursive_helper(node):
        for child in node.children:
            if child not in seen:
                seen.add(child)
                recursive_helper(child)
        result.append(node)

    recursive_helper(final_node)
    print([n.name for n in result])
    return result


ProtoFunction = namedtuple("ProtoFunction", ["name", "return_type", "arg_names", "is_edef", "is_builtin"])

def get_or_trace_function(f, original_args):
    global _jitted
    arg_types = tuple(arg.shape_type.type for arg in original_args)
    key = (f.name, arg_types) # only key on arg types (not on shape)
    if key in _jitted:
        return _jitted[key]

    inputs = [node.Node(n, *arg.shape_type, data=arg._data) for n, arg in zip(f.arg_names, original_args)]

    # trace the function
    trace = f.trace(*inputs)

    print(f"Traced {f.name}: root {trace.body}")
    f = ProtoFunction(
        f.name,
        trace.shape_type.type,
        f.arg_names,
        f.is_edef,
        f.is_builtin
    )
    _jitted[key] = JittedFunction(f, trace)
    return _jitted[key]

def lift_constants(body):
    nodes = topological_sort(body)
    arg_names = []
    shape_types = []
    values = []
    name_generator = VarNameGenerator("arg")
    for node in nodes:
        if is_constant(node):
            name = name_generator()
            node._name = name
            arg_names.append(name)
            shape_types.append(node.shape_type)
            values.append(node.data)
    return arg_names, shape_types, values

def jit_and_execute_annonymous_function(body, backend):
    shape_type = body.shape_type
    arg_names, shape_types, values = lift_constants(body)
    trace = Trace(body, shape_type, shape_types)
    name = f"_anonymous"
    f = ProtoFunction(name, trace.shape_type.type, arg_names, False, False)
    jitted = JittedFunction(f, trace)
    # create a function node (which will be connected to
    # the jitted function through the origin attribute)
    arg_nodes = [node.Node(n, s, t, data=v) for n, (s, t), v in zip(arg_names, shape_types, values)]
    _ = node.Node(jitted.name, shape_type.shape, shape_type.type, children=arg_nodes, jitted=jitted)
    v = jitted(*values, backend=backend)
    s, t = utils.shape_type_from_object(v)
    value = node.Node("_identity", s, t, data=v)
    value._children = [jitted.origin]
    return value

class VarNameGenerator:
    def __init__(self, prefix="v"):
        self.index = 0
        self.prefix = prefix

    def __call__(self):
        vname = f"{self.prefix}{self.index}"
        self.index += 1
        return vname

def format_def_name(name):
    if "[" in name or "]" in name:
        return f'"{name}"'
    else:
        return name

def format_arg_list(arg_names, arg_types):
    return " ".join([f"({n} : {t})" for n, t in zip(arg_names, arg_types)])

def format_constant(value):
    _, type = utils.shape_type_from_object(value)
    if type.kind == "Tuple":
        args = " ".join([format_constant(v) for v in value])
        return f"(tuple {args})"
    elif type.kind == "Vec":
        n = len(value)
        template = f"(build {n} (lam (i : Integer) {{body}}))"
        for i in range(n - 1):
            template = template.format(
                body=f"(if (eq i {i}) {format_constant(value[i])} {{body}})"
            )
        return template.format(body=format_constant(value[-1]))
    else:
        return str(value)

def is_constant(node):
    return len(node.children) == 0 and node.data_ready

def compute_ks_str(f, nodes, arg_types):
    if f.is_builtin:
        return ""
    def_name = format_def_name(f.name)
    if f.is_edef:
        # this is an edef
        arg_types = " ".join([str(t) for t in arg_types])
        return f"(edef {def_name} {f.return_type} ({arg_types}))"

    var_name_generator = VarNameGenerator()
    _, return_type = nodes[-1].shape_type
    arg_name_types = format_arg_list(f.arg_names, arg_types)
    template = "\n".join(
        [f"(def {def_name} {return_type} ({arg_name_types})",
            "  {body}",
            ")"]
    )
    # computed_exprs should include only arguments and not constants
    computed_exprs = {n: n.name for n in nodes if len(n.children) == 0 and n.name in f.arg_names}
    # anything that takes inputs or not in the arguments
    nodes_to_process = [n for n in nodes if len(n.children) > 0 or n.name not in f.arg_names]
    indent = 2
    while len(nodes_to_process) > 0:
        current = nodes_to_process.pop(0)
        args = []
        for v in current.children:
            if v in computed_exprs:
                args.append(computed_exprs[v])
            else:
                cur_pos = nodes.index(current)
                arg_pos = nodes.index(v)
                raise ValueError(f"{v} appeared as an argument for {current.name}"
                                 f" in position {cur_pos}, which is earlier than it is"
                                 f" computed at position {arg_pos}."
                                 f" computed_exprs={computed_exprs}")
        if is_constant(current):
            # constant
            current_expr = format_constant(current.data)
        elif current.name == "_identity":
            assert len(args) == 1
            current_expr = args[0]
        else:
            args_str = " ".join(args)
            current_expr = f"({format_def_name(current.name)} {args_str})"
        # if more than one users, use let
        if len(current.users) > 1:
            joiner = "\n" + (" " * indent)
            var_name = var_name_generator()
            computed_exprs[current] = var_name
            # Update the template
            template = template.format(
                body=joiner.join([
                    f"(let (({var_name} {current_expr}))",
                    "{body}",
                    ")"
                ])
            )
            indent += 2
        else:
            computed_exprs[current] = current_expr
    return template.format(body=computed_exprs[nodes[-1]])

def _check_arg_names_unique(arg_names):
    seen = set()
    for arg_name in arg_names:
        if arg_name in seen:
            raise ValueError(f"Duplicated argument name {arg_name}")
        seen.add(arg_name)

def compose_shape_propagation_function(f, trace):
    _check_arg_names_unique(f.arg_names)
    arg_name_to_index = {arg_name: i for i, arg_name in enumerate(f.arg_names)}
    def helper(n):
        if len(n.children) == 0:
            if n.name in arg_name_to_index:
                return lambda args: args[arg_name_to_index[n.name]]
            else:
                # constant
                s, t = n.shape_type
                new_node = node.Node("", s, t, n.data) # disconnect from the trace
                return lambda args: new_node
        elif n.name == "_identity":
            assert len(n.children) == 1
            cf = helper(n.children[0])
            return lambda args: cf(args)
        else:
            try:
                node_prop = n.shape_prop_function
            except:
                print(f"During the composition of shape propagation function for {f.name},"
                      f" found node {n} without a shape_prop_function.")
                raise
            child_functions = [helper(c) for c in n.children]
            def prop(args):
                s, t = node_prop(*[cf(args) for cf in child_functions])
                return node.Node("", s, t)
            return prop
    prop = helper(trace.body) # prop :: args -> Node
    return lambda *args: prop(args).shape_type

class JittedFunction(KsFunction):
    def __init__(self, f, trace):
        # sort out dependencies
        nodes = topological_sort(trace.body)
        self._called_functions = {}
        for node in nodes:
            if node.name not in self._called_functions and node.jitted is not None:
                self._called_functions[node.name] = node.jitted
        # If _anonymous function is called in _anonymous, add appendix '_{index}'
        # to avoid name clash
        name = f.name
        called_func_gen_names = [n.split("@")[0] for n in self._called_functions.keys()]
        if name in called_func_gen_names and name.startswith("_anonymous"):
            index = max(int(n.split("_")[2]) if n.count("_") == 2 else 0
                for n in called_func_gen_names
                if n.startswith("_anonymous")) + 1
            name = f"_anonymous_{index}"
        # append arg types
        arg_types = [t for _, t in trace.arg_shape_types]
        arg_type_strings = "".join([t.shortstr() for t in arg_types])
        name = f"{name}@{arg_type_strings}"

        f = ProtoFunction(name, *f[1:])
        super().__init__(
            name,
            f.return_type,
            [(n, t) for n, t in zip(f.arg_names, arg_types)],
            compute_ks_str(f, nodes, arg_types),
            f.is_edef,
            f.is_builtin
        )
        self._shape_prop_function = compose_shape_propagation_function(f, trace)
        self._origin = None

    def all_called_functions(self):
        ret = {}
        for key, called in self._called_functions.items():
            if len(key) > 0:
                ret.update(called.all_called_functions())
            if key not in ret:
                ret[key] = called
        return ret

    def combined_ks_str(self):
        if len(self._ks_str) == 0:
            # built-in
            return ""
        all_called_functions = self.all_called_functions()
        called_functions = [called.ks_str for called in all_called_functions.values()]
        called_functions = filter(lambda x: len(x) > 0, called_functions)
        return "\n\n".join(
            list(called_functions) + [self._ks_str]
        )

    def shape_type(self, *args):
        return self._shape_prop_function(*args)

    @property
    def origin(self):
        return self._origin

    @origin.setter
    def origin(self, node):
        self._origin = node

def trace(f):
    class F(TraceableFunction):
        is_edef = False
        is_builtin = False
        def __init__(self):
            arg_names = inspect.getfullargspec(f).args
            super().__init__(f.__name__, arg_names=arg_names)
        def forward(self, *args):
            return f(*args)
    @wraps(f)
    def wrapper(*args):
        return F()(*args)
    wrapper.__qualname__ = f"{f.__name__} [traceable]"
    return wrapper

def memoize(f):
    memo = {}
    def wrapper(name, arg_names, type_prop_function):
        key = (name, tuple(arg_names))
        if key not in memo:
            memo[key] = f(name, arg_names, type_prop_function)
        return memo[key]
    return wrapper

@memoize
def make_edef(name, arg_names, shape_prop_function):
    class F(TraceableFunction):
        is_edef = True
        is_builtin = False
        def __init__(self):
            super().__init__(name, arg_names=arg_names)
        def trace(self, *args):
            shape, type = shape_prop_function(*args)
            body = node.Node(
                name=name,
                shape=shape,
                type=type,
                children=args,
                shape_prop_function=shape_prop_function)
            shape_types = tuple(arg.shape_type for arg in args)
            return Trace(body, ShapeType(shape, type), shape_types)
    d = {"F": F}
    arg_names_str = ", ".join(arg_names)
    exec(f"def wrapped({arg_names_str}): return F()({arg_names_str})", d)
    wrapped = d["wrapped"]
    wrapped.__name__ = name
    wrapped.__qualname__ = f"{name} [edef]"
    return wrapped
