from collections import namedtuple
from functools import wraps
import inspect

import numpy as np

from ksc.type import Type, make_tuple_if_many
from ksc.shape import ShapeType, shape_type_from_object

from ksc.abstract_value import AbstractValue
from ksc.ks_function import KsFunction

from ksc.tracing import node
from ksc.tracing.function import Trace, TraceableFunction



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
    return result


ProtoFunction = namedtuple("ProtoFunction", ["name", "return_type", "arg_names", "is_edef", "is_builtin", "shape_def", "cost_def"])

def get_or_trace_function(f, original_args):
    global _jitted
    arg_types = tuple(arg.shape_type.type for arg in original_args)

    # allow the name to be specialized during f.trace()
    if "{0}" not in f.name:
        key = (f.name, arg_types) # only key on arg types (not on shape)
        if key in _jitted:
            return _jitted[key]

    inputs = [node.Node(n, arg.shape, arg.type, data=arg._data) for n, arg in zip(f.arg_names, original_args)]

    # trace the function
    trace = f.trace(*inputs)
    # the name is specialized
    key = (f.name, arg_types)

    print(f"Traced {f.name}: root {trace.body}")
    f = ProtoFunction(
        f.name,
        trace.shape_type.type,
        f.arg_names,
        f.is_edef,
        f.is_builtin,
        f.shape_def if hasattr(f, "shape_def") else None,
        f.cost_def if hasattr(f, "cost_def") else None
    )
    _jitted[key] = JittedFunction.from_trace(f, trace)
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

def jit_and_execute_anonymous_function(body, backend):
    shape_type = body.shape_type
    arg_names, shape_types, values = lift_constants(body)
    trace = Trace(body, shape_type, shape_types)
    name = f"_anonymous"
    f = ProtoFunction(name, trace.shape_type.type, arg_names, False, False, None, None)
    jitted = JittedFunction.from_trace(f, trace)
    # create a function node (which will be connected to
    # the jitted function through the origin attribute)
    arg_nodes = [node.Node(n, st.shape, st.type, data=v) for n, st, v in zip(arg_names, shape_types, values)]
    _ = node.Node(jitted.name, shape_type.shape, shape_type.type, children=arg_nodes, jitted=jitted)
    if backend == "abstract":
        # wrap the concrete values in AbstractValue
        values = [AbstractValue.from_data(value) for value in values]
    v = jitted(*values, backend=backend)
    st = shape_type_from_object(v)
    value = node.Node("_identity", st.shape, st.type, data=v)
    value._children = [jitted.origin]
    return value

class VarNameGenerator:
    def __init__(self, prefix="tmpvar__"):
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
    type = shape_type_from_object(value).type
    if type.is_tuple:
        args = " ".join([format_constant(v) for v in value])
        return f"(tuple {args})"
    elif type.is_tensor:
        return f"(Vec_init " + " ".join(format_constant(v) for v in value) + ")"
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
        arg_types = make_tuple_if_many(arg_types)
        return f"(edef {def_name} {f.return_type} {arg_types})"

    var_name_generator = VarNameGenerator()
    return_type = nodes[-1].shape_type.type
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
                    f"(let ({var_name} {current_expr})",
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
                st = n.shape_type
                new_node = node.Node("", st.shape, st.type, n.data) # disconnect from the trace
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
                st = node_prop(*[cf(args) for cf in child_functions])
                return node.Node("", st.shape, st.type)
            return prop
    prop = helper(trace.body) # prop :: args -> Node
    return lambda *args: prop(args).shape_type

class JittedFunction(KsFunction):
    def __init__(self, name, return_type, arg_name_types, ks_str, is_edef, is_builtin, called_functions, shape_prop_function):
        super().__init__(
            name,
            return_type,
            arg_name_types,
            ks_str,
            is_edef,
            is_builtin
        )
        self._called_functions = called_functions
        self._shape_prop_function = shape_prop_function
        self._origin = None

    @staticmethod
    def from_trace(f, trace):
        # sort out dependencies
        nodes = topological_sort(trace.body)
        called_functions = {}
        for node in nodes:
            if node.jitted is not None:
                key = (node.name, tuple(node.jitted.arg_types))
                if key not in called_functions:
                    called_functions[key] = node.jitted
        # If _anonymous function is called in _anonymous, add appendix '_{index}'
        # to avoid name clash
        name = f.name
        called_func_names = [n for n, _arg_types in called_functions.keys()]
        if name in called_func_names and name.startswith("_anonymous"):
            index = max(int(n.split("_")[2]) if n.count("_") == 2 else 0
                for n in called_func_names
                if n.startswith("_anonymous")) + 1
            name = f"_anonymous_{index}"
        # append arg types
        arg_types = tuple(t.type for t in trace.arg_shape_types)
        if f.is_edef:
            if f.shape_def is not None:
                called_functions[(f.shape_def.name, arg_types)] = f.shape_def
            if f.cost_def is not None:
                called_functions[(f.cost_def.name, arg_types)] = f.cost_def

        f = ProtoFunction(name, *f[1:]) # for compute_ks_str
        return JittedFunction(
            name,
            f.return_type,
            [(n, t) for n, t in zip(f.arg_names, arg_types)],
            compute_ks_str(f, nodes, arg_types),
            f.is_edef,
            f.is_builtin,
            called_functions,
            compose_shape_propagation_function(f, trace)
        )

    def all_called_functions(self, seen=None):
        """ Returns two ordered dictionaries before and after that contains
            functions (instances of JittedFunction) called by this function.
            `before` contains all the functions that need to be defined before
            this function. `after` contains cost$ and shape$ functions associated
            with this function.
        """
        before = {}
        after = {}
        if seen is None:
            seen = set()
        for key, called in self._called_functions.items():
            if len(key[0]) > 0 and key not in seen:
                called_function = called() if isinstance(called, JittedFunctionFromCall) else called
                seen.add(key)
                inner_before, inner_after = called_function.all_called_functions(seen)
                before.update(inner_before) # add functions called by called first
                if not isinstance(called, JittedFunctionFromCall):
                    # if called is not cost$ or shape$, add the called function itself
                    before[key] = called
                else:
                    after[key] = called_function
                before.update(inner_after)  # cost$ and shape$
        return before, after

    def combined_ks_str(self):
        if len(self._ks_str) == 0:
            # built-in
            return ""
        before, after = self.all_called_functions()
        print(f"All called functions for {self.name}: {list(before.keys()) + list(after.keys())}")
        before_functions = [
            called.ks_str
            for called in before.values()
            if len(called.ks_str) > 0
        ]
        after_functions = [
            called.ks_str
            for called in after.values()
            if len(called.ks_str) > 0
        ]
        return "\n\n".join(
            before_functions + [self._ks_str] + after_functions
        )

    def shape_type(self, *args):
        return self._shape_prop_function(*args)

    @property
    def origin(self):
        return self._origin

    @origin.setter
    def origin(self, node):
        self._origin = node

def trace(f, name=None):
    if name is None:
        name = f.__name__
    class F(TraceableFunction):
        is_edef = False
        is_builtin = False
        def __init__(self):
            # This does not work for variable length arguments
            arg_names = inspect.getfullargspec(f).args
            super().__init__(name, arg_names=arg_names)
        def forward(self, *args):
            return f(*args)
    @wraps(f)
    def wrapper(*args):
        return F()(*args)
    wrapper.__name__ = name
    wrapper.__qualname__ = f"{name} [traceable]"
    return wrapper

class JittedFunctionFromCall:
    """
    This class stores a traceable function and arguments so that
    it can be called later to produce the JittedFunction. It is
    useful for avoiding a circular dependency between two functions
    that needs to be compiled.

    TODO: better name
    """
    def __init__(self, name, f, args):
        self.name = name
        self.f = trace(f, name)
        self.args = args
    def __call__(self):
        node = self.f(*self.args)
        return node.creator._jitted

def make_edef(name, arg_names, shape_prop_function, traceable_shape_function=None, traceable_cost_function=None):
    """
    Declare a function/op/node for tracing and the abstract interpreter

        (def name 
            (x y)  ; arg_names
            ... )

    -- shape_prop_function 
    Should be a python function taking AbstractValues, 
    and returning a ShapeType of the result, e.g.
        def mat_vec_mul_shape_propagate(M, v):
            return ShapeType(M.shape.dims[0], v.type)

    The algebra of these shape classes is:
        Shape of scalar is      ScalarShape (constant initialized to ())
        Shape of tuple is       tuple of Shape
        Shape of tensor is      TensorShape(dims, element_shape)

    Note that tensor shape is assumed non-jagged for now.  When we switch to
    jagged, tensor shape will be tensor of shapes, using a constvec for efficient
    representation of non-jagged tensors

    -- traceable_shape_function
    Should be an appropriate Node for shape$name, returning a shape.
    The shape algebra restricted to KS datatypes is as follows:
        shape of scalar   is (tuple)                              ;; empty tuple
        shape of tensor   is (tuple (tuple dim1 dim2 ... dimN) element_shape)
        shape of tuple    is (tuple shape1 shape2 .. shapeN)
    The supplied body should be as if we had parsed code such as:
    (def shape$mat_vec_mul ((x : Tensor 2 Float) (y : Tensor 1 Float))
        (tuple (tuple (get$1$2 (size x)))  ;; shapes are more strict than "size", use singleton tuple for 1D vector  
               (tuple)))   ;; and include the element shape, here shape of scalar
    This will normally be created using the python node creators in ksc.tracing.core, e.g.
    shape_mat_vec_mul = lambda x,y: core.make_tuple(core.make_tuple(x.shape_program[0][0]), y.shape_program[1]),
    
    -- traceable_cost_function
    Should be an appropriate Node for cost$name, returning a float.
    (def cost$name (x y)
        ...)    
    """

    class F(TraceableFunction):
        is_edef = True
        is_builtin = False
        def __init__(self):
            super().__init__(name, arg_names=arg_names)
        def trace(self, *args):
            st = shape_prop_function(*args)
            self.shape_def = (
                JittedFunctionFromCall(f"shape${name}", traceable_shape_function, args)
                if traceable_shape_function is not None else None
            )
            self.cost_def = (
                JittedFunctionFromCall(f"cost${name}", traceable_cost_function, args)
                if traceable_cost_function is not None else None
            )
            body = node.Node(
                name=name,
                shape=st.shape,
                type=st.type,
                children=args,
                shape_prop_function=shape_prop_function)
            shape_types = tuple(arg.shape_type for arg in args)
            return Trace(body, ShapeType(st.shape, st.type), shape_types)
    d = {"F": F}
    arg_names_str = ", ".join(arg_names)
    exec(f"def wrapped({arg_names_str}): return F()({arg_names_str})", d)
    wrapped = d["wrapped"]
    wrapped.__name__ = name
    wrapped.__qualname__ = f"{name} [edef]"
    return wrapped
