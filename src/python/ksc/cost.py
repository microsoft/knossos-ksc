from ksc.abstract_value import ExecutionContext
from ksc.utils import translate_and_import

def shape_from_type(arg_type, assumed_vector_size=100):
    if arg_type.is_tuple:
        return tuple(shape_from_type(c) for c in arg_type.children)
    if arg_type.is_tensor:
        child_shape = shape_from_type(arg_type.children[1])
        return (assumed_vector_size,) + child_shape
    else:
        return ()

def compute_cost(ks_str, def_name, args, exec_context=None, aggregation_option="default"):
    m = translate_and_import(__file__, ks_str, "abstract")
    if def_name in m.defs:
        f = m.defs[def_name]
    else:
        f = m.__dict__[def_name]
    with (ExecutionContext() if exec_context is None else exec_context) as ctx:
        _ = f(*args)
    if aggregation_option == "default":
        return ctx.costs[None]
    elif aggregation_option == "all":
        return ctx.costs
    elif callable(aggregation_option):
        return aggregation_option(ctx.costs)
    else:
        raise ValueError(f"Don't know how to handle option {aggregation_option}")
