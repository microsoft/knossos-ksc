# fmt: off
# mypy: ignore-errors
import re
import sexpdata
from typing import Mapping, Optional, Sequence, Tuple

from pyrsistent import pmap

from rlo.expression import Expression, EF
from rlo.expression_util import ExprWithEnv, deep_copy_with_types, NamedExprWithEnv, SymtabAndDefs
from rlo.costs import elementwise_cost
from ksc.type import Type
from rlo import utils

_default_funcs: Sequence[Tuple[str, int, Type]] = [
    ("add$ii", 1, Type.Lam(Type.Tuple(Type.Integer, Type.Integer), Type.Integer)),
    ("add$ff", 1, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Float)),
    ("add$vfvf", elementwise_cost(Type.Tensor(1,Type.Float), 1), # cost of add
        Type.Lam(Type.Tuple(Type.Tensor(1,Type.Float), Type.Tensor(1,Type.Float)),Type.Tensor(1,Type.Float))),

    ("sub$ii", 1, Type.Lam(Type.Tuple(Type.Integer, Type.Integer), Type.Integer)),
    ("sub$ff", 1, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Float)),
    ("mul$ii", 1, Type.Lam(Type.Tuple(Type.Integer, Type.Integer), Type.Integer)),
    ("mul$ff", 2, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Float)),
    # It's not clear the two ideas of division are similar enough to warrant the same grouping.
    ("div$ii", 1, Type.Lam(Type.Tuple(Type.Integer, Type.Integer), Type.Integer)),
    ("div$ff", 2, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Float)),
    ("exp$f", 4, Type.Lam(Type.Float, Type.Float)),
    ("log$f", 4, Type.Lam(Type.Float, Type.Float)),
    ("eq$ii", 1, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Bool)),
    ("eq$ff", 1, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Bool)),
    ("gt$ii", 1, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Bool)),
    ("gt$ff", 1, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Bool)),
    ("gte$ii", 1, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Bool)),
    ("gte$ff", 1, Type.Lam(Type.Tuple(Type.Float, Type.Float), Type.Bool)),
    ("to_float", 1, Type.Lam(Type.Integer, Type.Float)),
    ("$ranhashdoub", 1, Type.Lam(Type.Integer, Type.Float)),
]
default_symtab_and_defs = SymtabAndDefs(
    symtab = pmap({
        name: typ for name, _cost, typ in _default_funcs
    }),
    defs = pmap({
        "cost$" + name: EF.Lam(Expression.Variable("_", type=typ.lam_arg_type), cost)
        for name, cost, typ in _default_funcs
    })
)
default_symtab = default_symtab_and_defs.symtab

_select_regex = re.compile("get\\$([0-9]+)\\$([0-9]+)")

def _convert_to_type(se, allow_any=False, allow_implicit_tuple=False):
    """ Converts an S-Expression representing a type, like (Vec Float) or (Tuple Float (Vec Float)),
        into a Type object, e.g. Type.Tensor(1,Type.Float) or Type.Tuple(Type.Float, Type.Tensor(1,Type.Float)).

        If allow_implicit_tuple is true, also converts a list of types into a Tuple, e.g.
        (Float (Vec Float)) becomes Type.Tuple(Type.Float, Type.Tensor(1,Type.Float)), i.e. as if
        the S-Expression began with an extra "Tuple".
    """
    while isinstance(se, list) and len(se)==1:
        se=se[0] # Discard ((pointless)) brackets
    if isinstance(se, sexpdata.Symbol):
        if se.value() == "Any" and allow_any: return None
        return Type(se.value())
    if isinstance(se, list) and len(se)>0:
        if isinstance(se[0], sexpdata.Symbol):
            sym = se[0].value()
            if sym == "Tensor" and len(se) == 3:
                assert se[1] == 1, "Only 1D 'Tensor's ('Vec's) supported"
                return Type.Tensor(1, _convert_to_type(se[2]))
            children = [_convert_to_type(s) for s in se[1:]]
            if sym == "Vec" and len(se)==2:
                return Type.Tensor(1, utils.single_elem(children))
            if sym == "Tuple":
                return Type.Tuple(*children)
            # Fall through in case it's a list of types with allow_implicit_tuple.
        if allow_implicit_tuple:
            return Type.Tuple(*[_convert_to_type(s) for s in se])
    raise ValueError("Did not know how to parse type {}".format(se))

def _convert_param(se, allow_any=False):
    if len(se) >= 3 and isinstance(se[0], sexpdata.Symbol) and se[1] == sexpdata.Symbol(":"):
        return Expression.Variable(se[0].value(), _convert_to_type(se[2:], allow_any))
    raise ValueError("Expected (var : type maybe unbracketed) as parameter but got {}".format(sexpdata.dumps(se)))

def _convert_to_variable(se, type=None):
    n = se.value()
    # Check it's a reasonable variable name. Note isidentifier() is FALSE for python keywords.
    if not n.replace('$','_').isidentifier() or n in Expression.node_types:
        raise ValueError("Cannot use {} as a variable name".format(n))
    return Expression.Variable(n, type=type)

def _convert_to_expr(se):
    if isinstance(se, sexpdata.Symbol):
        name = se.value()
        if name not in Expression.node_types:
            return _convert_to_variable(se)
        # Fall through... will create an Expression with no children, e.g. unary tuple
    if isinstance(se, bool) or isinstance(se, float) or isinstance(se, int):
        return Expression.Constant(se)
    if not isinstance(se, list):
        raise ValueError("Don't know how to parse {}".format(se))
    if se[0] == sexpdata.Symbol("build") or se[0] == sexpdata.Symbol("sumbuild"):
        if len(se) != 3 or not isinstance(se[2], list) or se[2][0] != sexpdata.Symbol("lam"):
            raise ValueError("({} n (lam x b)) and ({} n (lam (x : Integer) b)) are only supported forms of {}; {} did not match".format(
                *([se[0].value()]*3), sexpdata.dumps(se)))
        _, var, body = se[2]
        if isinstance(var, list) and var[1:] == [sexpdata.Symbol(":"), sexpdata.Symbol("Integer")]:
            var = _convert_param(var)
        else:
            # Also support build (lam x body) without : Integer
            var = _convert_to_variable(var, Type.Integer)
        limit = _convert_to_expr(se[1])
        body = _convert_to_expr(body)
        return Expression(se[0].value(), [limit, var, body])    
    #allows to parse expressions in the format (let ((var1 (assign_to_var1)) (var2 (assign_to_var2))...) (body_containing_var1_var2_...))
    if se[0] == sexpdata.Symbol("let") and len(se) == 3:
        if not isinstance(se[1], list):
            raise ValueError("Bad let expression - binds in brackets (let (x val) body), or old (let x val body), not {}".format(sexpdata.dumps(se)))
        body = se[2]
        if isinstance(se[1][0], list):
            for var, val in reversed(se[1]): # construct innermost first
                se = [sexpdata.Symbol("let"), var, val, body]
                body = se
        else: #for a single variable ks supports format (let (var1 (assign_to_var1)) (body_containing_var1_)) in addition to (let ((var1 (assign_to_var1))) (body_containing_var1_))
            se = [sexpdata.Symbol("let"), se[1][0], se[1][1], body]

    if se[0] == sexpdata.Symbol("lam"): # Support direct syntax, rather than defs, for tests
        if len(se) != 3: raise ValueError("Lambdas must be (lam (var : type) body) not {}".format(sexpdata.dumps(se)))
        return EF.Lam(_convert_param(se[1]), _convert_to_expr(se[2]))

    # The four-child form (let x arg body), is supported below without special treatment
    parsed_args = [_convert_to_expr(a) for a in se[1:]]
    if isinstance(se[0], sexpdata.Symbol):
        sym = se[0].value()
        select_indices = _select_regex.match(sym)
        if select_indices is not None:
            if len(se) != 2:
                raise ValueError("Select takes only one argument, which must produce a tuple, not {}".format(se))
            ksc_idx = int(select_indices.group(1)) # Note ksc indices are 1-based
            # Expression constructor checks validity of idx vs size.
            return EF.Select(parsed_args[0], ksc_idx - 1, size=int(select_indices.group(2)))
        if sym == "neg":
            assert len(parsed_args) == 1
            return Expression.Constant(0.0) - parsed_args[0]
        if sym == "lt":
            l,r = parsed_args
            return Expression("gt", [r, l])
        if sym == "lte":
            l,r = parsed_args
            return Expression("gte", [r, l])
        if sym == "or":
            l,r = parsed_args
            return EF.If(l, Expression.Constant(True), r)
        if sym == "and":
            l,r = parsed_args
            return EF.If(l, r, Expression.Constant(False))
        # Not a special case. Use node_types understood by Expression constructor.
        # (note, this includes apply and select, which are *not* part of ksc syntax)
        if sym in Expression.node_types:
            try:
                return Expression(sym, parsed_args)
            except Exception as e:
                raise ValueError("Bad input {}".format(se)) from e
    # Fall through. Either the first component is not a symbol, or it's not recognized as a node-type.
    lhs = _convert_to_expr(se[0])
    if len(parsed_args) == 0:
        return lhs
    # So, the first component must be an expression/variable that evaluates to a lambda
    return EF.Apply(lhs, EF.Tuple(*parsed_args) if len(parsed_args)>1 else parsed_args[0])

def _convert_params(se, allow_any=False):
    if len(se)>1 and se[1] == sexpdata.Symbol(":"):
        # format (var1 : type1) rather than ((var1 : type1))
        args = [se]
    else:
        # format ((var1 : type1) (var2 : type)...)
        args = se
    return [_convert_param(a, allow_any) for a in args]

def parse_defs_with_symtab(
    string_or_stream,
    symtab_and_defs: Optional[SymtabAndDefs] = None
) -> Tuple[
    Sequence[Tuple[str, Expression]],
    SymtabAndDefs
]:
    """ Parses s-expressions from the .kso file. This consists only of function definitions
        and edefs, and cannot have standalone expressions.
        Functions must be in the format: (def func_name func_return_type args body).
        Edefs must be (edef name return_type arg_types).

        Return value consists of:
           * A list of tuples (name, Expression) for each def (the Expression being a chain of let+lam much as parse_defs below,
                 but Expressions not wrapped into ExprWithEnv's), excluding cost$ functions
           * A SymtabAndDefs with the type of each def/edef and the body of each cost$ function
    """
    se_list = _parse_to_s_exp(string_or_stream)
    func_list: Sequence[Tuple[str, Expression]] = [] # Preserve order, individual Lam's
    def_list: Sequence[Tuple[str, Expression]] = [] # Nested chains of let+lam
    # Copy the initial symtab+defs so we don't mutate that passed in
    if symtab_and_defs is None:
        symtab_and_defs = default_symtab_and_defs
    symtab_and_defs = SymtabAndDefs(
        symtab = dict(symtab_and_defs.symtab),
        defs = dict(symtab_and_defs.defs)
    )

    for se in se_list:
        se_def_or_edef= se[0].value()
        se_name = se[1].value()

        if se_def_or_edef not in ["def", "edef"]:
            raise ValueError("Cannot have a standalone expression in a .kso file!")

        #TODO - Simon and Tom suggested ignoring main - check
        if se_def_or_edef == "def" and se_name != "main":
            args = se[3]
            func_body = se[4]

            if len(se) != 5:
                raise ValueError("Unsupported def format! Length of passed function is ", len(se), ", while only 5 is allowed!")
            if not isinstance(args, list) or (isinstance(func_body, list) and len(func_body)==0):
                raise ValueError("Unsupported def format! Args must be a list, func_body - non-empty list or a single const/var.")

            def_name = se[1].value()
            # The *declared* ret_type can come as a single (i.e. float) or tuple like (i.e. (Vec float)) element.
            ret_type = _convert_to_type(se[2])
            lambda_vars = _convert_params(args)
            lambda_body = _convert_to_expr(func_body)

            if len(lambda_vars) == 1:
                lambda_var = lambda_vars[0]
            else:
                # Multi-argument def. Convert to a function of one argument of tuple type.
                tuple_val = EF.Tuple(*lambda_vars)
                lambda_var = Expression.new_var(tuple_val, type = Type.Tuple(*[lv.type for lv in lambda_vars]))

                for i, v in enumerate(lambda_vars):
                    lambda_body = EF.Let(v, EF.Select(lambda_var, i, size=len(lambda_vars)), lambda_body)

            func = EF.Lam(lambda_var, lambda_body)
            declared_type = Type.Lam(lambda_var.type, ret_type)
            symtab_and_defs.symtab[def_name] = declared_type
            # The expressions to optimize, are everything that's not a cost$ function
            if def_name.startswith("cost$"):
                symtab_and_defs.defs[def_name] = deep_copy_with_types(func, symtab_and_defs.symtab)
            else:
                func_list.append((def_name, func))
                func = _build_nested(func_list)
                typed_expr = deep_copy_with_types(func, symtab_and_defs.symtab)
                if typed_expr.type != declared_type:
                    raise ValueError(f"Function {def_name} has declared type {declared_type} but actually produces type {typed_expr.type}")
                def_list.append((def_name, typed_expr))
        elif se_def_or_edef == "edef":
            if len(se) != 4 or not isinstance(se[3], list):
                raise ValueError("Expected (edef name ret_type (arg_types)) not {}".format(se))
            edef_name = se[1].value()
            func_type = Type.Lam(_convert_to_type(se[3], allow_implicit_tuple=True), _convert_to_type(se[2]))
            if edef_name in symtab_and_defs.symtab:
                raise ValueError(f"Edef {edef_name} repeats built-in" if edef_name in default_symtab else f"Repeated edef {edef_name}")
            symtab_and_defs.symtab[edef_name] = func_type

    return (def_list, symtab_and_defs)

def parse_defs(string_or_stream) -> Sequence[NamedExprWithEnv]:
    """
        Returns a NamedExprWithEnv for each function other than main or cost$ functions,
            in the order present in the input (.kso) file.
            The sequence can easily be passed to dict() constructor if required.

        functions become a nested chain of let f1=lam... in let f2=lam.... in f2.
        This supports only .kso files where functions go in increasing order of dependency,
        i.e. first function is a simple function, second function is a
        function that can be at most a composition with the function above, third function is a function
        that can be at most a composition with the two functions above etc.
        """
    expr_list, symtab_and_defs = parse_defs_with_symtab(string_or_stream)
    return [NamedExprWithEnv(name, ExprWithEnv(expr, symtab_and_defs)) for name, expr in expr_list]

def _build_nested(func_list):
    """ func_list is a list of pairs (function_name, lambda) """
    if len(func_list) == 0:
        raise ValueError("Must have at least one function")
    def_name, body = func_list[0]
    let_rhs = def_name if len(func_list)==1 else _build_nested(func_list[1:])
    return EF.Let(def_name, body, let_rhs)

def _parse_to_s_exp(string_or_stream):
    return sexpdata.Parser(string_or_stream, nil=None, true="true", false="false").parse()

def parse_expr(string_or_stream) -> Expression:
    return _convert_to_expr(_parse_to_s_exp(string_or_stream))

def parse_rule(string_or_stream) -> Tuple[str, Mapping[str,Type], Expression, Expression]:
    """ Parse a (rule name (args) lhs rhs). """
    s_exp = utils.single_elem(_parse_to_s_exp(string_or_stream))
    # Check that we have something beginning with "rule" and a name
    if len(s_exp) != 5 or s_exp[0] != sexpdata.Symbol("rule") or not isinstance(s_exp[1], sexpdata.Symbol):
        raise ValueError(f"Not a rule with args: {s_exp}")
    name = s_exp[1].value()
    args = {v.name: v.type for v in _convert_params(s_exp[2], allow_any=True)}
    lhs = _convert_to_expr(s_exp[3])
    rhs = _convert_to_expr(s_exp[4])
    return (name, args, lhs, rhs)

def remove_multi_arg_wrappings(body, tuple_arg):
    """ Remove the (let (x get$1$N arg) ...) wrappings pulling the original args of a multiple-arg-def
        out from the created Tuple.
        Params: body - the generated body, including all such lets wrapped around the original body of the ksc def
                tuple_arg - the generated tuple, with type a list beginning Tuple
        returns a pair of (a list of the original argument variables) and the remaining body,
            or (None, None) if it was not possible to do so. """
    original_args = []
    for component_idx, expected_type in reversed(list(enumerate(tuple_arg.type.tuple_elems()))):
        if (body.op == "let" and body.second.op == "select"
                and body.second.left == tuple_arg # Selecting from the created Tuple parameter
                and body.second.right == Expression.Constant(component_idx)): # In increasing index order
            if (body.first.type is None) or body.first.type == expected_type:
                # Ok, remove one let wrapping
                original_args.append(Expression.Variable(body.first.name, expected_type))
                body = body.third
                continue
            print("Let-bound {} had type {} but expected {} from {}".format(
                body.first, body.first.type, expected_type, body.second))
        # Fall-through - failed
        return (None, None)
    # All iters of loop succeeded in removing the expected let-wrapper
    return reversed(original_args), body


def main():
    import sys
    if len(sys.argv)!=2 and len(sys.argv)!=3:
        print("Usage: {} <filename> <stop_name (optional)>".format(sys.argv[0]))
        sys.exit(-1)
    if sys.argv[1] == "-":
        s_exp = parse_expr(sys.stdin.read())
    else:
        with open(sys.argv[1]) as f:
            f = f.read()
            if len(sys.argv)==3:
                s_exp = dict(parse_defs(f))[sys.argv[2]]
            else:
                _, s_exp = parse_defs(f)[-1]
    print(s_exp)

if __name__ == "__main__": main()
