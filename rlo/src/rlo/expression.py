# fmt: off
# mypy: ignore-errors
import functools
import hashlib
import re
from typing import Any, Callable, Dict, Generator, List, Mapping, Optional, Sequence, Tuple, Union

from rlo import utils
from ksc.type import Type


def anonymous(cls):
    return cls()

@anonymous
class EF:
    """ Singleton - expression.EF is the only instance. Use e.g. EF.Let(...) """
    def __getattr__(self, attr_name):
        node_type_name = attr_name[0].lower() + attr_name[1:]
        if node_type_name == attr_name or node_type_name not in Expression.node_types:
            # Only allow attribute names starting with uppercase (all node_types are lowercase)
            raise AttributeError(attr_name)
        return lambda *children, **kwargs: Expression(node_type_name, [to_expression(c) for c in children], **kwargs)


class TypePropagationError(Exception):
    pass


class Expression:
    """
    Represents an expressions as a tree of operations, with each sub-tree being an Expression.

    Some key attributes of an Expression object are:
        self.children: the list of children, each of which is an Expression object
        self.op: a string specifying the operations type (e.g. "add", "let")

    """
    # dict from op/type to num_children.
    node_types = {
        "constant": 0,
        "variable": 0,
        "let": 3, #variable, bound value, body
        "if": 3,
        "tuple": None, # Uniquely, has any number of children !=1, via special edges.
        "select": 2, # First child is tuple, second child is 0-based *constant* index
        "apply": 2,
        "lam": 2,

        # Not sure whether these should be considered primitive functions...
        "stop" : 1, # if present - does not allow any rules to be applied to the consequent expr
        "assert" : 2,

        # KSC distinguishes the following as being "primitive functions", subject to a call,
        # but there seems no advantage in treating them differently in RLO.
        "add": 2, "sub": 2, "mul": 2, "div": 2,
        "exp": 1, "log": 1,
        "eq": 2, "gt": 2, "gte": 2,
        "to_float": 1,
        "$ranhashdoub": 1,

        "build": 3, #Includes lambda. Arguments are (size, boundvariable, functionbody)

        "index": 2, #position and vec
        "size": 1,
        "sum": 1,
        "sumbuild": 3, # Includes lambda. Arguments are (size, boundvariable, functionbody).
        "deltaVec": 3,
        "constVec": 2,

        # "<" and "<=" done by switching operands
        # No !=, pr, inline, neg,  yet
    }

    # Use a deterministic ordering of node_types for python versions < 3.6
    node_type_lookup = {n: i for i, n in enumerate(sorted(node_types))}
    num_node_types = len(node_types)

    def __init__(self, op, children, value=None, name=None, type=None, size=None, cost=None):
        expected_num_ch = Expression.node_types[op]
        assert op != "tuple" or len(children)!= 1, "Tuple can have any #children except 1: {}".format(children[0])
        assert op == "tuple" or len(children) == expected_num_ch, "op {} expected {} children but received {}: {}".format(
                op, expected_num_ch, len(children), children)
        self.op = op
        self.children = children.copy()
        ##### Restrict to first-order KSC language #####
        assert ((self.op == "let" and self.second.without_stop().op == "lam")
              or self.op == "stop"
              or all(c.is_lambda_free for c in children))
        # The following was intended to assure that all calls were to top-level functions.
        # However it does not prevent variables bound to top-level functions being passed around elsewhere (as function pointers),
        # nor calls to variables that are bound to e.g. if-statements or the results of other calls...such may break the cost model!
        assert self.op != "apply" or self.left.without_stop().op == "variable", "first-order calls must be to variables, not {}".format(self.left)
        ##### End first-order restriction #####
        assert not self.is_binder or self.bound_var.op == "variable"

        assert type is None or (op in ["variable", "constant"] and isinstance(type, Type))
        assert name is None or op == "variable"
        assert value is None or op == "constant"
        assert size is None or op == "select"
        assert cost is None or op == "stop"

        # Set type and per-op fields
        if op == "variable":
            assert name is not None and name.find(" ")==-1
            self.name = name
            self._type = type
        elif op == "constant":
            assert is_constant(value)
            self.value = value
            self._type = type
        else:
            assert type is None
            if op == "lam":
                assert children[0].op == "variable"
                # Require type for argument, as type propagation will not infer one
                assert children[0].type is not None
            elif op == "select":
                assert self.right.op == "constant" and isinstance(self.right.value, int) and self.right.value >= 0
                if size is not None:
                    assert self.right.value < size
                self.size = size
            elif op == "stop":
                self.declared_cost = cost
            self._type = self._compute_type()

        self._num_nodes = 1 + sum([n.num_nodes for n in self.children])
        self._hash = None # compute on demand
        # Compute free variables.
        if self.is_binder:
            # Free vars of parent = union of free vars of children, EXCEPT that we don't count the binding occurrence,
            # nor do we count occurrences of the bound variable in the last child (true for lam, let, sum/build).
            # If the same variable name occurs in any other child, we include that, as that refers to an outer variable.
            free_var_names = set(v for i,ch in enumerate(self.children) if self.binds_in_child(i) for v in ch.free_var_names)
            free_var_names.remove(self.bound_var.name) # Definitely present as binds_in_child true for the binding occurrence
            for i, ch in enumerate(self.children):
                if not self.binds_in_child(i):
                    free_var_names.update(ch.free_var_names)
            self._free_var_names = frozenset(free_var_names)
        elif self.op == "variable":
            self._free_var_names = frozenset([self.name])
        else:
            self._free_var_names = functools.reduce(frozenset.union, (ch.free_var_names for ch in self.children), frozenset())
        # Compute index of next unused temporary var
        if self.op == "variable":
            m = re.fullmatch("var([\\d]+)", self.name)
            self._next_unused_var = (int(m.group(1)) + 1) if m else 0
        else:
            self._next_unused_var = max([0] + [ch._next_unused_var for ch in self.children])

    def without_stop(self):
        return self.only_child.without_stop() if self.op == "stop" else self

    def _compute_type(self):
        # Propagate types upwards (towards the root), by computing the type for this Expression given the types of its children.
        # Note that this does not check consistency within the child Expressions, for example:
        #   EF.Let(Expression.Variable("x", Type.Int), 5, EF.Add(Expression.Variable("x", Type.Float), 3.0))
        # will be given type float (rather than error) despite mis-typing of 'x'.
        # Rather, we rely on the client to have already ensured such consistency: deep_copy_with_types / make_toplevel do so.
        child_types = [ch.type for ch in self.children]
        if any(ct is None for ct in child_types):
            return None
        if self.op == "let":
            var_type, val_type, body_type = child_types
            if var_type != val_type:
                raise TypePropagationError(f"Let-bound variable {self.first.name} : {var_type} bound to value of type {val_type}")
            return body_type
        elif self.op == "lam":
            arg_type, body_type = child_types
            return Type.Lam(arg_type, body_type)
        elif self.op in ["sumbuild", "build"]:
            size_type, var_type, body_type = child_types
            assert size_type == var_type == Type.Integer
            return Type.Tensor(1,body_type) if self.op == "build" else body_type
        elif self.op == "if":
            assert child_types[0] == Type.Bool
            assert child_types[1] == child_types[2], "Expected {} but got {} for the else branch of if".format(child_types[1], child_types[2])
            return child_types[1]
        elif self.op == "apply":
            func_type, arg_type = child_types
            if arg_type != func_type.lam_arg_type:
                raise TypePropagationError(
                    f"Function of type {func_type} cannot accept argument of type {arg_type} in Expression {self}"
                )
            return func_type.lam_return_type
        elif self.op in ["to_float", "$ranhashdoub"]:
            assert child_types[0] == Type.Integer
            return Type.Float
        elif self.op == "assert":
            assert child_types[0] == Type.Bool
            return child_types[1]
        elif self.op == "index":
            return Type.Index(child_types[1])
        elif self.op == "select":
            assert self.right.op == "constant" # The index
            assert child_types[0].is_tuple, "Select into a non-Tuple type found in {}".format(str(self))
            return child_types[0].tuple_elem(self.right.value)
        elif self.op == "sum":
            return Type.Index(child_types[0])
        elif self.op == "size":
            return Type.Integer
        elif self.op == "tuple":
            return Type.Tuple(*child_types)
        elif self.op == "constVec":
            return Type.Tensor(1,child_types[1])
        elif self.op == "deltaVec":
            return Type.Tensor(1,child_types[2])
        elif len(self.children) == 2:
            # Binary mathematical operations, all on pairs of same primitive type
            l, r = child_types
            if self.op in ["gte", "gt", "eq"]:
                assert l == r
                assert l in [Type.Integer, Type.Float]
                return Type.Bool
            if self.op in ["add", "mul", "sub", "div"]:
                if l == r:
                    # No broadcasting required (this includes vector + vector)
                    return l
                # Handle broadcasting
                if l in [Type.Integer, Type.Float]:
                    # left-hand side is scalar
                    r_elem_type = utils.single_elem(r.all_element_types())
                    assert l == r_elem_type
                    return r
                if r in [Type.Integer, Type.Float]:
                    # right-hand side is scalar
                    l_elem_type = utils.single_elem(l.all_element_types())
                    assert r == l_elem_type
                    return l
                raise TypePropagationError(
                    f"Cannot broadcast operator {self.op} with child types {l} and {r}"
                )
            raise TypePropagationError(f"Unhandled binary operator {self.op}")
        elif len(self.children) == 1:
            return child_types[0]
        raise TypePropagationError(f"Cannot compute type for {self}")

    @property
    def is_lambda_free(self):
        if self.op == "let" and self.second.op == "lam":
            return False
        # We could assert that our children are lambda_free, but that'd be expensive
        # (Potentially O(n^2) in constructing an Expression of size n)
        return self.op != "lam"

    def __hash__(self):
        if self._hash is None:
            self._hash = self._hash_with_var_mapping({}, 0)
        return self._hash

    def _hash_with_var_mapping(self, hash_for_var, num_binders):
        # hash_for_var is a dictionary where the keys are variable names and the values are integers:
        #    0 for the outermost/topmost bound variable, 1 for the next, and so on.
        #    (These may not be contiguous if some names are re-bound i.e. the outer binding is shadowed.)
        # num_binders is the total number of binders on the path back to the root; thus,
        #    (num_binders - hash_for_var[name]) gives the DeBruijn number for the variable with that name.
        #    DeBruijn numbering gives the most-closely-bound variable number 0, the next-most-closely numbered 1, etc.
        #    This is very normalizing as every subtree with the same structure like (lam x x) or (lam y y)
        #    gets the same numbering no matter how deeply nested.
        h = Expression.node_type_lookup[self.op]
        if self.op == "variable":
            if self.name in hash_for_var:
                # Use DeBruijn number. (May not be strictly necessary, just hash_for_var[self.name] would be enough
                # for consistency of hash, but the extra subtraction seems insignificant.)
                h += (num_binders - hash_for_var[self.name])
            else:
                # Free variable, hash the name. Use a hash that's stable across invocations of python (without PYTHONHASHSEED). [Consider zlib.crc32 ?]
                # An alternative would be to number these according to the order they are found in e.g. L-to-R depth-first traversal;
                # this would produce the same hash for Expressions that have the same GNN representation, for example (mul x y) and (mul y x).
                # This would allow to improve caching of values (for equivalent Expressions from the same starting Expr), with appropriate ==.
                h += int(hashlib.sha224(self.name.encode()).hexdigest()[:8], base=16)
        elif self.op == "constant":
            h += hash(self.value)
        if self.is_binder:
            # Bind variable, so give it hash now. Use the current length of the mapping, so deBruijn number 0,
            # even if the name is already bound (i.e. overwrite the previous number within this subtree) - so
            # the numbering will be sparse if a name is bound more than once on a single path down from the root.
            hash_for_var_bound = {**hash_for_var, self.bound_var.name: num_binders}
        for i,c in enumerate(self.children):
            h *= 13
            hv, n = (hash_for_var_bound, num_binders+1) if self.is_binder and self.binds_in_child(i) else (hash_for_var, num_binders)
            h += hash(c) if c.free_var_names.isdisjoint(hv.keys()) else c._hash_with_var_mapping(hv, n)
        # Keep to something that fits in a signed 32-bit int (rather arbitrary given this is python)
        return h % 2147483647 # the largest prime number that fits in 32-bits is, as luck would have it....the largest number that fits in 32 bits!

    @classmethod
    def Constant(cls, value) -> 'Expression':
        if isinstance(value, float):
            type = Type.Float
        elif isinstance(value, bool):
            type = Type.Bool
        elif isinstance(value, int):
            type = Type.Integer
        else:
            raise ValueError("Not a valid constant {}".format(value))
        return cls("constant", [], value=value, type=type)

    @classmethod
    def Variable(cls, name, type=None) -> 'Expression':
        return cls("variable", [], name=name, type=type)

    def __add__(self, other) -> 'Expression':
        return Expression("add", [self, to_expression(other)])

    def __sub__(self, other) -> 'Expression':
        return Expression("sub", [self, to_expression(other)])

    def __mul__(self, other) -> 'Expression':
        return Expression("mul", [self, to_expression(other)])

    def __truediv__(self, other) -> 'Expression':
        return Expression("div", [self, to_expression(other)])

    def __radd__(self, other) -> 'Expression':
        return to_expression(other) + self

    def __rsub__(self, other) -> 'Expression':
        return to_expression(other) - self

    def __rmul__(self, other) -> 'Expression':
        return to_expression(other) * self

    def __rtruediv__(self, other) -> 'Expression':
        return to_expression(other) / self

    def __gt__(self, other) -> 'Expression':
        return Expression("gt", [self, to_expression(other)])

    def _get_var_mapping(self, other: "Expression") -> Optional[Dict]:
        """Returns a dictionary mapping free variable names of self to variable names of other.

        If the value is None, no mapping/conversion is possible.
        If the value is an empty dict, or contains only identity mappings (key == value),
          then the two are alpha-convertible i.e. by renaming only *bound* variables.
        If the value is a dict containing mappings where key != value,
          then they can be converted but only by renaming free variables.
        """
        if self is other:
            # Shortcut/optimization for a common case.
            # Sadly "return dict()" is not quite right, because we also need to record
            # that the free variables in the LHS must be the same as the free variables on the RHS.
            # Otherwise, if this is part of a containing expression, we could map those variables
            # differently in other parts of the containing expressions; if those variables were bound
            # this would lead to false equivalences, e.g. (lam x -> x+x) == (lam y -> x+y).
            return {v : v for v in self.free_var_names}
        if self.op != other.op:
            return None # Fail
        if self.op == "constant":
            return dict() if self.value == other.value else None
        if self.op == "variable":
            return {self.name: other.name}
        if len(self.children) != len(other.children):
            return None # Fail
        if self.op == "lam" and self.left.type != other.left.type:
            return None # Fail
        combined = dict()
        def add_child_mappings(paired_children):
            for c1, c2 in paired_children:
                cd = c1._get_var_mapping(c2)
                if cd is None:
                    return False
                common_vars = combined.keys() & cd.keys()
                if any(combined[v] != cd[v] for v in common_vars):
                    return False # Fail
                combined.update(cd)
            return True
        if self.is_binder:
            bound_pairs, unbound_pairs = [], []
            for i, cs in enumerate(zip(self.children, other.children)):
                (bound_pairs if self.binds_in_child(i) else unbound_pairs).append(cs)
            # First process children in which bound variable is bound
            if not add_child_mappings(bound_pairs):
                return None
            # Check equivalence of bound variable, then remove it
            assert self.bound_var.name in combined
            assert combined[self.bound_var.name] == other.bound_var.name
            del combined[self.bound_var.name] # Remove bound variable from return
            if other.bound_var.name in combined.values():
                # We've already ruled out self.bound_var.name mapping to multiple things.
                # Now rule out the converse.
                return None
            # Fall through to check children in which bound variable is not bound
        else:
            unbound_pairs = zip(self.children, other.children)
        return combined if add_child_mappings(unbound_pairs) else None

    def __eq__(self, other: Any) -> bool:
        """ This compares alpha-equivalence of expressions (i.e. can one be converted to the other
              by renaming bound variables), producing a boolean.
            To construct the expression that compares values at runtime, use EF.Eq().
            To check the expressions are the same without alpha-renaming, use str(e)==str(e2). """
            #(Or, if we had a flyweight pattern, could require using "is").
        if not isinstance(other, Expression):
            return False
        sd = self._get_var_mapping(other)
        if sd is None:
            return False
        if any(k != v for k,v in sd.items()):
            return False
        return True

    def has_same_graph(self, other: "Expression") -> bool:
        """Compares whether the two expressions have the same graph to feed into the GNN.

        This is true if they can be converted by renaming both free and bound variables,
        e.g. "let x=a in x+1" has the same graph as "let x=b in x+1".

        However, note that whereas alpha-convertible expressions continue to behave the same
        even when embedded inside larger expressions (which may bind variables free in both),
        this is not true of expressions with the same graph: as we would wish,
        "let a=3 in let x=a in x+1" has s different graph from "let a=3 in let x=b in x+1"!
        """
        return self._get_var_mapping(other) is not None

    def __str__(self):
        if self.op == "constant":
            if isinstance(self.value, bool):
                return "true" if self.value else "false"
            return str(self.value)
        if self.op == "variable":
            return "{}".format(self.name) # var usages should not have types, they should appear only in arguments to defs and arguments to lams
        if self.op == "select":
            idx = self.right.value + 1
            return "(get${}${} {})".format(idx, self.size, self.left)
        if self.op == "build" or self.op == "sumbuild":
            # Add lambda
            return "({} {} (lam ({} : Integer) {}))".format(self.op, *self.children)
        if self.op == "apply":
            return "({} {})".format(self.left, # Special case tuple for neatness (not essential)
                self.right if self.right.op != "tuple" else " ".join([str(e) for e in self.right.children]))
        if self.op == "lam":
            # Display type of bound variable
            return "(lam ({} : {}) {})".format(self.left, self.left.type, self.right)
        if self.op == "let":
            # Add extra brackets
            return "(let ({} {}) {})".format(*self.children)
        return "({})".format(" ".join([self.op] + [str(c) for c in self.children]))

    def ksc_str(self, last_fn=None):
        from rlo.sparser import remove_multi_arg_wrappings
        if self.op == "let" and self.second.op == "lam":
            body = self.second.right
            args = [self.second.left] # Assume unary unless we discover otherwise (next)
            if self.second.left.type.is_tuple:
                multi_args, inner_body = remove_multi_arg_wrappings(body, self.second.left)
                if multi_args and self.second.left.name not in inner_body.free_var_names:
                    # Found bindings for all expected components of tuple, and tuple now unused.
                    # So, generate multi-arg def, without the original tuple-type argument.
                    # Note, many trivial optimizations (e.g. inlining the let-bound vars with their "select"s) will prevent this...
                    body = inner_body # What's left after removing the lets
                    args = list(multi_args)
            ret_type = self.second.type.lam_return_type
            arg_str = " ".join(["({} : {})".format(a, a.type) for a in args])
            body_str = ("{}" if len(body.children)>0 else "({})").format(body) # Extra brackets needed around trivial var/const
            # Recurse on the let body i.e. the next def
            return f"(def {self.first} {ret_type} ({arg_str}) {body_str})\n\n" + self.third.ksc_str(self.first.name)
        # The body of the last let should be the innermost function
        assert self.op == "variable" and self.name == last_fn
        return ""

    def __repr__(self, nested=False):
        if self.op == "constant":
            # Inside another expression, raw constants are turned into Expression.Constants by to_expression
            return ("{}" if nested else "Expression.Constant({})").format(self.value)
        elif self.op == "variable":
            if self.type is not None:
                return 'Expression.Variable("{}", {})'.format(self.name, repr(self.type))
            # Inside another expression, the string "x" is interpreted as a variable by to_expression
            return ('"{}"' if nested else 'Expression.Variable("{}")').format(self.name)

        chs = [c.__repr__(nested=True) for c in self.children]
        # Make first character uppercase, but (unlike capitalize()) leave the rest as per node_types
        return "EF.{}({})".format(self.op[0].upper()+self.op[1:], ", ".join(chs))

    @property
    def type(self):
        return self._type

    def eval(self):
        if self.op != "constant":
            raise ValueError("Cannot evaluate a non-constant expression")
        return self.value

    @property
    def nodes(self) -> List["Expression"]:
        """
        Does a depth-search traversal of the expression tree, returning a list of all sub-expressions (sub-trees).
        """
        res = []
        def traverse(subexp):
            res.append(subexp)
            for c in subexp.children:
                traverse(c)
        traverse(self)
        return res

    @property
    def num_nodes(self) -> int:
        return self._num_nodes

    @property
    def only_child(self):
        if len(self.children)==1:
            return self.children[0]
        else:
            raise TypeError("child property requires an expression with exactly 1 child")

    @property
    def left(self):
        if len(self.children)==2:
            return self.children[0]
        else:
            raise TypeError("left property requires an expression with exactly 2 children")

    @property
    def right(self):
        if len(self.children)==2:
            return self.children[1]
        else:
            raise TypeError("right property requires an expression with exactly 2 children")

    @property
    def first(self):
        if len(self.children)==3:
            return self.children[0]
        else:
            raise TypeError("first property requires an expression with exactly 3 children")

    @property
    def second(self):
        if len(self.children)==3:
            return self.children[1]
        else:
            raise TypeError("second property requires an expression with exactly 3 children")

    @property
    def third(self):
        if len(self.children)==3:
            return self.children[2]
        else:
            raise TypeError("third property requires an expression with exactly 3 children")

    @property
    def is_binder(self):
        return self.op in ["build", "lam", "let", "sumbuild"]

    def binds_in_child(self, idx):
        """ Returns whether the bound variable may appear in child <idx> (E.g. build n x body: x may appear in x or body, but not n).
            The converse means any reference in that child must be to an enclosing variable with the same name as that bound. """
        assert self.is_binder
        return idx == self._bound_var_index or idx == len(self.children) - 1

    @property
    def makes_vec(self):
        return self.op in ["deltaVec", "constVec"]

    @property
    def bound_var(self):
        return self.children[self._bound_var_index]

    @property
    def _bound_var_index(self):
        assert self.is_binder
        return 0 if self.op in ["lam", "let"] else 1


    def involves_var(self, var):
        """ Returns true if this Expression uses the specified variable (from outside).
            Returns false if the variable does not appear or is bound within this Expression. """
        name = var if isinstance(var, str) else var.name
        return name in self.free_var_names

    def binds_any_in(self, other):
        return self.is_binder and other.involves_var(self.bound_var)

    @property
    def free_var_names(self):
        return self._free_var_names

    @staticmethod
    def new_var(*exprs, type=None):
        """ Makes a variable with a name fresh within all Expressions in exprs (or this Expression if no exprs)"""
        return Expression.Variable("var{}".format(max(e._next_unused_var for e in exprs)), type)

    def children_with_indices(self, idx_offset=0):
        c_idx = idx_offset + 1
        for child in self.children:
            yield (child, c_idx)
            c_idx += child.num_nodes

    def node_ids_with_environment(self, idx_offset=0, env=None, stop_at_stop=True):
        """ Generator, yields tuples (index, Expression, environment) for all subexps (except binding occurrences of variables).
            The environment is a mapping from variable names to tuple (index of binder, bound value), for let-bound variables only. """
        if env is None:
            env = {}
        if self.op == "stop" and stop_at_stop:
            return
        yield (idx_offset, self, env)
        if self.is_binder:
            if self.op == "let":
                bound_env = {**env, self.first.name: (idx_offset, self.second)}
            else:
                bound_env = {k:v for k,v in env.items() if k != self.bound_var.name}
            for i, (child, c_idx) in enumerate(self.children_with_indices(idx_offset)):
                if i != self._bound_var_index:
                    yield from child.node_ids_with_environment(c_idx,
                        bound_env if self.binds_in_child(i) else env,
                        stop_at_stop)
        else:
            for child, c_idx in self.children_with_indices(idx_offset):
                yield from child.node_ids_with_environment(c_idx, env, stop_at_stop)


    def get_path(self, to_id: int, idx_offset: int=0) -> Generator[Tuple[int, "Expression", Optional[int]], None, None]:
        """ Generator that yields tuples of (node_index, Expression, index of next node within this node's children)
            for all nodes on the path from here to the specified <to_id>.
            The index within children will be None for the last tuple only. """
        assert to_id >= idx_offset and to_id < idx_offset + self.num_nodes
        if to_id == idx_offset:
            yield (idx_offset, self, None)
        else:
            for i, (child, c_idx) in enumerate(self.children_with_indices(idx_offset)):
                if to_id >= c_idx and to_id < c_idx + child.num_nodes:
                    yield (idx_offset, self, i)
                    yield from child.get_path(to_id, c_idx)


    def replace_subtree(self, target_idx: int, val: Union['Expression', Callable], preserve: Optional['Expression'] = None):
        """ Returns an expression identical to this one except that
                * The subtree at <target_idx> is replaced by <val>
                * Any variables bound within this Expression on the path to target_idx, that would capture variables free in <preserve>,
                     are alpha-converted to new names.
            For example, (let x=y in x+z)[z -> x+1] =/=> let x=y in x+(x+1), as the inserted x+1 would be captured by the x bound by the let;
                instead, the inserted x must refer to a different (outer) x, achieved by renaming
                (let x=y in x+z)[z -> x+1] ==> let x'=y in (x+z)[x->x', z->x+1] ==> let x'=y in x'+(x+1)
            Params:
                * target_idx: index within this subtree to replace (0 = root).
                * val: either an Expression to place at <target_idx>,
                           or a function from the subtree currently at <target_idx> (after renamings) to the new value.
                * preserve: an Expression which must have the same meaning, when substituted in at <target_idx>, as it would
                     in place of this Expression (that is, variables bound to have new values at <target_idx> must be renamed).
                     May be None, to indicate <val> (if <val> is an Expression) or the empty Expression (if <val> is a function). """
        if preserve is None:
            preserve = val if isinstance(val, Expression) else Expression.Constant(0)
        return self._cav_helper((target_idx, val, preserve), dict())

    def rename_free(self, var_name: str, new_var: 'Expression'):
        """ Returns an Expression identical to this one except that
            *free* occurrences of variables named <var_name> are replaced with <new_var>. """
        return self._cav_helper(None, {var_name: new_var})

    def _cav_helper(self, targetted: Optional[Tuple[int, Union['Expression', Callable], 'Expression']], subst: Mapping[str, 'Expression']):
        """ Params:
                * targetted: either None, or a tuple (target_idx, val, preserve)
                    - target_idx: index to replace (0=root)
                    - <val> Expression with which to replace, or function, as per replace_subtree,
                    - preserve: RHS in which any free variables must not be captured
                * subst: optional dict from string (variable-name) to Expression, of other substitutions to apply
                    (across the whole subtree; RHS's typically are free variables to avoid capture by renaming)
            Returns: an Expression identical to this one, except that
                * if <targetted> is non-None, then
                    the subtree at <target_idx> is replaced by <val> (or the result of applying <val> to that subtree)
                * Any substitutions in <subst> are also applied to any _free_ variables in this Expression;
                * Any variables bound within this Expression, are alpha-converted to new names, *iff* they would capture variables free in
                    - <preserve> (if non-None)
                    - any of the Expression RHSs in <subst>
            """
        if targetted is not None:
            target_idx, replacement, preserve = targetted
            assert target_idx >= 0 and target_idx < self.num_nodes
            if target_idx == 0:
                if isinstance(replacement, Expression):
                    return replacement
                # Apply function to subtree-to-be-replaced *after* making consistent with renames already applied above
                return replacement(self._cav_helper(None, subst))
        if self.op == "variable":
            # Check we haven't pointlessly traversed the tree to do an identity rename
            assert self.name not in subst or subst[self.name] != self
            return subst.get(self.name, self)

        # Reduce down the substitution to what'll have an effect within this subtree
        vars_to_keep = self.free_var_names.intersection(subst) # This removes any substitution for a variable bound by this node
        vars_to_vals = {var:subst[var] for var in vars_to_keep} if len(vars_to_keep) < len(subst) else subst
        if targetted is None and len(vars_to_vals) == 0:
            # Nothing to do in this subtree
            return self

        all_vals = ([preserve] if targetted is not None else []) + list(vars_to_vals.values())
        # If this node rebinds a variable name free in something that'll be substituted in beneath us, rename the rebind to avoid capture
        if self.is_binder:
            if any(self.bound_var.name in v.free_var_names for v in all_vals):
                nv = Expression.new_var(self, *all_vals, type=self.bound_var.type) # TODO better to use toplevel expr as name-generator
                bound_vars_to_vals = {**vars_to_vals, self.bound_var.name: nv}
            else:
                bound_vars_to_vals = vars_to_vals
        if targetted is not None: target_idx -= 1 # Move onto first child
        n_children = []
        for i,child in enumerate(self.children):
            if targetted is None:
                child_tgt = None
            elif target_idx < child.num_nodes:
                child_tgt = (target_idx, replacement, preserve)
                # Found the right child; don't consider substituting into subsequent children
                targetted = None
            else:
                child_tgt = None
                target_idx -= child.num_nodes # For next child (after this one)
            n_children.append(child._cav_helper(child_tgt,
                bound_vars_to_vals if self.is_binder and self.binds_in_child(i) else vars_to_vals))
        assert targetted is None # Found the target somewhere
        return self.clone_with_new_children(n_children)

    def clone_with_new_children(self, new_children: Sequence["Expression"]) -> "Expression":
        """ Creates a node like this one except that it has children <new_children>. """
        assert len(self.children) == len(new_children)
        if len(new_children) == 0:
            return self
        extra_args = ({"name": self.name, "type": self.type} if self.op == "variable"
            else {"value": self.value, "type": self.type} if self.op == "constant"
            else {"size": self.size} if self.op == "select"
            else {"cost": self.declared_cost} if self.op == "stop"
            else {})
        return Expression(self.op, new_children, **extra_args)

    _child_accessor_name_map = {
        # A map from the name of each of the child accessors, to which element is selected.
        # (Does not detail the restriction on the number of children that must be present for each.)
        "left": 0, "right": 1, "first": 0, "second": 1, "third": 2, "only_child": 0
    }

    def child_and_index_accessor(self, attr_name:str, idx_offset=0) -> Optional[Tuple["Expression", int]]:
        """ If <attr_name> is 'left', 'right' etc. (one of the child accessors),
            returns the corresponding child Expression *and that child's index* (pre-order traversal numbering).
            If <attr_name> is not such, returns None.
        """
        which_ch = Expression._child_accessor_name_map.get(attr_name)
        if which_ch is None:
            return None
        ch = getattr(self, attr_name) # Performs legality check
        ch2, ch_idx = list(self.children_with_indices(idx_offset))[which_ch]
        assert ch is ch2
        return (ch, ch_idx)


ConstantType = Union[float, bool, int]

def is_constant(val: Any) -> bool:
    return isinstance(val, (float, bool, int))

def to_expression(other: Union[ConstantType, str, Expression]) -> Expression:
    if is_constant(other):
        return Expression.Constant(other)
    elif isinstance(other, str):
        return Expression.Variable(other)
    elif isinstance(other, Expression):
        return other
    raise ValueError("Not an Expression or convertible: {}".format(other))


class RewriteTarget:
    def __init__(self, root, exp=None, idx=0):
        if exp is None:
            exp = root
        assert id(root.nodes[idx]) == id(exp)
        # Enforces idx==0 iff root==exp
        self._root = root
        self._exp = exp
        self._idx = idx

    def __getattr__(self, attr_name):
        ch_and_idx = self._exp.child_and_index_accessor(attr_name, self._idx)
        if ch_and_idx is not None:
            return RewriteTarget(self._root, *ch_and_idx)
        raise AttributeError(attr_name)

    @property
    def node(self):
        return self._exp

    def _rewrites_here(self, rule):
        # This is very inefficient, as it first finds all actions on the whole root expression,
        # but RewriteTarget is used for testing on small expressions only.
        return [rw for rw in rule.get_all_rewrites_expr(self._root) if rw.node_id == self._idx]

    def is_rule_applicable(self, rule):
        num_rewrites = len(self._rewrites_here(rule))
        assert num_rewrites <= 1
        return num_rewrites == 1

    def apply_rule(self, rule):
        return utils.single_elem(self._rewrites_here(rule)).apply_expr(self._root)
