import numpy as np

class Type:
    node_kinds = {
        "Vec": 1, # one child (Type)
        "Tuple": -1, # special case two or more
        "Integer": 0,
        "Float": 0,
        "Bool": 0,
        "String": 0,
        "Lam": 2, # TODO: Lambda args are in reverse order, prefer src -> dst
        "LM": 2 # Linear map, used in AD
    }

    @staticmethod
    def Vec(elem_type):
        return Type("Vec", [elem_type])

    @staticmethod
    def Tuple(*args):
        return Type("Tuple", args)

    @staticmethod
    def Lam(return_type, arg_type):
        return Type("Lam", [return_type, arg_type])

    @staticmethod
    def LM(return_type, arg_type):
        return Type("LM", [return_type, arg_type]) 

    @staticmethod
    def Index(vec):  # TODO: Call this elem_type for consistency with ksc-MLIR?
        if vec is None:
            return None
        assert vec.kind == "Vec"
        return vec.children[0]

    def __init__(self, kind, children=[]):
        if kind not in Type.node_kinds:
            raise ValueError("bad kind:", kind)
        assert kind == "Tuple" or (Type.node_kinds[kind] == len(children)), (kind, len(children)) # dont' check for 1-tuple
        assert all((ch is None or isinstance(ch, Type)) for ch in children)
        self.kind = kind
        self.children = children

    @property
    def is_scalar(self):
        return self.kind in ["Integer", "Float", "Bool", "String"]

    @property
    def is_lam_or_LM(self):
        return self.kind == "Lam" or self.kind == "LM"

    @property
    def return_type(self):
        assert self.is_lam_or_LM
        return self.children[0]

    @property
    def arg_type(self):
        assert self.is_lam_or_LM
        return self.children[1]

    def accept_value_of_type(self, other):
        """ Finds if a variable of type 'other' can fit into this type.  """
        if other is None: # Allow unknown arguments to fit into any (known) parameter
            return True
        if self.kind != other.kind:
            return False
        if len(self.children) != len(other.children):
            # Rules out different size tuples
            return False
        return all(c.accept_value_of_type(o) for c, o in zip(self.children, other.children))

    def num_elements(self, assumed_vector_size=100):
        if self.kind == "Tuple":
            return sum([c.num_elements(assumed_vector_size) for c in self.children])
        elif self.kind == "Vec":
            return assumed_vector_size * self.children[0].num_elements(assumed_vector_size)
        elif self.is_scalar or self.is_lam_or_LM:
            return 1

    def all_element_types(self):
        if self.kind == "Tuple":
            return set([t for c in self.children for t in c.all_element_types()])
        elif self.kind == "Vec":
            return self.children[0].all_element_types()
        else:
            return set([self])

    @property
    def ndim(self):
        if self.kind == "Vec":
            child_ndim = self.children[0].ndim
            return child_ndim + 1 if child_ndim is not None else None
        elif self.is_scalar:
            return 0
        return None

    def __len__(self):
        assert self.kind == "Tuple"
        return len(self.children)

    def __iter__(self):
        assert self.kind == "Tuple"
        return (c for c in self.children)

    def shortstr(self, tb="<", te=">"):
        el_types = {"Integer": "i", "Bool": "b", "String" : "s", "Float": "f", "Lam": "l", "LM": "l"}
        if self.kind in el_types:
            return el_types[self.kind]
        if self.kind == "Tuple":
            return tb + "".join([c.shortstr() for c in self.children]) + te
        elif self.kind == "Vec":
            return "v" + self.children[0].shortstr()
        else:
            raise ValueError(f"Unknown Type.{self.kind}")

    def shortstr_py_friendly(self):
        return self.shortstr("_t", "t_")

    def __str__(self):
        if len(self.children) == 0 and (self.kind != "Tuple"):
            return self.kind
        elems = [str(c) for c in self.children]
        if self.is_lam_or_LM:
            elems = ["{}({})".format(*elems)]
        return "({})".format(" ".join([self.kind] + elems))

    def __repr__(self):
        res = "Type.{}".format(self.kind.capitalize())
        if Type.node_kinds[self.kind] != 0:
            res += "({})".format(", ".join([repr(c) for c in self.children]))
        return res

    def __eq__(self, other):
        if id(self) == id(other):
            return True
        if other is None or self.kind != other.kind:
            return False
        if self.is_scalar:
            return True
        if self.kind == "Tuple" and len(self.children) != len(other.children):
            return False
        return all([c == o for c, o in zip(self.children, other.children)])

    def __hash__(self):
        return hash(str(self))

Type.String = Type("String")
Type.Integer = Type("Integer")
Type.Float = Type("Float")
Type.Bool = Type("Bool")
Type.String = Type("String")
