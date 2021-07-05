from collections.abc import Sequence as AbstractSeq
from dataclasses import dataclass
from itertools import islice
import re
from typing import Dict, List, Mapping, NamedTuple, Sequence, Tuple, Type, Union

from ksc.expr import Expr, Let, Lam, If, Assert, Call
from ksc.utils import singleton


#####################################################################
# Path elements


@dataclass(frozen=True, order=True)
class _FieldElement:
    expr_class: Type[Expr]
    field_name: str

    def __post_init__(self):
        assert self.expr_class.__annotations__[self.field_name] == Expr
        class_fields: Dict[str, "_FieldElement"] = _field_elements_by_class.setdefault(
            self.expr_class, {}
        )
        assert self.field_name not in class_fields
        class_fields[self.field_name] = self

    def __str__(self):
        return self.expr_class.__name__ + "." + self.field_name

    def get(self, e: Expr) -> Expr:
        assert e.__class__ == self.expr_class
        return getattr(e, self.field_name)


_field_elements_by_class: Dict[Type, Dict[str, _FieldElement]] = {}


@dataclass(frozen=True, order=True)
class _CallArg:
    n: int

    def get(self, e: Expr) -> Expr:
        assert e.__class__ == Call
        return e.args[self.n]

    def __str__(self):
        return f"call_args[{self.n}]"


PathElement = Union[_FieldElement, _CallArg]
# Note all PathElements define get(Expr) -> Expr

let_rhs = _FieldElement(Let, "rhs")
let_body = _FieldElement(Let, "body")
if_cond = _FieldElement(If, "cond")
if_t_body = _FieldElement(If, "t_body")
if_f_body = _FieldElement(If, "f_body")
assert_cond = _FieldElement(Assert, "cond")
assert_body = _FieldElement(Assert, "body")
lam_body = _FieldElement(Lam, "body")


@singleton
class call_args(AbstractSeq):
    def __getitem__(self, idx):
        if isinstance(idx, int) and idx >= 0:
            return _CallArg(idx)
        raise ValueError(f"Key {idx} not present, only valid array indices.")

    def __len__(self):
        # No good answer here.
        return float("inf")


#####################################################################
# Paths and manipulation thereof

Path = Tuple[PathElement, ...]


class ExprWithPath(NamedTuple):
    root: Expr
    path: Path
    expr: Expr

    def __getattr__(self, attr_name: str):
        # This allows ExprWithPath.rhs, ExprWithPath.body, etc., returning ExprWithPath
        if attr_name in _field_elements_by_class.get(self.expr.__class__, []):
            return self.get(_field_elements_by_class[self.expr.__class__][attr_name])
        # Allow retrieving any other field (not a member of ExprWithPath) straight from the underlying Expr
        return getattr(self.expr, attr_name)

    def get(self, pe: PathElement) -> "ExprWithPath":
        return ExprWithPath(self.root, self.path + (pe,), pe.get(self.expr))

    @property
    def args(self) -> List["ExprWithPath"]:
        if isinstance(self.expr, Call):
            return [self.get(pe) for pe in islice(call_args, 0, len(self.expr.args))]
        raise AttributeError(f"No args on {self.expr}")

    def all_subexprs_with_paths(self) -> List["ExprWithPath"]:
        return (
            self.args
            if isinstance(self.expr, Call)
            else [
                self.get(pe)
                for pe in _field_elements_by_class.get(self.expr.__class__, {}).values()
            ]
        )

    @classmethod
    def from_expr(
        cls, root: Expr, path_elems: Sequence[PathElement] = ()
    ) -> "ExprWithPath":
        ewp = cls(root, (), root)
        for path_elem in path_elems:
            ewp = ewp.get(path_elem)
        return ewp


def subexps_no_binds(e: Expr) -> List[Expr]:
    # ExprWithPath identifies all the non-binding sub-expressions.
    # TODO: consider rewriting callers into Visitors in order to remove this.
    return [c.expr for c in ExprWithPath.from_expr(e).all_subexprs_with_paths()]


#####################################################################
# Serialization to JSON

SerializedPath = List[str]
""" A JSON-able representation of a Path """


def serialize_path(path: Path) -> SerializedPath:
    # There is no particular need to use str() to turn elements to strings,
    # but str() contains all the information that's needed.
    return [str(elem) for elem in path]


_known_field_elements: Mapping[str, _FieldElement] = {
    str(e): e
    for class_fields in _field_elements_by_class.values()
    for e in class_fields.values()
}
# Check _FieldElements all have different str()s
assert len(_known_field_elements) == sum(
    len(c) for c in _field_elements_by_class.values()
)
_call_args_regex = re.compile(r"call_args\[([0-9]+)\]")


def deserialize_path(s_path: SerializedPath) -> Path:
    def deserialize_elem(s_elem: str) -> PathElement:
        if s_elem in _known_field_elements:
            return _known_field_elements[s_elem]
        call_arg = _call_args_regex.match(s_elem)
        if call_arg is not None:
            which_arg = int(call_arg.group(1))
            return call_args[which_arg]
        raise ValueError(f"Could not decode serialized PathElement {s_elem}")

    return tuple(deserialize_elem(e) for e in s_path)
