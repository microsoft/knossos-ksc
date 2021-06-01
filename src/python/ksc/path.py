from collections.abc import Sequence as AbstractSeq
from dataclasses import dataclass
from itertools import count, islice
from typing import List, Mapping, NamedTuple, Sequence, Tuple, Type, Union

from ksc.expr import Expr, Let, Lam, If, Assert, Call
from ksc.utils import group_by, single_elem, singleton


@dataclass(frozen=True, order=True)
class _FieldElement:
    expr_class: Type[Expr]
    field_name: str

    def __post_init__(self):
        assert self.expr_class.__annotations__[self.field_name] == Expr
        if globals().setdefault(repr(self), self) != self:
            raise ValueError("Not a unique instance")

    def __repr__(self):
        return self.expr_class.__name__ + "_" + self.field_name

    def get(self, e: Expr) -> Expr:
        assert e.__class__ == self.expr_class
        return getattr(e, self.field_name)


@dataclass(frozen=True, order=True)
class _CallArg:
    n: int

    def get(self, e: Expr) -> Expr:
        assert e.__class__ == Call
        return e.args[self.n]

    def __repr__(self):
        return f"Call_args[{self.n}]"


PathElement = Union[_FieldElement, _CallArg]
# Note all PathElements define get(Expr) -> Expr

_field_elements_by_class: Mapping[Type, Mapping[str, _FieldElement]] = {
    clazz: {
        field_name: single_elem(field_elems)
        for field_name, field_elems in group_by(
            fields, lambda fe: fe.field_name
        ).items()
    }
    for clazz, fields in group_by(
        [
            _FieldElement(Let, "rhs"),
            _FieldElement(Let, "body"),
            _FieldElement(If, "cond"),
            _FieldElement(If, "t_body"),
            _FieldElement(If, "f_body"),
            _FieldElement(Assert, "cond"),
            _FieldElement(Assert, "body"),
            _FieldElement(Lam, "body"),
        ],
        lambda fe: fe.expr_class,
    ).items()
}


@singleton
class Call_args(AbstractSeq):
    def __getitem__(self, idx):
        if isinstance(idx, int) and idx >= 0:
            return _CallArg(idx)
        raise ValueError(f"Key {idx} not present, only valid array indices.")

    def __len__(self):
        # No good answer here.
        return float("inf")


Path = Tuple[PathElement, ...]


class ExprWithPath(NamedTuple):
    root: Expr
    path: Path
    subtree: Expr

    def __getattr__(self, attr_name: str):
        # This allows ExprWithPath.rhs, ExprWithPath.body, etc., returning ExprWithPath
        if attr_name in _field_elements_by_class.get(self.subtree.__class__, []):
            return self.get(_field_elements_by_class[self.subtree.__class__][attr_name])
        # Allow retrieving any other field (not a member of ExprWithPath) straight from the underlying Expr
        return getattr(self.subtree, attr_name)

    def get(self, pe: PathElement) -> "ExprWithPath":
        return ExprWithPath(self.root, self.path + (pe,), pe.get(self.subtree))

    @property
    def args(self) -> List["ExprWithPath"]:
        if isinstance(self.subtree, Call):
            return [self.get(pe) for pe in islice(Call_args, 0, len(self.subtree.args))]
        raise AttributeError(f"No args on {self.subtree}")

    def all_subexps(self) -> List["ExprWithPath"]:
        return (
            self.args
            if isinstance(self.subtree, Call)
            else [
                self.get(pe)
                for pe in _field_elements_by_class.get(
                    self.subtree.__class__, {}
                ).values()
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
    return [c.subtree for c in ExprWithPath.from_expr(e).all_subexps()]
