from typing import List, NamedTuple, Sequence, Tuple, Union

from ksc.expr import Expr, Let, Lam, If, Assert, Call

PathElement = Union[str, int]

Path = Tuple[PathElement, ...]

_legal_path_members = {
    Let: ["rhs", "body"],
    Lam: ["body"],
    If: ["cond", "t_body", "f_body"],
    Assert: ["cond", "body"],
    # None for Var, Const; special-case Call
}

assert all(
    clas.__annotations__[field_name] == Expr
    for clas, field_names in _legal_path_members.items()
    for field_name in field_names
)


class ExprWithPath(NamedTuple):
    root: Expr
    path: Path
    subtree: Expr

    def __getattr__(self, attr_name: str):
        if attr_name in _legal_path_members.get(self.subtree.__class__, []):
            # attr_name to PathElement is the identity function
            return self._subexp_with_name(attr_name)
        # Allow retrieving any other field (not a member of ExprWithPath) straight from the underlying Expr
        return getattr(self.subtree, attr_name)

    def _subexp_with_name(self, attr_name: str) -> "ExprWithPath":
        assert attr_name in _legal_path_members[self.subtree.__class__]
        # attr_name to PathElement is the identity function
        return ExprWithPath(
            self.root, self.path + (attr_name,), getattr(self.subtree, attr_name)
        )

    @property
    def args(self) -> List["ExprWithPath"]:
        if isinstance(self.subtree, Call):
            return [
                ExprWithPath(self.root, self.path + (i,), arg)
                for i, arg in enumerate(self.subtree.args)
            ]
        raise AttributeError(f"No args on {self.subtree}")

    def all_subexps(self) -> List["ExprWithPath"]:
        return (
            self.args
            if isinstance(self.subtree, Call)
            else [
                self._subexp_with_name(field_name)
                for field_name in _legal_path_members.get(self.subtree.__class__, [])
            ]
        )

    def get(self, p: PathElement):
        if isinstance(p, int):
            # Decode int PathElement as call arg
            return self.args[p]
        # Decode str PathElement as attr_name
        elif p in _legal_path_members[self.subtree.__class__]:
            return self._subexp_with_name(p)

    @staticmethod
    def for_expr(root: Expr) -> "ExprWithPath":
        return ExprWithPath(root, (), root)

    @classmethod
    def from_path(cls, root: Expr, path_elems: Sequence[PathElement]) -> "ExprWithPath":
        ewp = cls.for_expr(root)
        for path_elem in path_elems:
            ewp = ewp.get(path_elem)
        return ewp


def subexps_no_binds(e: Expr) -> List[Expr]:
    return [c.subtree for c in ExprWithPath.for_expr(e).all_subexps()]
