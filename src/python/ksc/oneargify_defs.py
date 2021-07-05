from typing import List, Iterable
from ksc.cav_subst import make_nonfree_var
from ksc.expr import ASTNode, Def, Let, Var
from ksc.untuple_lets import untuple_one_let
from ksc.type import Type


def oneargify_def(d: Def) -> Def:
    if len(d.args) == 1:
        return d
    tuple_arg = make_nonfree_var(
        "func_arg", [d.body] + d.args, type=Type.Tuple(*[a.type_ for a in d.args]),
    )
    return Def(
        d.name,
        d.return_type,
        args=[Var(tuple_arg.name, decl=True, type=tuple_arg.type_)],
        body=untuple_one_let(
            Let([Var(a.name, a.type_) for a in d.args], tuple_arg, d.body)
        ),
    )


def oneargify_defs(decls: Iterable[ASTNode]) -> List[ASTNode]:
    return [oneargify_def(decl) if isinstance(decl, Def) else decl for decl in decls]
