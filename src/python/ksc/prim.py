import re

from ksc.expr import StructuredName
from ksc.type import Type, SizeType

_prim_lookup_re_get = re.compile(r"get\$(\d+)\$(\d+)")


def prim_lookup(sname: StructuredName, argtype: Type) -> Type:
    if argtype.is_tuple:
        tys = tuple(argtype.tuple_elems())
    else:
        tys = [argtype]
    n = len(tys)

    name = sname.mangle_without_type()

    # tuple
    # (tuple 1.0) -> (Tuple Float)
    # (tuple 1.0 2.0) -> (Tuple Float Float)
    # (tuple (tuple 1.0)) -> (Tuple (Tuple Float))

    if name == "tuple":
        return Type.Tuple(*tys)

    # get$n$m
    if name.startswith("get$"):
        assert argtype.is_tuple
        m = _prim_lookup_re_get.fullmatch(name)
        if m:
            n = int(m.group(1))
            max = int(m.group(2))
            assert argtype.tuple_len == max
            return argtype.tuple_elem(n - 1)

    # vec
    if name == "Vec_init":
        if n == 0:
            print("Vec_init: Assuming empty vector is float")
            return Type.Tensor(1, Type.Float)

        assert all(ty == tys[0] for ty in tys)
        return Type.Tensor(1, tys[0])

    # size : Tensor N T -> Size
    if n == 1 and name == "size":
        return SizeType.from_rank(tys[0].tensor_rank)

    if n == 1 and name == "shape":
        return shape_type(tys[0])

    # index : Size, Tensor N T -> T
    if n == 2 and name == "index":
        assert tys[0] == SizeType.from_rank(tys[1].tensor_rank)
        return tys[1].tensor_elem_type

    # build : Size, Lam IntTuple T -> Tensor T
    if n == 2 and name == "build":
        size_ty = tys[0]
        lam_ty = tys[1]
        assert lam_ty.lam_arg_type == size_ty

        rank = SizeType.get_rank(size_ty)
        assert rank is not None

        elem_type = lam_ty.lam_return_type
        return Type.Tensor(rank, elem_type)

    # sumbuild : Size, Lam IntTuple T -> T
    if n == 2 and name == "sumbuild":
        size_ty = tys[0]
        lam_ty = tys[1]
        assert lam_ty.lam_arg_type == size_ty

        return lam_ty.lam_return_type

    # constVec(n T)
    if n == 2 and name == "constVec":
        size_ty = tys[0]
        elem_ty = tys[1]

        rank = SizeType.get_rank(size_ty)
        assert rank is not None

        return Type.Tensor(rank, elem_ty)

    # deltaVec(n i v)
    if n == 3 and name == "deltaVec":
        size_ty = tys[0]
        ind_ty = tys[1]
        elem_ty = tys[2]
        assert size_ty == ind_ty
        rank = SizeType.get_rank(size_ty)
        assert rank is not None

        return Type.Tensor(rank, elem_ty)

    # map : Lam (Tuple S T), Tensor S -> T
    if n == 2 and name == "map":
        assert tys[0].is_lam_or_LM
        tyS = tys[0].lam_arg_type
        tyT = tys[0].lam_return_type
        assert tys[1].is_tensor_of(tyS)
        return Type.Tensor(tys[1].tensor_rank, tyT)

    # fold : Lam (Tuple State T) State, State, Tensor T -> State
    if n == 3 and name == "fold":
        assert tys[0].is_lam_or_LM
        assert tys[0].lam_arg_type.tuple_len == 2
        tyState = tys[0].lam_arg_type.tuple_elem(0)
        tyT = tys[0].lam_arg_type.tuple_elem(1)
        assert tys[0].lam_return_type == tyState
        assert tys[1] == tyState
        assert tys[2].is_tensor_of(tyT)
        return tyState

    # eq : T, T -> Bool
    if n == 2 and name in ("eq", "ne", "lt", "gt", "lte", "gte"):
        assert tys[0] == tys[1]  # TODO: MOVEEQ Move eq to prelude
        return Type.Bool

    # Polymorphic arithmetic
    # sum : Tensor T -> T
    if n == 1 and name == "sum" and tys[0].is_tensor:
        return tys[0].tensor_elem_type

    # ts_add : T, dT -> T
    if n == 2 and name == "ts_add":
        # assert tys[0] == tys[1] TODO: check rhs is tangent_type
        return tys[0]

    # ts_dot : T, T -> Float
    if n == 2 and name == "ts_dot":
        assert tys[0] == tys[1]
        return Type.Float

    # ts_scale : Float, dT -> dT
    if n == 2 and name == "ts_scale":
        assert tys[0] == Type.Float
        return tys[1]

    # print : T... -> Int
    if name == "print":
        return Type.Integer

    return None
