-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.

module Shapes where

import Lang
import LangUtils
import Prim
import GHC.Stack
import Data.Maybe(mapMaybe)

shapeDefs :: HasCallStack => [TDef] -> [TDef]
shapeDefs = mapMaybe (shapeDef . noTupPatifyDef)

shapeDef :: HasCallStack => TDef -> Maybe TDef

shapeDef (Def { def_fun = ShapeFun _ })
  = Nothing

shapeDef (Def { def_fun = f
              , def_arg = params
              , def_rhs = UserRhs def_rhs
              , def_res_ty = res_ty })
  = Just $
    Def { def_fun    = ShapeFun f
        , def_arg    = params
        , def_res_ty = shapeType res_ty
        , def_rhs    = UserRhs (mkLetForShapeOfParameter params (shapeE def_rhs)) }

shapeDef _ = Nothing


{- Note [Variables in the shape transform]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For each variable v in the original function (including the function parameter),
the shape transform generates two variables, v and varshape$v, such that
varshape$v is the shape of v. Normally one of these variables will turn out to
be unused, and will eventually be optimized away.
-}

shapeVar :: TVar -> TVar
shapeVar tv = mkTVar (shapeType (tVarType tv)) ("varshape$" ++ nameOfVar (tVarVar tv))

-- Generate a variable for the shape of the function parameter:
-- see note [Variables in the shape transform]
mkLetForShapeOfParameter :: TVar -> TExpr -> TExpr
mkLetForShapeOfParameter param = mkLet (shapeVar param) (pShape (Var param))

shapeE :: HasCallStack => TExpr -> TExpr

shapeE e
  | Just unit <- shapeIsUnit_maybe (typeof e)
  = unit

shapeE (Konst _)      = error "Shape of constant: should not occur as this is handled by the unit-shape case"
shapeE (Var tv)       = Var (shapeVar tv)
shapeE (Dummy ty)     = Dummy (shapeType ty)
shapeE (Assert e1 e2) = Assert e1 (shapeE e2)
shapeE (Tuple es)     = Tuple (map shapeE es)
shapeE (If b t e)     = If b (shapeE t) (shapeE e)

-- Shape of Let: see Note [Variables in the shape transform]
shapeE (Let (VarPat v) e1 e2) = mkLets [(v, e1), (shapeVar v, shapeE e1)] (shapeE e2)
shapeE (Let (TupPat _) _ _) = error "shapeE: TupPat should not occur"

shapeE (Call tf e)    = shapeCall tf e
shapeE (Lam{})        = error "Shape of lambda not supported"
shapeE (App{})        = error "Shape of App not supported"


shapeCall :: HasCallStack => TFun -> TExpr -> TExpr

shapeCall (TFun _ (Fun (SelFun i n))) e
  = pSel i n (shapeE e)

shapeCall (TFun _ (Fun (PrimFun f))) e
  | Just e' <- shapeCallPrim f e
  = e'

shapeCall (TFun ty f) e
  | isUserFun (funIdOfFun f)
  = Call (TFun (shapeType ty) (ShapeFun f)) e

shapeCall tf e = pShape (Call tf e)  -- Fall back to calling the original function and evaluating the shape of the returned object

shapeCallPrim :: HasCallStack => PrimFun -> TExpr -> Maybe TExpr
shapeCallPrim "index" (Tuple [i, n]) = Just $ pIndex i (shapeE n)
shapeCallPrim "build" (Tuple [sz, Lam i e2]) = Just $ pBuild sz (Lam i (shapeE e2))
shapeCallPrim "sumbuild" (Tuple [n, Lam i e2])
  = fmap (\d -> mkLet i (zeroIndexForDimension d) (shapeE e2)) (tensorDimensionFromIndexType_maybe (typeof n))
shapeCallPrim "sum" v
  | TypeTensor d _ <- typeof v
  = Just $ pIndex (zeroIndexForDimension d) (shapeE v)
shapeCallPrim "constVec" (Tuple [sz, v]) = Just $ pConstVec sz (shapeE v)
shapeCallPrim "deltaVec" (Tuple [sz, _i, v]) = Just $ pConstVec sz (shapeE v)
shapeCallPrim "ts_add" (Tuple [lhs, _]) = Just $ shapeE lhs
shapeCallPrim "ts_scale" (Tuple [_, v]) = Just $ shapeE v
shapeCallPrim "ts_neg" v = Just $ shapeE v
shapeCallPrim "delta" (Tuple [_, _, v]) = Just $ shapeE v
shapeCallPrim "unzip" v = Just $ pUnzip (shapeE v)
shapeCallPrim "$trace" v = Just $ shapeE v
shapeCallPrim "$copydown" v = Just $ shapeE v
shapeCallPrim _ _ = Nothing

-- Given a Type t, determines whether (shapeType t) is a unit type;
-- if so then the unit value is returned.
shapeIsUnit_maybe :: Type -> Maybe TExpr
shapeIsUnit_maybe TypeInteger = Just (Tuple [])
shapeIsUnit_maybe TypeBool = Just (Tuple [])
shapeIsUnit_maybe TypeFloat = Just (Tuple [])
shapeIsUnit_maybe TypeString = Just (Tuple [])
shapeIsUnit_maybe (TypeTuple ts) = fmap Tuple (traverse shapeIsUnit_maybe ts)
shapeIsUnit_maybe _ = Nothing
