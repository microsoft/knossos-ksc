-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.

{-# LANGUAGE DataKinds #-}

module Shapes where

import Lang
import LangUtils
import Prim
import GHC.Stack
import Data.Maybe(mapMaybe)

shapeDefs :: HasCallStack => [TDef] -> [TDef]
shapeDefs = mapMaybe (shapeDef . noTupPatifyDef)

shapeDef :: HasCallStack => TDef -> Maybe TDef

shapeDef (Def { def_fun = Fun ShapeFun{} _ })
  = Nothing

shapeDef (Def { def_fun = Fun ds f
              , def_pat = VarPat params
              , def_rhs = UserRhs def_rhs
              , def_res_ty = res_ty })
  = Just $
    Def { def_fun    = Fun (ShapeFun ds) f
        , def_pat    = VarPat params
        , def_res_ty = shapeType res_ty
        , def_rhs    = UserRhs (mkLetForShapeOfParameter params (shapeE def_rhs)) }

shapeDef (Def { def_pat = TupPat {} })
  -- TupPat should not appear.  See Note [Replacing TupPat with nested Let] in LangUtils
  = error "shapeDef: TupPat encountered.\nThis should not occur."

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


shapeCall :: HasCallStack => TFun Typed -> TExpr -> TExpr

shapeCall (TFun _ (Fun JustFun (PrimFun (P_SelFun i n)))) e
  = pSel i n (shapeE e)

shapeCall (TFun _ (Fun JustFun (PrimFun f))) e
  | Just e' <- shapeCallPrim f e
  = e'

shapeCall (TFun ty (Fun ds f)) e
  | isBaseUserFun f
  = Call (TFun (shapeType ty) (Fun (ShapeFun ds) f)) e

shapeCall tf e = pShape (Call tf e)  -- Fall back to calling the original function and evaluating the shape of the returned object

shapeCallPrim :: HasCallStack => PrimFun -> TExpr -> Maybe TExpr
shapeCallPrim P_index (Tuple [i, n]) = Just $ pIndex i (shapeE n)
shapeCallPrim P_build (Tuple [sz, Lam i e2]) = Just $ pBuild sz (Lam i (shapeE e2))
shapeCallPrim P_sumbuild (Tuple [sz, Lam i e2]) = Just $ mkLet i (mkZero sz) (shapeE e2)
shapeCallPrim P_sum v = Just $ pIndex (mkZero (pSize v)) (shapeE v)
shapeCallPrim P_constVec (Tuple [sz, v]) = Just $ pConstVec sz (shapeE v)
shapeCallPrim P_deltaVec (Tuple [sz, _i, v]) = Just $ pConstVec sz (shapeE v)
shapeCallPrim P_ts_add (Tuple [lhs, _]) = Just $ shapeE lhs
shapeCallPrim P_ts_scale (Tuple [_, v]) = Just $ shapeE v
shapeCallPrim P_ts_neg v = Just $ shapeE v
shapeCallPrim P_delta (Tuple [_, _, v]) = Just $ shapeE v
shapeCallPrim P_unzip v = Just $ pUnzip (shapeE v)
shapeCallPrim P_trace v = Just $ shapeE v
shapeCallPrim P_copydown v = Just $ shapeE v
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
