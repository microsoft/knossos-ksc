module Ksc.Opt.Shape where

import Lang
import Prim
import Shapes

optShape :: TExpr -> Maybe TExpr

optShape e
  | Just unit <- shapeIsUnit_maybe (typeof e)
  = Just unit

optShape (Dummy ty)     = Just $ Dummy (Lang.shapeType ty)
optShape (Assert e1 e2) = Just $ Assert e1 (pShape e2)
optShape (Tuple es)     = Just $ Tuple (map pShape es)
optShape (If b t e)     = Just $ If b (pShape t) (pShape e)

optShape (Let (VarPat v) e1 e2) = Just $ Let (VarPat v) e1 (pShape e2)
optShape (Let (TupPat p) e1 e2) = Just $ Let (TupPat p) e1 (pShape e2)

optShape (Call (TFun _ (Fun JustFun (PrimFun p))) arg) = optShapePrim p arg
optShape (Call (TFun ty (Fun ds f)) e) = Just $ Call (TFun (shapeType ty) (Fun (ShapeFun ds) f)) e
optShape (Konst _)      = Nothing
-- Shape of constant: should not occur as this is handled by the unit-shape case
optShape Var{}          = Nothing
-- Shape of Lam not supported
optShape (Lam{})        = Nothing
-- Shape of App not supported
optShape (App{})        = Nothing

-- ✓ = no change required to accommodate Integer as shape type for Vec <scalar>
-- (and n-tuple of Integer as shape type for Tensor n <scalar>)
optShapePrim :: PrimFun -> TExpr -> Maybe TExpr
-- v ✓
optShapePrim (P_SelFun i n) e = Just $ pSel i n (pShape e)
-- v Needs to change beacuse pShape n could be unit
optShapePrim P_index (Tuple [i, n]) = Just $ pIndex i (pShape n)
                                    -- ^ ?
-- v Needs to change because the shape of the element type could be unit
optShapePrim P_build (Tuple [sz, Lam i e2]) =
  Just $ case shapeType (typeof e2) of
    TypeTuple [] -> sz
    _ -> pBuild sz (Lam i (pShape e2))
-- v ✓
optShapePrim P_sumbuild (Tuple [sz, Lam i e2]) = Just $ mkLet i (mkZero sz) (pShape e2)
-- v ✓
optShapePrim P_sum v = Just $ pIndex (mkZero (pSize v)) (pShape v)
-- v Needs to change because the shape of v might be unit
optShapePrim P_constVec (Tuple [sz, v]) =
  Just $ case shapeType (typeof v) of
    TypeTuple [] -> sz
    _ -> pConstVec sz (pShape v)
-- v Needs to change because the shape of v might be unit
optShapePrim P_deltaVec (Tuple [sz, _i, v]) =
  Just $ case shapeType (typeof v) of
    TypeTuple [] -> sz
    _ -> pConstVec sz (pShape v)
-- v ✓
optShapePrim P_ts_add (Tuple [lhs, _]) = Just $ pShape lhs
-- v ✓
optShapePrim P_ts_scale (Tuple [_, v]) = Just $ pShape v
-- v ✓
optShapePrim P_ts_neg v = Just $ pShape v
-- v ✓
optShapePrim P_delta (Tuple [_, _, v]) = Just $ pShape v
-- Suppose we have Vec (Float, Vec Float)
--
-- Then the shape will have type Vec ((), Int)
--
-- So the unzip of the shape will have type (Vec (), Vec Int)
--
-- So perhaps we need still to be able to interpret Vec () as a shape
-- type for Vec Float.
--
-- v
optShapePrim P_unzip v = Just $ pUnzip (pShape v)
                       -- ^ ?
-- v ✓
optShapePrim P_trace v = Just $ pShape v
-- v ✓
optShapePrim P_copydown v = Just $ pShape v
optShapePrim _ _ = Nothing
