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
-- v Changed because pShape n could be unit
optShapePrim P_index (Tuple [i, n])
  | TypeTensor _ e_ty <- typeof n
  = case shapeIsUnit_maybe e_ty of
      Just _ -> Nothing -- already handled by optShape
      Nothing -> Just $ pIndex i (pShape n)
-- v Changed because the shape of the element type could be unit
optShapePrim P_build (Tuple [sz, Lam i e2]) =
  Just $ case shapeType (typeof e2) of
    TypeTuple [] -> sz
    _ -> pBuild sz (Lam i (pShape e2))
-- v ✓
optShapePrim P_sumbuild (Tuple [sz, Lam i e2]) = Just $ mkLet i (mkZero sz) (pShape e2)
-- v ✓
optShapePrim P_sum v = Just $ pIndex (mkZero (pSize v)) (pShape v)
-- v Changed because the shape of v might be unit
optShapePrim P_constVec (Tuple [sz, v]) =
  Just $ case shapeType (typeof v) of
    TypeTuple [] -> sz
    _ -> pConstVec sz (pShape v)
-- v Changed because the shape of v might be unit
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
-- FIXME: Still needs doing
--
-- If we have 'v : Vec (Float, Vec Float)' then
--
-- - unzip v : (Vec Float, Vec (Vec Float))
-- - shape (unzip v) : (Int, Vec Int)
-- - shape v : Vec ((), Int), and
-- - unzip (shape v) :  (Vec (), Vec Int)
--
-- So if we are to optimise by pushing the 'shape' call inside 'unzip'
-- then we will need some special logic for checking whether we get a
-- 'Vec ()' and converting it to an 'Int' (by taking its size).
--
-- v
optShapePrim P_unzip v = Just $ pUnzip (pShape v)
                       -- ^ ?
-- v ✓
optShapePrim P_trace v = Just $ pShape v
-- v ✓
optShapePrim P_copydown v = Just $ pShape v
optShapePrim _ _ = Nothing
