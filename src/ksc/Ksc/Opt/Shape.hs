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

optShapePrim :: PrimFun -> TExpr -> Maybe TExpr
optShapePrim (P_SelFun i n) e = Just $ pSel i n (pShape e)
optShapePrim P_index (Tuple [i, n]) = Just $ pIndex i (pShape n)
optShapePrim P_build (Tuple [sz, Lam i e2]) = Just $ pBuild sz (Lam i (pShape e2))
optShapePrim P_sumbuild (Tuple [sz, Lam i e2]) = Just $ mkLet i (mkZero sz) (pShape e2)
optShapePrim P_sum v = Just $ pIndex (mkZero (pSize v)) (pShape v)
optShapePrim P_constVec (Tuple [sz, v]) = Just $ pConstVec sz (pShape v)
optShapePrim P_deltaVec (Tuple [sz, _i, v]) = Just $ pConstVec sz (pShape v)
optShapePrim P_ts_add (Tuple [lhs, _]) = Just $ pShape lhs
optShapePrim P_ts_scale (Tuple [_, v]) = Just $ pShape v
optShapePrim P_ts_neg v = Just $ pShape v
optShapePrim P_delta (Tuple [_, _, v]) = Just $ pShape v
optShapePrim P_unzip v = Just $ pUnzip (pShape v)
optShapePrim P_trace v = Just $ pShape v
optShapePrim P_copydown v = Just $ pShape v
optShapePrim _ _ = Nothing
