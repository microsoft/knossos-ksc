{-# OPTIONS_GHC -Wno-unused-matches #-}
module OptLet( optLets )  where

import Lang
import qualified Data.Map as M

optLets :: TExpr -> TExpr
optLets = optLetsE . occAnal

----------------------
-- Dead code elimination - occurrence analysis
----------------------

occAnal :: TExpr -> ExprX TFun (Int,TVar)
occAnal e = fst (occAnalE e)

occAnalE :: TExpr -> (ExprX TFun (Int,TVar), M.Map TVar Int)
occAnalE (Var v)   = (Var (1,v), M.singleton v 1)
occAnalE (Konst k) = (Konst k, M.empty)
occAnalE (App e1 e2)
  = (App e1' e2', M.union vs1 vs2)
  where
    (e1', vs1) = occAnalE e1
    (e2', vs2) = occAnalE e2

occAnalE (Assert e1 e2)
  = (Assert e1' e2', M.union vs1 vs2)
  where
    (e1', vs1) = occAnalE e1
    (e2', vs2) = occAnalE e2

occAnalE (Lam v e)
  = (Lam v e', v `M.delete` vs)
  where
    (e', vs) = occAnalE e

occAnalE (Call f e) = (Call f e', vs)
                      where
                        (e',vs) = occAnalE e
occAnalE (Tuple es) = (Tuple es', unions vs)
                      where
                        (es', vs) = unzip (map occAnalE es)
occAnalE (Let var rhs body)
  = (Let (n, var) rhs' body', vs)
  where
    n = case var `M.lookup` vsb of
          Just n  -> n
          Nothing -> 0
    (rhs',  vsr) = occAnalE rhs
    (body', vsb) = occAnalE body
    vs | n == 0    = (var `M.delete` vsb)
       | otherwise = (var `M.delete` vsb) `union` vsr

occAnalE (If b t e)
  = (If b' t' e', vsb `M.union` vst `M.union` vse)
  where
    (b', vsb) = occAnalE b
    (t', vst) = occAnalE t
    (e', vse) = occAnalE e

union :: Ord b => M.Map b Int -> M.Map b Int -> M.Map b Int
union = M.unionWith (+)

unions :: Ord b => [M.Map b Int] -> M.Map b Int
unions = foldr union M.empty

-------------------------
-- Substitute trivials
-------------------------
optLetsE :: ExprX TFun (Int,TVar) -> TExpr
optLetsE e = go M.empty e
  where
    go :: M.Map TVar TExpr -> ExprX TFun (Int,TVar) -> TExpr
    go subst (Let (n, tv@(TVar ty v)) r b)
      | inline_me n v r' = go (M.insert (TVar ty v) r' subst) b
      | otherwise        = Let (TVar ty v) r' (go subst b)
      where
        r' = go subst r

    go subst (Var (_,v))
      = case M.lookup v subst of
          Just e  -> e
          Nothing -> Var v

    go subst (Konst k)      = Konst k
    go subst (Call f e)     = Call f (go subst e)
    go subst (If b t e)     = If (go subst b) (go subst t) (go subst e)
    go subst (Tuple es)     = Tuple (map (go subst) es)
    go subst (App e1 e2)    = App (go subst e1) (go subst e2)
    go subst (Assert e1 e2) = Assert (go subst e1) (go subst e2)
    go subst (Lam v e)      = Lam v (go (v `M.delete` subst) e)

inline_me :: Int -> Var -> TExpr -> Bool
inline_me n bndr rhs
  | n==0            = True
  | n==1            = True  -- ToDo: AWF wrote False, oddly
  | isTrivial rhs   = True
  | Grad {} <- bndr = True
  | otherwise       = False

isTrivial :: TExpr -> Bool
isTrivial (Tuple [])          = True
isTrivial (Var {})            = True
isTrivial (Konst {})          = True
isTrivial (Call _ (Tuple [])) = True
isTrivial (Assert e1 e2)      = isTrivial e2
isTrivial e = False
