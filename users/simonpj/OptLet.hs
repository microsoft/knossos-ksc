module OptLet( optLets )  where

import Lang
import Prim
import Text.PrettyPrint as PP
import qualified Data.Set as S
import qualified Data.Map as M

optLets :: Expr -> Expr
optLets = optLetsE . occAnal

----------------------
-- Dead code elimination
----------------------

occAnal :: Expr -> ExprX (Int,Var)
occAnal e = fst (occAnalE e)

occAnalE :: Expr -> (ExprX (Int,Var), M.Map Var Int)
occAnalE (Var v)   = (Var v, M.singleton v 1)
occAnalE (Konst k) = (Konst k, M.empty)
occAnalE (App e1 e2)
  = (App e1' e2', M.union vs1 vs2)
  where
    (e1', vs1) = occAnalE e1
    (e2', vs2) = occAnalE e2

occAnalE (Lam v e)
  = (Lam (n,v) e', v `M.delete` vs)
  where
    (e', vs) = occAnalE e
    n = case v `M.lookup` vs of
          Just n  -> n
          Nothing -> 0

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

union :: M.Map Var Int -> M.Map Var Int -> M.Map Var Int
union = M.unionWith (+)

unions :: [M.Map Var Int] -> M.Map Var Int
unions = foldr union M.empty

-------------------------
-- Substitute trivials
-------------------------
optLetsE :: ExprX (Int,Var) -> Expr
optLetsE e = go M.empty e
  where
    go :: M.Map Var Expr -> ExprX (Int,Var) -> Expr
    go subst (Let (n,v) r b)
      | inline_me n v r' = go (M.insert v r' subst) b
      | otherwise        = Let v r' (go subst b)
      where
        r' = go subst r

    go subst (Var v)
      = case M.lookup v subst of
          Just e  -> e
          Nothing -> Var v

    go subst (Konst k)     = Konst k
    go subst (Call f e)    = Call f (go subst e)
    go subst (If b t e)    = If (go subst b) (go subst t) (go subst e)
    go subst (Tuple es)    = Tuple (map (go subst) es)
    go subst (App e1 e2)   = App (go subst e1) (go subst e2)
    go subst (Lam (_,v) e) = Lam v (go (v `M.delete` subst) e)

inline_me :: Int -> Var -> Expr -> Bool
inline_me n bndr rhs
  | n==0            = True
  | n==1            = True
  | isTrivial rhs   = True
  | Grad {} <- bndr = True
  | otherwise       = False

isTrivial :: Expr -> Bool
isTrivial (Tuple [])          = True
isTrivial (Var {})            = True
isTrivial (Konst {})          = True
isTrivial (Call _ (Tuple [])) = True
isTrivial e = False
