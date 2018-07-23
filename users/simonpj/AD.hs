module AD where

import Lang
import Prim
import Text.PrettyPrint as PP


gradF :: Fun -> Fun
gradF (Fun f) = GradFun f False
gradF f       = error ("gradF: bad function: " ++ show f)

gradV :: Var -> Var
gradV (Simple x) = Grad x False
gradV v          = error ("gradV: bad varaible: " ++ PP.render (ppr v))

gradE :: Expr -> Expr
gradE (Konst k)       = lmZero
gradE (Var v)         = Var (gradV v)
gradE (Call fun arg) = Call (gradF fun) arg
                           `lmCompose`
                       gradE arg
gradE (Let v e1 e2) = Let v e1                 $
                      Let (gradV v) (gradE e1) $
                      gradE e2

gradE (Tuple [])      = lmZero
gradE (Tuple [e1,e2]) = gradE e1 `lmPair` gradE e2

gradD :: Def -> Def
gradD (Def fun [var] rhs)
  = Def (gradF fun) [var] $
    Let (gradV var) lmOne $
    gradE rhs

---------------------------------

-- Apply-to-dx
--
-- Local bindings of type (S -o T)
-- are transformed to ones of type T

applyD :: Def -> Def
applyD (Def (GradFun f d) [Simple x] rhs)
  = Def (DrvFun f d) [Simple x, Delta s] $
    lmApply rhs (Var (Delta s))
  where
    s | d         = "r"  -- Reverse mode
      | otherwise = x
      

---------------------------------
-- Traanspose

transD :: Def -> Def
transD (Def (GradFun f d) args rhs)
  = Def (GradFun f (not d)) args $
    lmTranspose rhs
