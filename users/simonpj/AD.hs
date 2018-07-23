module AD where

import Lang
import Prim
import Text.PrettyPrint as PP


gradF :: Fun -> Fun
gradF (Fun f) = GradFun f Fwd
gradF f       = error ("gradF: bad function: " ++ show f)

gradV :: Var -> Var
gradV (Simple x) = Grad x Fwd
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

gradE (Tuple es) = lmPair (map gradE es)

gradD :: Def -> Def
gradD (Def fun vars rhs)
  = Def (gradF fun) vars $
    mkLets [ (gradV v, gradSelFun i n)
           | (v, i) <- vars `zip` [1..] ] $
    gradE rhs
  where
    n = length vars

---------------------------------

-- Apply-to-dx
--
-- Local bindings of type (S -o T)
-- are transformed to ones of type T

applyD :: Def -> Def
applyD (Def (GradFun f d) vars rhs)
  | Rev <- d
  = Def (DrvFun f d) (vars ++ [dr]) $
    lmApply rhs (Var dr)

  | otherwise
  = Def (DrvFun f d) (vars ++ dvars) $
    lmApply rhs (mkTuple (map Var dvars))
  where
    dvars = map to_delta vars

    to_delta (Simple x) = Delta x

    dr = Delta "r"

---------------------------------
-- Traanspose

transposeD :: Def -> Def
transposeD (Def (GradFun f d) args rhs)
  = Def (GradFun f (flipMode d)) args $
    lmTranspose rhs
