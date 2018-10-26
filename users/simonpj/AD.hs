module AD where

import Lang
import Prim
import Text.PrettyPrint as PP


gradF :: Fun -> Fun
gradF (Fun f) = GradFun f Fwd
gradF f       = error ("gradF: bad function: " ++ show f)

gradV :: Var -> Var
gradV (Simple x) = Grad x Fwd
gradV (StopGrad x) = Grad x Fwd
gradV v          = error ("gradV: bad variable: " ++ PP.render (ppr v))

gradE :: Expr -> Expr
gradE (Konst k)       = lmZero
gradE (Var v)         = Var (gradV v)
gradE (Assert e1 e2)  = Assert e1 (gradE e2)

gradE (Call (Fun (SFun "build")) (Tuple [n, Lam i b]))
  = lmBuild n (Lam i (gradE b))

gradE (Call fun arg) = Call (gradF fun) arg
                           `lmCompose`
                       gradE arg

gradE (Let v e1 e2) = Let v e1                 $
                      Let (gradV v) (gradE e1) $
                      gradE e2

gradE (Tuple es) = lmVCat (map gradE es)
gradE (If b t e) = If b (gradE t) (gradE e)
gradE e@(Lam {}) = pprPanic "gradE: can't deal with lambda yet" (ppr e)

gradDefs :: [Def] -> [Def]
gradDefs = map gradDef

gradDef :: Def -> Def
gradDef (Def fun params rhs)
  = Def (gradF fun) params $
    mkLets [ (gradV param, gradParam param i n)
           | (param, i) <- params `zip` [1..] ] $
    gradE rhs
  where
    n = length params
    gradParam (StopGrad _) _ _= lmZero
    gradParam _ i n = gradSelFun i n 

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
    to_delta (StopGrad x) = Delta x

    dr = Delta "r"

---------------------------------
-- Transpose

transposeD :: Def -> Def
transposeD (Def (GradFun f d) args rhs)
  = Def (GradFun f (flipMode d)) args $
    lmTranspose rhs
