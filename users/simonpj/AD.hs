module AD where

import Lang
import Prim

gradF :: Fun -> Fun
gradF (Simple f) = Grad f
gradF (Grad f)   = error ("Can't grad twice: " ++ f)

gradV :: Var -> Var
gradV (Simple x) = Grad x
gradv (Grad x)   = error ("Can't grad twice: " ++ x)

gradE :: Expr -> Expr
gradE (Konst k) = lmZero
gradE (Var v)   = Var (gradV v)
gradE (Call fun args) = Call (gradF fun) args
                           `lmCompose`
                        gradEs args
gradE (Let v e1 e2) = Let v e1                 $
                      Let (gradV v) (gradE e1) $
                      gradE e2

gradEs :: [Expr] -> Expr
gradEs []      = lmZero
gradEs [e]     = gradE e
gradEs [e1,e2] = gradE e1 `lmPair` gradE e2

gradD :: Def -> Def
gradD (Def fun var rhs)
  = Def (gradF fun) var $
    Let (gradV var) lmOne $
    gradE rhs
