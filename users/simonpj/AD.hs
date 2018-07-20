module AD where

import Lang
import Prim

gradF :: Fun -> Fun
gradF (Simple f) = Grad f
gradF (Grad f)   = error ("Can't grad twice: " ++ f)

gradV :: Var -> Var
gradV (Simple x) = Grad x
gradv (Grad x)   = error ("Can't grad twice: " ++ x)

gradE :: Var -> Expr -> Expr
gradE _ (Const k) = lmZero
gradE _ (Var v)   = Var (gradV v)
gradE x (Call fun arg) = Call (gradF fun) [Var x]
                           `lmCompose`
                         gradE x arg
gradE x (Let v e1 e2) = Let (gradV v) (gradE x e1) (gradE x e2)

gradD :: Bind -> Bind
gradD (Bind fun var rhs)
  = Bind (gradF fun) var $
    Let (gradV var) lmOne $
    gradE var rhs
