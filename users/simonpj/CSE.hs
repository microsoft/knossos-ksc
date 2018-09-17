module CSE where

import Lang
import ANF
import qualified Data.Map as M

cseD :: Def -> Def
cseD (Def f args rhs) = Def f args (cseRhs rhs)

cseRhs :: Expr -> Expr
cseRhs e = cseE M.empty M.empty e

cseE :: M.Map Expr Var -> M.Map Var Expr -> Expr -> Expr
cseE cse_env subst (Let v r b)
  | Just v' <- M.lookup r cse_env
  = cseE cse_env (M.insert v (Var v') subst) b
  | otherwise
  = Let v r' $
    cseE (M.insert r' v cse_env) (M.delete v subst) b
  where
    r' = substE subst r

cseE _ subst e = substE subst e

substE :: M.Map Var Expr -> Expr -> Expr
substE subst (Konst k)   = Konst k
substE subst (Var v)     = case M.lookup v subst of
                            Just e  -> e
                            Nothing -> Var v
substE subst (Call f e)  = Call f (substE subst e)
substE subst (Tuple es)  = Tuple (map (substE subst) es)
substE subst (App e1 e2) = App (substE subst e1) (substE subst e2)
substE subst (Lam v e)   = Lam v (substE (v `M.delete` subst) e)
substE subst (Let v r b) = Let v (substE subst r) $
                           substE (v `M.delete` subst) b
