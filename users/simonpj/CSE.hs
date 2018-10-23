module CSE where

import Lang
import Text.PrettyPrint
import ANF
import Opt
import KMonad
import qualified Data.Map as M

cseDefs :: [Def] -> KM [Def]
cseDefs defs
  = do { anf_defs <- anfDefs defs
--       ; banner "ANF'd"
--       ; displayN anf_defs

       ; let cse_defs = map cseD anf_defs
--       ; banner "CSE'd"
--       ; displayN anf_defs

             -- cseE turns   let x = e in ..let y = e in ...
             --      into    let x = e in ..let y = x in ...
            -- Then optDefs substitutes x for y

       ; let simpl_defs = optDefs cse_defs
       ; return simpl_defs }

--cseDef :: Uniq -> Def -> (Uniq, Def)
--cseDef u def = runAnf u (cseD def)

---------------------------------
cseD :: Def -> Def
cseD (Def f args rhs) = Def f args (cseE M.empty rhs)

cseE :: M.Map Expr Expr -> Expr -> Expr
cseE cse_env (Let v rhs body)
  | Just rhs'' <- M.lookup rhs' cse_env
  = Let v rhs'' (cseE_check cse_env body)
  | otherwise
  = Let v rhs'  (cseE_check cse_env' body)
  where
    rhs' = cseE cse_env rhs
    cse_env' = M.insert rhs' (Var v) cse_env

cseE cse_env (Assert e1 e2)
 | Call (Fun (SFun "==")) (Tuple [e1a, e1b]) <- e1'
 , let cse_env' = M.map (substAssert e1a e1b) cse_env
 = Assert e1' (cseE cse_env' e2)
 | otherwise
 = Assert e1' (cseE cse_env e2)
 where
   e1' = cseE cse_env e1

cseE cse_env (If e1 e2 e3)
  = If (cseE_check cse_env e1)
       (cseE_check cse_env e2)
       (cseE_check cse_env e3)

cseE cse_env (Call f e)  = Call f (cseE_check cse_env e)
cseE cse_env (Tuple es)  = Tuple (map (cseE_check cse_env) es)
cseE cse_env (App e1 e2) = App (cseE_check cse_env e1)
                               (cseE_check cse_env e2)

cseE cse_env (Lam v e) = Lam v (cseE cse_env e)
  -- Watch out: the variable might capture things in cse_env

cseE _ e = e  -- For now: lambda, app, const, var

cseE_check :: M.Map Expr Expr -> Expr -> Expr
-- Look up the entire expression in the envt
cseE_check cse_env e
  | Just e'' <- M.lookup e' cse_env
  = e''
  | otherwise
  = e'
  where
    e' = cseE cse_env e

substAssert (Var v) e1b = substE (M.insert v e1b M.empty)
substAssert e1a (Var v) = substE (M.insert v e1a M.empty)
substAssert _ _ = \e -> e

------------------------
substE :: M.Map Var Expr -> Expr -> Expr
-- Substitution
substE subst (Konst k)      = Konst k
substE subst (Var v)        = case M.lookup v subst of
                               Just e  -> e
                               Nothing -> Var v
substE subst (Call f e)     = Call f (substE subst e)
substE subst (If b t e)     = If (substE subst b) (substE subst t) (substE subst e)
substE subst (Tuple es)     = Tuple (map (substE subst) es)
substE subst (App e1 e2)    = App (substE subst e1) (substE subst e2)
substE subst (Assert e1 e2) = Assert (substE subst e1) (substE subst e2)
substE subst (Lam v e)      = Lam v (substE (v `M.delete` subst) e)
substE subst (Let v r b)    = Let v (substE subst r) $
                              substE (v `M.delete` subst) b
