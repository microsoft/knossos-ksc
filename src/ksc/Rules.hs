module Rules( Rules, tryRules ) where

import Lang
import Control.Monad( guard )
import Data.Map as M

newtype Rules = Rules [Rule]

data Rule = Rule { ru_lhs :: TExpr
                 , ru_rhs :: TExpr }
  -- You may bind any of the Vars, but not Funs
  -- in ru_lhs to make ru_rhs
  -- That is, there are no "global, in-scope" Vars
  -- If we want them we'll have to add a field to
  -- identify the Vars over which we are quantifying

type TSubst = M.Map (TVar Var) TExpr       -- Substitution for fv(lhs)
type VSubst = M.Map (TVar Var) (TVar Var)  -- Substitution for bv(lhs)

theRules :: Rules
theRules = Rules []

tryRules :: TExpr -> Maybe TExpr
tryRules e
  = case matchRules theRules e of
      []               -> Nothing
      ((rule,subst):_) -> Just (substE subst (ru_rhs rule))
         -- For now, arbitrarily pick the first rule that matches
         -- One could imagine priority schemes (e.g. best-match)

matchRules :: Rules -> TExpr -> [(Rule, TSubst)]
matchRules (Rules rules) e
   = [ (rule, subst)
     | rule <- rules
     , Just subst <- [match (ru_lhs rule) e] ]

match :: TExpr -> TExpr -> Maybe TSubst
match e1 e2 = match_e M.empty M.empty e1 e2

match_e :: VSubst -> TSubst -> TExpr -> TExpr -> Maybe TSubst
match_e vsubst tsubst (Var v) e
  | Just v' <- M.lookup v vsubst
  = do { guard (e == Var v')
       ; return tsubst }

  | Just e2 <- M.lookup v tsubst
  = do { guard (e == e2)
       ; return tsubst }

  | otherwise
  = return (M.insert v e tsubst)

match_e _ tsubst (Konst k1) (Konst k2)
  = do { guard (k1 == k2)
       ; return tsubst }

match_e vsubst tsubst (Call f1 e1) (Call f2 e2)
  = do { guard (f1 == f2)
       ; match_e vsubst tsubst e1 e2 }

match_e vsubst tsubst (App e1a e1b) (App e2a e2b)
  = do { tsubst1 <- match_e vsubst tsubst e1a e2a
       ; match_e vsubst tsubst1 e1b e2b }

match_e vsubst tsubst (Assert e1a e1b) (Assert e2a e2b)
  = do { tsubst1 <- match_e vsubst tsubst e1a e2a
       ; match_e vsubst tsubst1 e1b e2b }

match_e vsubst tsubst (If e1a e1b e1c) (If e2a e2b e2c)
  = do { tsubst1 <- match_e vsubst tsubst  e1a e2a
       ; tsubst2 <- match_e vsubst tsubst1 e1b e2b
       ; match_e vsubst tsubst2 e1c e2c }

match_e vsubst tsubst (Tuple es1) (Tuple es2)
  = match_es vsubst tsubst es1 es2

match_e vsubst tsubst (Lam v1 _ e1) (Lam v2 _ e2)
  = match_e (M.insert v1 v2 vsubst) tsubst e1 e2

match_e vsubst tsubst (Let v1 e1 b1) (Let v2 e2 b2)
  = do { tsubst1 <- match_e vsubst tsubst e1 e2
       ; match_e (M.insert v1 v2 vsubst) tsubst1 b1 b2 }

match_es :: VSubst -> TSubst -> [TExpr] -> [TExpr] -> Maybe TSubst
match_es vs ts []       []       = return ts
match_es vs ts (e1:es1) (e2:es2) = do { ts1 <- match_e vs ts e1 e2
                                      ; match_es vs ts1 es1 es2 }
match_es _ _ _ _ = Nothing