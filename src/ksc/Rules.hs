-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Rules( RuleBase, tryRules, mkRuleBase ) where

import Lang
import Control.Monad( guard )
import Data.Map as M

newtype RuleBase = Rules [TRule]
        -- Rule is defined in Lang

type TSubst = M.Map TVar TExpr -- Substitution for fv(lhs)
type VSubst = M.Map TVar TVar  -- Substitution for bv(lhs)

mkRuleBase :: [TRule] -> RuleBase
mkRuleBase = Rules

tryRules :: RuleBase -> TExpr -> Maybe TExpr
tryRules rules e
  = case matchRules rules e of
      []               -> Nothing
      ((rule,subst):_) -> -- pprTrace ("Rule fired: " ++ ru_name rule)
                          --   (vcat [ text "Before:" <+> ppr e
                          --         , text "After: " <+> ppr e' ]) $
                          Just e'
        where
         e' = mkLets (M.toList subst) (ru_rhs rule)
         -- For now, arbitrarily pick the first rule that matches
         -- One could imagine priority schemes (e.g. best-match)
         --
         -- Use lets, not substE, so that optLetsE will guarantee
         -- capture-avoiding substitution

matchRules :: RuleBase -> TExpr -> [(TRule, TSubst)]
matchRules (Rules rules) e
   = [ (rule, subst)
     | rule <- rules
     , Just subst <- [match (ru_lhs rule) e] ]

match :: TExpr -> TExpr -> Maybe TSubst
match = match_e M.empty M.empty

match_e :: VSubst -> TSubst -> TExpr -> TExpr -> Maybe TSubst
match_e vsubst tsubst (Var v) e
  | Just v' <- M.lookup v vsubst
  = do { guard (e == Var v')
       ; return tsubst }

  | Just e2 <- M.lookup v tsubst
  = do { guard (e == e2)
       ; return tsubst }

  | typeof v == typeof e
  = return (M.insert v e tsubst)

  | otherwise
  = Nothing

match_e _ tsubst (Konst k1) (Konst k2)
  = do { guard (k1 == k2)
       ; return tsubst }

match_e vsubst tsubst (Call f1 es1) (Call f2 es2)
  = do { guard (f1 == f2)
       ; match_e vsubst tsubst es1 es2 }

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

match_e vsubst tsubst (Lam v1 e1) (Lam v2 e2)
  = match_e (M.insert v1 v2 vsubst) tsubst e1 e2

match_e vsubst tsubst (Let v1 e1 b1) (Let v2 e2 b2)
  = do { tsubst1 <- match_e vsubst tsubst e1 e2
       ; match_e (M.insert v1 v2 vsubst) tsubst1 b1 b2 }

match_e _ _ _ _ = Nothing

match_es :: VSubst -> TSubst -> [TExpr] -> [TExpr] -> Maybe TSubst
match_es vs ts []       []       = return ts
match_es vs ts (e1:es1) (e2:es2) = do { ts1 <- match_e vs ts e1 e2
                                      ; match_es vs ts1 es1 es2 }
match_es _ _ _ _ = Nothing
