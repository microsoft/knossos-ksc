{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances,
             PatternSynonyms,
	     ScopedTypeVariables #-}

module Opt( optLets, optDef, optDefs, optE, simplify,
            Opt.hspec, test_opt ) where

import Lang
import LangUtils
import ANF
import Prim
import Rules
import OptLet
import KMonad

import Test.Hspec
import qualified Data.Set as S


---------------
optDefs :: HasCallStack => RuleBase -> GblSymTab -> [TDef]
                        -> KM (GblSymTab, [TDef])
-- Returned GblSymTab contains the optimised definitions
optDefs _  gst [] = return (gst, [])
optDefs rb gst (def:defs) = do { (gst1, def')  <- optDef  rb gst def
                               ; (gst2, defs') <- optDefs rb gst1 defs
                               ; return (gst2, def' : defs') }

optDef :: HasCallStack => RuleBase -> GblSymTab -> TDef
                       -> KM (GblSymTab, TDef)
optDef rb gst def@(Def { def_pat = pat, def_rhs = UserRhs rhs })
  = do { -- The variables brought into scope by the argument list are
         -- the names of the arguments themselves (pat)
         let varsBroughtIntoScopeByArgs = mkEmptySubst (patVars pat)
             env = OptEnv { optRuleBase = rb
                          , optGblST = gst
                          , optSubst = varsBroughtIntoScopeByArgs }
       ; rhs' <- simplify env rhs
       ; let def' = def { def_rhs = UserRhs rhs' }
       ; return (extendGblST gst [def'], def') }

optDef _ gst def = return (gst,def)

simplify :: OptEnv -> TExpr -> KM TExpr
simplify env rhs
  = do { let subst = optSubst env

       ; rhs1 <- runAnf (anfExpr subst rhs)
--       ; banner "ANF'd (1)"
--       ; display rhs1

       ; let rhs2 = optLets subst rhs1
--       ; banner "OptLets (1)"
--       ; display rhs2

       ; let rhs3 = optE env rhs2
--       ; banner "OptE (1)"
--       ; display rhs3

       ; rhs4 <- runAnf (anfExpr subst rhs3)
--       ; banner "ANF'd (2)"
--       ; display rhs4

       ; let rhs5 = optLets subst rhs4
--       ; banner "OptLets (2)"
--       ; display rhs5

       ; let rhs6 = optE env rhs5
--       ; banner "OptE (2)"
--       ; display rhs6

          -- We run optLets again at the end of the simplify pass to ensure
          -- that variables have unique names.  See
          --
          --     https://github.com/awf/knossos/pull/386#issuecomment-476586918
       ; let rhs7 = optLets subst rhs6
--       ; banner "OptLets (3)"
--       ; display rhs7

       ; return rhs7 }

---------------
optE :: HasCallStack => OptEnv -> TExpr -> TExpr
optE env
  = go
  where
    go :: HasCallStack => TExpr -> TExpr
    go e | Just e' <- tryRules (optRuleBase env) e
         = go e'

    go (Tuple es)         = Tuple (map go es)
    go (Var v) | Just e <- lookupSubst (tVarVar v) (optSubst env)
               = e
               | otherwise = Var v
    go e@(Konst _)        = e
    go e@(Dummy _)        = e
    go (App e1 e2)        = optApp (go e1) (go e2)
    go (Assert e1 e2)     = Assert (go e1) (go e2)
    go (Lam tv e)         = Lam tv' (optE env' e)
       where
         (tv', env') = optSubstBndr tv env
    go (Let tv rhs body)  = mkLet tv' (go rhs) (optE env' body)
       where
         (tv', env') = optSubstBndr tv env
    go (If b t e)    = optIf (go b) (go t) (go e)
    go (Call f arg)  = optCall (optZapSubst env) f (go arg)
                       -- Simplify the args, and then try to rewrite the call

--------------
optCall :: OptEnv -> TFun -> TExpr -> TExpr
optCall env fun opt_args
  | Just new_e <- rewriteCall env fun opt_args
--  = pprTrace "Rule fired:" (vcat [ text "Before:" <+> ppr (Call fun opt_args)
--                                 , text "After: " <+> ppr new_e ])
    = optE env new_e
  | otherwise
  = Call fun opt_args

--------------
optApp :: TExpr -> TExpr -> TExpr
optApp (Lam v e) a = Let v a e
optApp f a         = App f a

--------------
optIf :: TExpr -> TExpr -> TExpr -> TExpr
optIf (Konst (KBool True))  t _ = t
optIf (Konst (KBool False)) _ e = e
optIf (Let v r b)           t e = Let v r   (optIf b t e)
optIf (Assert e1 e2)        t e = Assert e1 (optIf e2 t e)
optIf e_cond e_then e_else
  | Just (ei, ej) <- isEqualityCall e_cond
  , isKZero e_else
  = pDelta ei ej e_then
optIf b                     t e = If b t e

--------------
-- 'rewriteCall' performs one rewrite, returning (Just e) if
-- it succeeds in rewriting, and Nothing if not
--
-- The same goes for optFun, optGradFun, etc

rewriteCall :: HasCallStack => OptEnv -> TFun -> TExpr -> Maybe TExpr

-- RULE: f( let x = e in b )  =  let x = e in f(b)
-- Just for unary functions so far
-- Could do this for n-ary functions but beware shadowing
rewriteCall _ fun (Let v r arg)
  = Just (Let v r (Call fun arg))

-- RULE: f( if e1 then e2 else e3 )  =  if e1 then f(e2) else f(e3)
-- Again unary functions only (notably fst, snd)
-- For nary functions worry about code duplication.
rewriteCall _ fun (If e1 e2 e3)
  = Just (If e1 (Call fun e2) (Call fun e3))

rewriteCall env (TFun res_ty fun) arg
  = case funIdOfFun fun of
      PrimFun f -> rewritePrimCall env f fun arg res_ty
      UserFun f -> rewriteUserCall env f fun arg

-- Inline any white-listed function
rewriteUserCall :: OptEnv -> UserFunName -> Fun -> TExpr -> Maybe TExpr
rewriteUserCall env f fun arg
  | f `S.member` inlineWhiteList
  , let arg_ty = typeof arg
  , Just fun_def <- lookupGblST (fun, arg_ty) (optGblST env)
  , Def { def_pat = pat, def_rhs = UserRhs body } <- fun_def
  = Just (inlineCall (optEnvInScope env) pat body arg)
  | otherwise
  = Nothing


inlineWhiteList :: S.Set UserFunName
inlineWhiteList = S.fromList [ "add", "mul" ]

--------------------------------------
hspec :: Spec
hspec = do
    describe "optLM tests" $ do
      it "lmAdd(S(x),S(y)) -> S(x+y)" $
        optLMAdd (Fun (PrimFun (Builtin "lmAdd")))
                 (lmScale TypeFloat (kTFloat 1.3))
                 (lmScale TypeFloat (kTFloat 0.4))
        `shouldBe`
        Just (lmScale TypeFloat (pTsAdd (kTFloat 1.3) (kTFloat 0.4)))

      it "lmAdd(HCat) = HCat(lmAdd) and some more simplifications" $
        let l1 = lmOne TypeFloat
            f2 = kTFloat 2.0
            f4 = kTFloat 4.0
            l2 = lmScale TypeFloat f2
        in
            optE emptyOptEnv
                 (lmAdd (lmHCat [l1, l2]) (lmHCat [l2, l2]))
            `shouldBe`
            lmHCat [lmAdd l1 l2, lmScale TypeFloat f4]

test_opt:: IO ()
test_opt = Test.Hspec.hspec Opt.hspec
