{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances,
             PatternSynonyms,
	     ScopedTypeVariables #-}

module Opt( optLets, optDef, optDefs, optE, Opt.hspec, simplify, test_opt ) where

import Lang
import LangUtils
import ANF
import Prim
import Rules
import OptLet
import KMonad
import qualified Ksc.SUF.Rewrite as SUF
import Ksc.Traversal( traverseState, mapAccumLM )

import Debug.Trace
import Test.Hspec
import Data.List( mapAccumR )
import Data.Sequence( mapWithIndex, fromList )
import Data.Foldable( toList )

optTrace :: String -> a -> a
optTrace _msg t = t
-- optTrace msg t = trace msg t

data OptEnv = OptEnv { optRuleBase :: RuleBase
                     , optGblST    :: GblSymTab
                     , optSubst    :: Subst }

optEnvInScope :: OptEnv -> InScopeSet
optEnvInScope env = substInScope (optSubst env)

emptyOptEnv :: OptEnv
emptyOptEnv = OptEnv { optRuleBase = mkRuleBase []
                     , optGblST    = emptyGblST
                     , optSubst    = mkEmptySubst [] }

optZapSubst :: OptEnv -> OptEnv
optZapSubst env@(OptEnv { optSubst = subst })
  = env { optSubst = zapSubst subst }

optSubstBndr :: TVar -> OptEnv -> (TVar, OptEnv)
optSubstBndr tv env@(OptEnv { optSubst = subst })
  = (tv', env { optSubst = subst' })
  where
    (tv', subst') = substBndr tv subst

---------------
optDefs :: HasCallStack => RuleBase -> GblSymTab -> [TDef]
                        -> KM (GblSymTab, [TDef])
-- Returned GblSymTab contains the optimised definitions
optDefs = mapAccumLM . optDef

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
       ; return (stInsertFun def' gst, def') }

optDef _ gst def = return (gst,def)

simplify :: OptEnv -> TExpr -> KM TExpr
simplify env rhs
  = do { let subst = optSubst env

       -- We use ANF to expose optimisation opportunities and use
       -- optLets and optE to take them.  See Note [Inline tuples] for
       -- the motiviation for doing ANF-then-opt.
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
    go e | Just e' <- tryRules (optRuleBase env) e = go e'

    go (Tuple es)         = Tuple (map go es)
    go (Var v) | Just e <- lookupSubst (tVarVar v) (optSubst env)
               = e
               | otherwise = Var v
    go e@(Konst _)        = e
    go e@(Dummy _)        = e
    go (App e1 e2)        = optApp env (go e1) (go e2)
    go (Assert e1 e2)     = Assert (go e1) (go e2)
    go (Lam tv e)         = Lam tv' (optE env' e)
       where
         (tv', env') = optSubstBndr tv env
    go (Let tv rhs body)  = Let tv' (go rhs) (optE env' body)
       where
         (tv', env') = traverseState optSubstBndr tv env
    go (If b t e)         = optIf (go b) (go t) (go e)
    go (Call f arg)       = optCall (optZapSubst env) f (go arg)

--------------
optCall :: OptEnv -> TFun Typed -> TExpr -> TExpr
optCall env fun opt_args
  | Just new_e <- rewriteCall env fun opt_args
  -- = pprTrace "Rule fired:" (vcat [ text "Before:" <+> ppr (Call fun opt_args)
  --                                , text "After: " <+> ppr new_e ])
  = let before = Call fun opt_args
        after = new_e
        type_before = typeof before
        type_after = typeof after
    in if type_before == type_after
       then optE env new_e
       else
         pprPanic "Rule changed type:"
           (vcat [ text "Before:" <+> ppr before
                 , text "After:"  <+> ppr after
                 , text "Type before:" <+> ppr type_before
                 , text "Type after:"  <+> ppr type_after ])
  | otherwise
  = Call fun opt_args

--------------
optApp :: OptEnv -> TExpr -> TExpr -> TExpr
optApp env (Lam v e) a = mkLet v (optE env a) (optE env e)
optApp _ f a         = App f a

--------------
optIf :: TExpr -> TExpr -> TExpr -> TExpr
optIf (Konst (KBool True))  t _ = t
optIf (Konst (KBool False)) _ e = e
optIf (Let v r b)           t e = Let v r   (optIf b t e)
optIf (Assert e1 e2)        t e = Assert e1 (optIf e2 t e)
optIf e_cond e_then e_else
  | e_then == e_else
  -- See Note [Conditionals with equal branches]
  -- NB: (==) on expressions does equality modulo alpha (see Lang.hs)
  = e_then
  | Just (ei, ej) <- isEqualityCall e_cond
  , isTensorIndexType (typeof ei)
  , isKZero e_else
  = pDelta ei ej e_then
optIf b                     t e = If b t e

{- Note [Conditionals with equal branches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples of this are sometimes produced by AD, e.g.
    (def rev$not_ (Tuple)  (_t1 : (Tuple Bool (Tuple)))
            (if (get$1$2 _t1) (tuple) (tuple)))

which we would like to simplify to
    (def rev$not_ (Tuple) (_t1 : (Tuple Bool (Tuple))) (tuple))

This pattern becomes especially common when defining shape$ functions,
because for most conditionals the shape of the result does not depend
on the condition.
-}

--------------
-- 'rewriteCall' performs one rewrite, returning (Just e) if
-- it succeeds in rewriting, and Nothing if not
--
-- The same goes for optFun, optGradFun, etc

rewriteCall :: HasCallStack => OptEnv -> TFun Typed -> TExpr -> Maybe TExpr

-- RULE: f( let x = e in b )  =  let x = e in f(b)
rewriteCall _ fun (Let v r arg)
  = Just (Let v r (Call fun arg))

-- RULE: f( if e1 then e2 else e3 )  =  if e1 then f(e2) else f(e3)
rewriteCall _ fun (If e1 e2 e3)
  = Just (If e1 (Call fun e2) (Call fun e3))

rewriteCall env (TFun { tf_fun = Fun JustFun fun }) arg
  = optFun env fun arg

rewriteCall env (TFun { tf_fun = Fun (GradFun adm) f, tf_ret = ty }) arg
  = optGradFun (optEnvInScope env) adm ty f arg

rewriteCall _ (TFun { tf_fun = Fun (DrvFun adm) f }) arg
  = optDrvFun adm f arg

rewriteCall _ f@(TFun { tf_ret = TypeLM {} }) _
  = trace ("NOTE: Unmatched LM call {" ++ pps f ++ "}") Nothing

rewriteCall env (TFun { tf_fun = to_inline, tf_targs = targs }) arg
  | Just to_inline <- maybeUserFun to_inline
  , shouldInline to_inline
  , Just def <- lookupDef to_inline (optGblST env)
  = inlineCall env def targs arg

rewriteCall _ (TFun { tf_fun = Fun SUFFwdPass (PrimFun fun) }) arg
  = SUF.rewriteSUFFwdPass fun arg

rewriteCall _ (TFun { tf_fun = Fun SUFRevPass (PrimFun fun) }) arg
  = SUF.rewriteSUFRevPass fun arg

rewriteCall _ _ _
  = Nothing

-- If this gets big or complicated then move it to its own module,
-- perhaps Ksc.InlineHeuristics.
shouldInline :: UserFun Typed -> Bool
shouldInline to_inline
  = to_inline `elem` [ fwd "mul" ff
                     , rev "mul" ff
                     , fwd "add" ff
                     , rev "add" ff
                     ]
  where
    fwd f t = Fun SUFFwdPass (BaseUserFunId f t)
    rev f t = Fun SUFRevPass (BaseUserFunId f t)
    ff = Mono (TypeTuple [TypeFloat, TypeFloat])

-----------------------
optFun :: OptEnv -> BaseFun p -> TExpr -> Maybe TExpr

-- RULE:  sel_i_n (..., ei, ...)  ==>  ei
optFun _ (PrimFun (P_SelFun i _)) arg
  | Tuple es <- arg
  , i <= length es
  = Just (es !! (i-1))

  | otherwise
  = Nothing

-- $inline needs to look up the global symtab
optFun env (PrimFun P_inline) arg
  | Call (TFun { tf_fun = fun, tf_targs = targs }) inner_arg <- arg
  , Just userFun <- maybeUserFun fun
  , Just fun_def <- lookupDef userFun (optGblST env)
  = inlineCall env fun_def targs inner_arg

-- Other prims are determined by their args
optFun env (PrimFun f) e
  = optPrimFun (optEnvInScope env) f e

optFun _ (BaseUserFun {}) _
  = Nothing

-----------------------
optPrimFun :: InScopeSet -> PrimFun -> TExpr -> Maybe TExpr

-- Constant folding.
-- TODO: match precision to target machine

-- Don't try to constant-fold Vec_init
optPrimFun _ P_Vec_init _args 
  = Nothing 

optPrimFun _ op (Tuple [Konst (KFloat k1), Konst (KFloat k2)])
  = Just . Konst . KFloat $
    case op of
      P_ts_add -> k1 + k2
      P_ts_scale -> k1 * k2
      s -> errorFor s
  where errorFor s = error $ unlines $
          [ "Failed constant folding [" ++ render (ppr s) ++ "]."
          , "This error exists to prompt us"
          , "to add a constant folding rule for"
          , "this operation to ksc.  See also"
          , ""
          , "    https://github.com/microsoft/knossos-ksc/pull/61/commits/29c2ab04568e17b953d3fe942aba5881ab15e1f8#r309892713"
          ]

-- RULE: (e1 : ()) + (e2 : ()) = ()
-- The type () contains only one value (), which is a zero of the type
-- We use () as the tangent type for non-differentiatable types
optPrimFun _ P_ts_add (Tuple [e1, e2])
  | TypeTuple [] <- typeof e1
  , TypeTuple [] <- typeof e2
  = Just (Tuple [])

-- RULE: (a1,a2) + (b1,b2) = (a1+a2, b1+b2)
optPrimFun _ P_ts_add (Tuple [Tuple es1, Tuple es2])
  | length es1 == length es2
  = Just (Tuple (zipWith pAdd es1 es2))

-- RULE: x+0 = 0+x = x
optPrimFun _ P_ts_add (Tuple [x, y]) =
    if isKZero y then
      Just x
    else if isKZero x then
      Just y
    else
      Nothing

-- RULE: scale 0 y = 0
optPrimFun _ P_ts_scale (Tuple [x, y])
  | isKZero x || isKZero y
  -- We use the type of y because the two typing rule for scale in
  -- Prim.hs is
  --
  -- scale: (Float, t) -> t
  = Just $ mkZero y
  | otherwise
  = Nothing

-- RULE: dot 0 y = 0
optPrimFun _ P_ts_dot (Tuple [x, y])
  | isKZero x || isKZero y
  = Just $ zeroFloat
  | otherwise
  = Nothing

-- RULE: size (build (n, _)) = n
optPrimFun _ P_size (Call build (Tuple [n,_]))
  | build `isThePrimFun` P_build
  = Just n

-- RULE: size (constVec (n, _)) = n
optPrimFun _ P_size (Call constVec (Tuple [n,_]))
  | constVec `isThePrimFun` P_constVec
  = Just n

-- RULE: index ei (build ns f) = f ei
optPrimFun _ P_index (Tuple [ ei, arr ])
  | Just (_, i, e) <- isBuild_maybe arr
  = Just (mkLet i ei e)

-- RULE: index js (constVec (ns, v)) = v
optPrimFun _ P_index (Tuple [_, Call constVec (Tuple [_, v])])
  | constVec `isThePrimFun` P_constVec
  = Just v

-- RULE: sum (build n (\i. if (i==ej) then v else 0)
--  = let i = ej in v

-- RULE: deltaVec n i 0 = zero (build n (\i . 0))
optPrimFun _ P_deltaVec (Tuple [n, _i, val])
  | isKZero val
  = Just $ pConstVec n val

optPrimFun _ P_sum         arg           = optSum arg
optPrimFun _ P_build       (Tuple [sz, Lam i e2]) = optBuild sz i e2
optPrimFun _ P_sumbuild    (Tuple [sz, Lam i e2]) = optSumBuild sz i e2
optPrimFun env P_lmApply   (Tuple [e1,e2])        = optLMApply env (AD BasicAD Fwd) e1 e2
optPrimFun env P_lmApplyR  (Tuple [e1,e2])        = optLMApply env (AD BasicAD Rev) e2 e1
optPrimFun env P_lmApplyT  (Tuple [e1,e2])        = optLMApply env (AD TupleAD Fwd) e1 e2
optPrimFun env P_lmApplyTR (Tuple [e1,e2])        = optLMApply env (AD TupleAD Rev) e2 e1
optPrimFun _ P_lmCompose   (Tuple [f,g])  = optLMCompose f g

optPrimFun _ P_lmVCat (Tuple es)
  | Just prs <- mapM isLMZero_maybe es
  , (s:_, ts) <- unzip prs
  = Just $ lmZero s (Tuple ts)

-- Add(0, x) = x = Add(x, 0)
optPrimFun _ P_lmAdd (Tuple [p,q])
  | isLMZero p = Just q
  | isLMZero q = Just p

-- Add(Scale(x), Scale(y)) = Scale(Add(x,y))
  | Call scale1 (Tuple [t1, x]) <- p
  , Call scale2 (Tuple [t2, y]) <- q
  , scale1 `isThePrimFun` P_lmScale
  , scale2 `isThePrimFun` P_lmScale
  , typeof t1 == typeof t2
  = Just $ lmScale (typeof t1) (pAdd x y)

-- Add(HCat(p1, p2, ...), HCat(q1, q2, ...)) = Hcat(Add(p1, q1), Add(p2, q2), ...)
optPrimFun _ P_lmAdd (Tuple [ Call hcat1 (Tuple ps)
                            , Call hcat2 (Tuple qs) ] )
  | hcat1 `isThePrimFun` P_lmHCat
  , hcat2 `isThePrimFun` P_lmHCat
  = Just (lmHCat (zipWith (\ pi qi -> lmAdds [pi, qi]) ps qs))

optPrimFun _ _ _ = Nothing

----------------------
optLMCompose :: TExpr -> TExpr -> Maybe TExpr
optLMCompose f g
  | isLMOne f  = Just g

  -- 1 o g = g
  | isLMOne g  = Just f

  -- f o 0 = 0
-- Can't do this without vector sizes :-(
-- optLMCompose f g
--  | isLMZero f = Just (lmZero s t)
--  | isLMZero g = Just (lmZero s t)

optLMCompose f g
  -- Scale(T, x) . Scale(T, y) = Scale(T, xy )
  | Call scale1 (Tuple [t1, x]) <- f
  , Call scale2 (Tuple [t2, y]) <- g
  , scale1 `isThePrimFun` P_lmScale
  , scale2 `isThePrimFun` P_lmScale
  , typeof t1 == typeof t2
  = Just $ lmScale (typeof t1) (pMulff x y)

  -- (f . g) . h   =>   f . (g . h)
  | Call lmcomp (Tuple [p1,p2]) <- f
  , lmcomp `isThePrimFun` P_lmCompose
  = optLMCompose p1 (lmCompose p2 g)

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call hcat (Tuple qs) <- g
  , hcat `isThePrimFun` P_lmHCat
  = Just (lmHCat (map (lmCompose f) qs))

  -- (m1 `hcat` m2) . (m3 `vcat` m4)  =>  (m1 . m3) + (m2 . m4)
  | Call hcat (Tuple ps) <- f
  , Call vcat (Tuple qs) <- g
  , hcat `isThePrimFun` P_lmHCat
  , vcat `isThePrimFun` P_lmVCat
  = traceWhenUnequal "H o V" (length ps) (length qs) $
    Just (lmAdds (zipWith lmCompose ps qs))

  | otherwise
  = Nothing

-----------------------
inlineCall :: OptEnv
           -> TDef         -- Function definition
           -> [Type]       -- Type arguments
           -> TExpr        -- Value argument
           -> Maybe TExpr
inlineCall env def targs val_arg
  | Just (pat, body) <- instantiateDef def targs
  = Just $ case pat of
      VarPat tv  -> mkLet tv val_arg body
      TupPat tvs -> mkLets (fresh_tvs `zip` args) $
                    -- See Note [Avoid name clashes in inlineCall]
                    mkLets [ (tv, Var fresh_tv)
                           | (tv,fresh_tv) <- tvs `zip` fresh_tvs
                           , tv /= fresh_tv ]
                    body
                 where
                   args = splitTuple val_arg (length tvs)
                   (_, fresh_tvs) = notInScopeTVs (optEnvInScope env) tvs
  | otherwise
  = Nothing

instantiateDef :: TDef -> [Type] -> Maybe (Pat, TExpr)
instantiateDef (Def { def_qvars = qvars, def_pat = pat, def_rhs = rhs }) targs
  | UserRhs body <- rhs
  , length qvars == length targs
  , let ty_subst = mkTySubst (qvars `zip` targs)
  = Just (if null qvars
          then (pat, body)
          else (tySubstPat ty_subst pat, tySubstExpr ty_subst body))
  | otherwise
  = Nothing

{- Note [Avoid name clashes in inlineCall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have the call (f e1 e2),
  where e1 and e2 mention variables p, q
  f is defined by  def f (p,q) = rhs
Then we want to generate
  let p' = e1
      q' = e2
  in let
      p = p'
      q = q'
  in rhs
to avoid accidental capture of p,q.
-}

-----------------------
optSum :: TExpr -> Maybe TExpr

-- RULE: sum (build n (\i. (e1,e2,...)))
--       = (sum (build n (\i.e1)), sum (build n (\i.e2)), ...)
optSum e
  | Just (n, i, body) <- isBuild_maybe e
  = Just $ pSumBuild n (Lam i body)

optSum e
  | Just (n, v) <- isConstVec_maybe e
  , TypeInteger <- typeof n   -- TODO: multidimensional version
  = Just $ sumOfConstVec n v

-- RULE: sum (build n (\i. e)) = (sumbuild n (\i. e))

-- RULE: sum (diag sz f)  =  build sz f
optSum (Call diag (Tuple [sz, f]))
  | diag `isThePrimFun` P_diag
  = Just $ pBuild sz f

-- RULE: sum (deltaVec sz i e) = e
optSum (Call deltaVec (Tuple [_, _, e]))
  | deltaVec `isThePrimFun` P_deltaVec
  = Just e

optSum _ = Nothing

-----------------------
sumOfConstVec :: TExpr -> TExpr -> TExpr
sumOfConstVec (Tuple _) _ = error "sumOfConstVec (Tuple) unimplemented"
sumOfConstVec n v = case typeof v of
  TypeInteger -> pMulii n v
  _ -> pScale (pToFloat n) v

-----------------------
optBuild :: TExpr -> TVar -> TExpr -> Maybe TExpr

-- RULE: build sz (\i. e) = constVec sz e
--       (if i is not free in e)
optBuild sz i e
  | i `notFreeIn` e
  = Just $ pConstVec sz e

-- RULE: build sz (\i. delta i ex eb)  =  let i = ex in
--                                        deltaVec sz i eb
--       (if i is not free in ex)
-- NB: however, i might be free in eb
optBuild sz i e
  | Call delta (Tuple [e1,e2,eb]) <- e
  , delta `isThePrimFun` P_delta
  , Just ex <- ok_eq e1 e2
  , i `notFreeIn` ex
  = Just $ mkLet i ex $ pDeltaVec sz (Var i) eb
  where
    -- We want this to work for both (\i. delta i j e)
    --                           and (\j. delta i j e)
    ok_eq (Var v) e2 | v == i = Just e2
    ok_eq e1 (Var v) | v == i = Just e1
    ok_eq _ _ = Nothing

-- RULE: build sz (\i. deltaVec sz i e)   = diag sz (\i. e)
optBuild sz i build_e
  | Call deltaVec (Tuple [sz2, Var i2, e]) <- build_e
  , deltaVec `isThePrimFun` P_deltaVec
  , i  == i2
  = Just $ pDiag sz sz2 (Lam i e)

{-
-- RULE: build sz (\i. f e1 ... eN ...)  =
--         let tN = eN in
--         build sz (\i. f e1 .. tN ... )
-- { if i is not free in eN }
optBuild sz i e
  | Call tf@(TFun _ (Fun (UserFun _))) (Tuple [e1,e2]) <- e
  , is_expensive e2 -- try to apply only to "expensive" operations
  , i `notFreeIn` e2
  = Just $ Let tmp e2 $ pBuild sz (Lam i (Call tf (Tuple [e1, Var tmp])))
  where
    tmp = newVarNotInT (typeof e2)
                       (pBuild sz (Lam i e))
          -- Slightly inefficient to reassemble outer expr here
    is_expensive (Var _) = False
    is_expensive (Konst _) = False
    is_expensive _ = False
-}

-- build sz (\i. e1 * e2)  = (build sz (\i.e1)) * e2
-- { if i is not free in e2 }
{-  TODO: once decided on amount of polymorphism in *
optBuild sz i e
  | Call f (Tuple [e1,e2]) <- e
  , f `isThePrimFun` "mul"
  , i `notFreeIn` e2
  , is_expensive e2
  = Just $ pMul (pBuild sz (Lam i e1)) e2
  where
      is_expensive (Call _ _) = True
      is_expensive _ = False
--}

optBuild _ _ _ = Nothing

-----------------------
optSumBuild :: TExpr -> TVar -> TExpr -> Maybe TExpr

-- RULE: sumbuild n (\i. (e1,e2))  =  (sumbuild n (\i.e1), sumbuild n (\i.e2))
optSumBuild n i (Tuple es)
  = Just $ Tuple (map (pSumBuild n . Lam i) es)

-- RULE: sumbuild n (\i. e)  =  n * e, when i not free in e
optSumBuild sz i e
  | TVar TypeInteger _ <- i
  , i `notFreeIn` e
  = Just $ sumOfConstVec sz e

-- RULE: sumbuild n (\i. delta i ej e)    where i is not free in ej
--       = let i = ej in e
optSumBuild _ i (Call delta (Tuple [Var i1, ej, e]))
  | delta `isThePrimFun` P_delta
  , i == i1
  , i `notFreeIn` ej
  = Just (mkLet i ej e)

-- RULE: sumbuild n (\i. deltaVec n i e)
--       = build n (\i. e)
--
-- We prove this rule by showing that LHS = RHS at each index.
--
--   index j (sumbuild n (\i. deltaVec n i e))
-- = sumbuild n (\i. index j (deltaVec n i e))
-- = sumbuild n (\i. if j == i then e else 0)
-- = e[i->j]
-- = index j (build n (\i. e))
optSumBuild n i (Call deltaVec (Tuple [_n1, Var i1, e]))
  | deltaVec `isThePrimFun` P_deltaVec
  , i == i1
  -- TODO n == sz
  = Just $ pBuild n (Lam i e)

optSumBuild _ _ _ = Nothing


-----------------------
-- See Note [Automatic differentiation documentation]
optGradFun :: HasCallStack => InScopeSet -> ADPlan
                           -> Type -> BaseFun Typed -> TExpr -> Maybe TExpr
-- Inline the definitions for grad(+), grad(*) etc
optGradFun _ _ _ (BaseUserFun {}) _
  = Nothing

-- From here on we have primitives or selection

optGradFun env TupleAD ty f args
  | TypeTuple [res_ty, lm_ty] <- ty
  , Just opt_grad <- optGradFun env BasicAD lm_ty f new_args
  , let tfun = TFun { tf_ret = res_ty, tf_fun = Fun JustFun f
                    , tf_targs = [] }  -- SLPJ: fix me
  = Just $
    mkLets binds $
    Tuple [ Call tfun new_args, opt_grad ]
  | otherwise
  = Nothing
  where
    (binds, [new_args]) = makeAtomic False env [args]

optGradFun _ BasicAD ty (PrimFun f)  args = optGradPrim ty f args

type TBinds = [(TVar, TExpr)]

makeAtomic :: Bool           -- True => add a fresh binding regardless
           -> InScopeSet     -- Do not chose these as binders
           -> [TExpr]        -- Arguments
           -> (TBinds, [TExpr])
makeAtomic always_bind in_scope args
  = (binds, new_args)
  where
    ((_,binds), new_args) = mapAccumR do_arg (in_scope, []) args
    do_arg :: (InScopeSet, TBinds) -> TExpr -> ((InScopeSet, TBinds), TExpr)
    do_arg (in_scope, binds) arg
       | not always_bind
       , isTrivial arg = ((in_scope, binds), arg)
       | otherwise     = ((in_scope', bind:binds), Var tv)
       where
         tv = TVar (typeof arg) (notInScope argVar in_scope)
         bind = (tv, arg)
         in_scope' = extendInScopeSet tv in_scope

-- See Note [Automatic differentiation documentation]
optGradPrim :: HasCallStack => Type -> PrimFun -> TExpr -> Maybe TExpr
--   sel 2 3 :: (a,b,c) -> b
-- D$sel 2 3 :: (a,b,c) -> (a,b,c) -o b
--
-- D$sel 2 3 (a,b,c)  --->   lmHCat [ 0 :: S1 -o S2
--                                  , 1 :: S2 -o S2
--                                  , 0 :: S3 -o S2 ]
-- NB: this works regardless of the value argument;
--     the gradient is the same everywhere
optGradPrim _ (P_SelFun i n) arg
  | TypeTuple tys <- typeof arg
  , length tys == n
  , let tyi = tys !! (i-1)
        ti  = pSel i n arg
  = Just $
    lmHCat [ if i == j then lmOne tyi
                       else lmZero (pSel j n arg) ti
           | j <- [1..n] ]

-- (+) :: (F,F) -> f
-- (D+) :: (F,F) -> ((dF,dF) -o dF)
-- (D+)(x,y) :: (dF,dF) -o dF
optGradPrim _ P_ts_add arg
  | Tuple arg' <- arg
  , [t1, t2] <- map typeof arg'
  = Just (lmHCat [lmOne t1, lmOne t2])

-- ts_scale :: (Float, T) -> T
optGradPrim (TypeLM _ _) P_ts_scale (Tuple [scalar, v])
  | TypeFloat <- typeof scalar
  = Just (lmHCat [lmScaleR v, lmScale (typeof v) scalar])

-- ts_dot :: (T,T) -> Float
-- D$ts_dot :: (T,T) -> (T,T) -o Float
optGradPrim (TypeLM _ _) P_ts_dot (Tuple [v1, v2])
  | typeof v1 == typeof v2
  = Just (lmHCat [lmDot v2, lmDot v1])

optGradPrim (TypeLM a _) P_ts_neg _
  = Just (lmScale a (kFloat $ -1.0))

optGradPrim _ P_sum e
  | TypeTensor d t <- typeof e
  = Just (lmBuildT (pSize e) (Lam (TVar (tensorIndexType d) $ Simple "sum$ii")
                             (lmOne t)))

optGradPrim _ P_size e = Just $ lmZero e (mkZero (pSize e))

optGradPrim _ P_index (Tuple [i,v])
  = Just (lmHCat [ lmZero i vi
                 , lmBuildT (pSize v) (Lam ii (lmDelta vi (Var ii) i)) ])
  where
    ii = TVar (typeof i) $ Simple "primDindex$ii"
    vi = pIndex i v

-- eq :: (T,T) -> Bool   (or, more generally, any f :: X -> Bool)
-- D$eq :: (T,T) -> (T,T) -o ()
optGradPrim (TypeLM _ TypeBool) _ e
  = Just (lmZero e (Konst (KBool True)))

optGradPrim (TypeLM a _) P_trace _ = Just (lmOne a)

optGradPrim (TypeLM a _) P_copydown _ = Just (lmOne a)

optGradPrim _ f     a = optTrace("No opt for grad of prim " ++ render (ppr f) ++ " at " ++ show (typeof a)) Nothing


-----------------------
-- See Note [Automatic differentiation documentation]
optDrvFun :: HasCallStack => ADMode -> BaseFun p -> TExpr -> Maybe TExpr
optDrvFun (AD BasicAD dir) (PrimFun f) args = optDrvPrim dir f args
optDrvFun _ _ _ = Nothing

-- See Note [Automatic differentiation documentation]
optDrvPrim :: HasCallStack => ADDir -> PrimFun -> TExpr -> Maybe TExpr

optDrvPrim Fwd P_constVec (Tuple [n_v, dn_dv])
  = Just $ pConstVec (pSel 1 2 n_v) (pSel 2 2 dn_dv)
optDrvPrim Rev P_constVec (Tuple [n_v, ddr])
  = Just $ Tuple [ mkTangentZero (pSel 1 2 n_v), pSum ddr ]

optDrvPrim Fwd P_deltaVec (Tuple [n_i_v, dn_di_dv])
  = Just $ pDeltaVec (pSel 1 3 n_i_v) (pSel 2 3 n_i_v) (pSel 3 3 dn_di_dv)
optDrvPrim Rev P_deltaVec (Tuple [n_i_v, ddr])
  = Just $ Tuple [ mkTangentZero (pSel 1 3 n_i_v), mkTangentZero (pSel 2 3 n_i_v), pIndex (pSel 2 3 n_i_v) ddr ]

optDrvPrim Fwd P_Vec_init (Tuple [_vs, dvs])
  = Just $ mkPrimCall P_Vec_init dvs
optDrvPrim Rev P_Vec_init (Tuple [Tuple vs, ddr])
  = Just $ Tuple (toList $ mapWithIndex (\i _ -> pIndex (kInt $ toInteger i) ddr) $ fromList vs)
optDrvPrim Rev P_Vec_init (Tuple [_v, ddr])
  = Just $ pIndex (kInt 0) ddr

optDrvPrim _ _ _ = Nothing

---------------
-- See Note [Automatic differentiation documentation]
--
-- Called for (lmApply lm dx)
optLMApply :: InScopeSet -> ADMode -> TExpr -> TExpr -> Maybe TExpr

optLMApply _ adm (Assert e1 e2) dx
  = Just (Assert e1 (lmApply_AD adm e2 dx))

optLMApply _ adm (Let v rhs body) dx
  = Just $ Let v rhs $ lmApply_AD adm body dx

optLMApply _ adm (If b et ef) dx
  = Just $ If b (lmApply_AD adm et dx) (lmApply_AD adm ef dx)

-- lmApplyT  (r, lm) dx ===> lmApply  lm dx
-- lmApplyTR dr (r, lm) ===> lmApplyR dr lm
optLMApply _ (AD TupleAD dir) (Tuple [_, lm]) dx
  = Just (lmApply_Dir dir lm dx)

-- Called for (lmApply (lm* es) dx)
-- In BasicAD only
optLMApply env (AD BasicAD dir) (Call (TFun { tf_fun = Fun JustFun (PrimFun f) }) es) dx
  = optLMApplyCall env dir f es dx

-- Looking at:   D$f(e1, e2) `lmApply` dx
--   f :: S1 S2 -> T
--   D$f :: S1 S2 -> ((S1,S2) -o T)
--   fwd$f :: S1 S2 S1_t S2_t -> T_t
optLMApply _ (AD adp1 Fwd) (Call tfun es) dx
  | TFun { tf_ret = TypeLM _ t, tf_fun = Fun (GradFun adp2) f } <- tfun
  , adp1 == adp2
  , let grad_fun = TFun { tf_ret = tangentType t
                        , tf_fun = Fun (DrvFun (AD adp1 Fwd)) f
                        , tf_targs = [] } -- SLPJ: fix me
  = Just (Call grad_fun es_dx)
  where
    es_dx = Tuple [es, dx]

-- Looking at:   dr `lmApplyR` D$f(e1, e2)
--   f :: S1 S2 -> T
--   D$f :: S1 S2 -> ((S1,S2) -o T)
--   rev$f :: S1 S2 T_ -> (S1_t,S2_t)
optLMApply _ (AD adp1 Rev) (Call tfun es) dx
  | TFun { tf_ret = TypeLM s _, tf_fun = Fun (GradFun adp2) f } <- tfun
  , adp1 == adp2
  , let grad_fun = TFun { tf_ret = tangentType s
                        , tf_fun = Fun (DrvFun (AD adp1 Rev)) f
                        , tf_targs = [] }  -- SLPJ: fix me
  = Just (Call grad_fun es_dx)
  where
    es_dx = Tuple [es, dx]

{-
optLMApply (Call (TFun (TypeLM _ t) (GradFun (PrimFun f) mode)) e) dx
  = -- trace ("Prim Grad->Der [" ++ f ++ "]")
    Just $ Call (TFun (tangentType t) (DrvFun (PrimFun f) mode)) (Tuple [e, dx])
-}

optLMApply _ _ _e _
  = --pprTrace "Apply not optimized:" (ppr e)
    Nothing

------------------
-- See Note [Automatic differentiation documentation]
--
-- Optimise (lmApply (fun arg) dx)
-- Only for the BasicAD form
optLMApplyCall :: HasCallStack
               => InScopeSet -> ADDir
               -> PrimFun -> TExpr  -- f args :: s -o t
               -> TExpr             -- :: T(s)
               -> Maybe TExpr       -- :: T(t)

-- (lmZero :: s -o t) `apply` (x :: T(s))  = 0 :: T(t)
optLMApplyCall _ dir P_lmZero (Tuple [s, t]) dx
  = traceWhenTypesUnequal "Apply lmZero" in_ty (typeof dx) $
    Just (case dir of
            Fwd -> mkTangentZero t
            Rev -> mkTangentZero s)
  where
    tangent_s = tangentType (typeof s)
    tangent_t = tangentType (typeof t)
    in_ty = case dir of
               Fwd -> tangent_s
               Rev -> tangent_t

optLMApplyCall _ _ P_lmOne t dx
  = traceWhenTypesUnequal "Apply lmOne"
             (tangentType (typeof t)) (typeof dx) $
    Just dx

optLMApplyCall _ dir P_lmAdd (Tuple [f,g]) dx
  = Just (pAdd (lmApply_Dir dir f dx) (lmApply_Dir dir g dx))

optLMApplyCall _ Fwd P_lmCompose (Tuple [f,g]) dx = Just (lmApply f (lmApply g dx))
optLMApplyCall _ Rev P_lmCompose (Tuple [f,g]) dx = Just (lmApplyR (lmApplyR dx f) g)

optLMApplyCall _ _ P_lmScale (Tuple [_ty, x]) dx
  | TypeFloat == typeof x
  = Just (pScale x dx)

optLMApplyCall _ Fwd P_lmScaleR v dx
  | TypeFloat == typeof dx
  = Just (pScale dx v)

optLMApplyCall _ Rev P_lmScaleR dr v
  | typeof dr == typeof v
  = Just (pDot dr v)

optLMApplyCall _ Fwd P_lmDot v1 v2
  | typeof v1 == typeof v2
  = Just (pDot v1 v2)

optLMApplyCall _ Rev P_lmDot v r
  | typeof r == TypeFloat
  = Just (pScale r v)

optLMApplyCall _ Fwd P_lmVCat (Tuple es) dx = do_prod Fwd es dx
optLMApplyCall _ Rev P_lmVCat (Tuple es) dx = do_sum  Rev es dx
optLMApplyCall _ Fwd P_lmHCat (Tuple es) dx = do_sum  Fwd es dx
optLMApplyCall _ Rev P_lmHCat (Tuple es) dx = do_prod Rev es dx

optLMApplyCall env Fwd P_lmVCatV e dx = do_prod_v env Fwd e dx
optLMApplyCall env Rev P_lmVCatV e dx = do_sum_v  env Rev e dx
optLMApplyCall env Fwd P_lmHCatV e dx = do_sum_v  env Fwd e dx
optLMApplyCall env Rev P_lmHCatV e dx = do_prod_v env Rev e dx

optLMApplyCall _ dir P_lmFold (Tuple [sZero, Lam i m, Lam i' m', acc, v]) dx =
  do_fold dir sZero i m i' m' acc v dx

optLMApplyCall _ _ fun e arg
  = optTrace ("No opt for LM apply of " ++
              render (parens (ppr fun 
                      <+> parens (ppr e <+> text ":" <+> ppr (typeof e))
                      <+> text "to" 
                      <+> parens (ppr arg <+> text ":" <+> ppr (typeof arg)))))
    Nothing

----------------------
do_prod :: ADDir -> [TExpr] -> TExpr -> Maybe TExpr
do_prod dir es dx = Just (Tuple [lmApply_Dir dir e dx | e <- es])

do_sum :: ADDir -> [TExpr] -> TExpr -> Maybe TExpr
do_sum dir es dx
  = Just $ foldr1 pAdd $ zipWith (lmApply_Dir dir) es dxs
  where
    n = length es

    dxs = case dx of
            Tuple dxs -> dxs
            _ -> [ pSel i n dx | i <- [1..n] ]

do_prod_v :: InScopeSet -> ADDir -> TExpr -> TExpr -> Maybe TExpr

-- (V( build n (\i.m) ) `lmApply` dx) = build n (\i. m `lmApply` dx)
--   This special case to avoids the hard-to-optimise
--       let m = build (\j. blah)
--       in ...(build n (\i. ...m[i]...))...
do_prod_v env dir e dx
  | Just (n, i, body) <- isBuild_maybe e
  , let (binds, [vdx]) = makeAtomic True (extendInScopeSet i env) [dx]
  = Just $ mkLets binds $
    pBuild n $ Lam i $
    lmApply_Dir dir body vdx
  
  | Just (n, v) <- isConstVec_maybe e
  = Just $ pConstVec n (lmApply_Dir dir v dx)

  -- (V(m) `lmApply` dx) = build n (\i. m[i] `lmApply` dx)
  | TypeTensor d _ <- typeof e
  , let i = indexTVar d
  , let (binds, [ve, vdx]) = makeAtomic True (extendInScopeSet i env) [e,dx]
  = Just $ mkLets binds $
    pBuild (pSize ve) $ Lam i $
    lmApply_Dir dir (pIndex (Var i) ve) vdx

  | otherwise = Nothing

do_sum_v :: InScopeSet -> ADDir -> TExpr -> TExpr -> Maybe TExpr
do_sum_v env dir e dx
  | Just (n, i, body) <- isBuild_maybe e
  , let (binds, [vdx]) = makeAtomic True (extendInScopeSet i env) [dx]
  = Just $ mkLets binds $
    pSumBuild n $ Lam i $
    lmApply_Dir dir body (pIndex (Var i) vdx)
  
  | Just (n, v) <- isConstVec_maybe e
  , Just d <- tensorDimensionFromIndexType_maybe (typeof n)
  , let i = indexTVar d
  , let (binds, [vdx]) = makeAtomic True (extendInScopeSet i env) [dx]
  = Just $ mkLets binds $
    pSumBuild n $ Lam i $
    lmApply_Dir dir v (pIndex (Var i) vdx)

  -- (H(m) `lmApply` dx) = sumbuild n (\i. m[i] `lmApply` dx[i])
  | TypeTensor d _ <- typeof e
  , let i = indexTVar d
  , let (binds, [vm, vdx]) = makeAtomic True (extendInScopeSet i env) [e,dx]
  = Just $
    mkLets binds $
    pSumBuild (pSize vm) $ Lam i $
    lmApply_Dir dir (pIndex (Var i) vm)
                    (pIndex (Var i) vdx)

  | otherwise = Nothing

do_fold :: ADDir
        -> TExpr
        -> TVar -> TExpr
        -> TVar -> TExpr
        -> TExpr -> TExpr -> TExpr
        -> Maybe TExpr
do_fold Rev sZero i m i' m' acc v dx = Just (pRFold (tangentType elt_ty) sZero f f' acc v dx)
  where acc_elt_ty = typeof i'
        TypeTuple [acc_ty, elt_ty] = acc_elt_ty
        dacc_ty = tangentType acc_ty
        f = Lam i m
        f' = Lam i'_dr
             $ mkLet i' (pFst (Var i'_dr))
             $ mkLet dr (pSnd (Var i'_dr))
             $ lmApplied
          where
            lmApplied = lmApply_Dir Rev m' (Var dr)
            dr        = newVarNotIn dacc_ty m'
            i'_dr     = newVarNotIn (TypeTuple [acc_elt_ty, dacc_ty]) lmApplied

do_fold Fwd _ i m i' m' acc v ds_acc_v = Just (pFFold f acc v df dacc dv)
  where f = Lam i m
        df = Lam i'_di'
             $ mkLet i' (pFst (Var i'_di'))
             $ mkLet di' (pSnd (Var i'_di'))
             $ lmApplied
          where
            lmApplied = lmApply_Dir Fwd m' (Tuple [ds, Var di'])
            di'       = newVarNotIn (tangentType (typeof i')) (Tuple [m', ds])
            i'_di'    =
              newVarNotIn (TypeTuple [ typeof i' , tangentType (typeof i')])
                          lmApplied

        ds   = pFst ds_acc_v
        dacc = pFst (pSnd ds_acc_v)
        dv   = pSnd (pSnd ds_acc_v)

--------------------------------------
hspec :: Spec
hspec = do
    describe "optLM tests" $ do
      it "lmAdd(S(x),S(y)) -> S(x+y)" $
        optPrimFun emptyInScopeSet P_lmAdd
            (Tuple [lmScale TypeFloat (kFloat 1.3), lmScale TypeFloat (kFloat 0.4)])
        `shouldBe`
        Just (lmScale TypeFloat (pAdd (kFloat 1.3) (kFloat 0.4)))

      it "lmAdd(HCat) = HCat(lmAdd) and some more simplifications" $
        let l1 = lmOne TypeFloat
            f2 = kFloat 2.0
            f4 = kFloat 4.0
            l2 = lmScale TypeFloat f2
        in
            optE emptyOptEnv
                 (lmAdd (lmHCat [l1, l2]) (lmHCat [l2, l2]))
            `shouldBe`
            lmHCat [lmAdd l1 l2, lmScale TypeFloat f4]

test_opt:: IO ()
test_opt = Test.Hspec.hspec Opt.hspec
