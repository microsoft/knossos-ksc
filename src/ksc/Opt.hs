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
import Ksc.Traversal( traverseState )

import Debug.Trace
import Test.Hspec
import Data.List( mapAccumR )

optTrace :: msg -> a -> a
optTrace _msg t = t -- trace msg t

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
optCall :: OptEnv -> TFun -> TExpr -> TExpr
optCall env fun opt_args
  | Just new_e <- rewriteCall env fun opt_args
--  = pprTrace "Rule fired:" (vcat [ text "Before:" <+> ppr (Call fun opt_args)
--                                 , text "After: " <+> ppr new_e ])
    = optE env new_e
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

rewriteCall :: HasCallStack => OptEnv -> TFun -> TExpr -> Maybe TExpr

-- RULE: f( let x = e in b )  =  let x = e in f(b)
rewriteCall _ fun (Let v r arg)
  = Just (Let v r (Call fun arg))

-- RULE: f( if e1 then e2 else e3 )  =  if e1 then f(e2) else f(e3)
rewriteCall _ fun (If e1 e2 e3)
  = Just (If e1 (Call fun e2) (Call fun e3))

rewriteCall env (TFun _ (Fun fun)) arg
  = optFun env fun arg

rewriteCall env (TFun ty (GradFun f adm)) arg
  = optGradFun (optEnvInScope env) adm ty f arg

rewriteCall _ f@(TFun (TypeLM _ _) _) _
  = trace ("NOTE: Unmatched LM call {" ++ pps f ++ "}") Nothing

rewriteCall _ _ _
  = Nothing

-----------------------
optFun :: OptEnv -> FunId -> TExpr -> Maybe TExpr

-- RULE:  sel_i_n (..., ei, ...)  ==>  ei
optFun _ (SelFun i _) arg
  | Tuple es <- arg
  , i <= length es
  = Just (es !! (i-1))

  | otherwise
  = Nothing

-- $inline needs to look up the global symtab
optFun env (PrimFun "$inline") arg
  | Call (TFun _ fun) inner_arg <- arg
  , Just fun_def <- lookupGblST (fun, typeof inner_arg) (optGblST env)
  , Def { def_pat = pat, def_rhs = UserRhs body } <- fun_def
  = Just (inlineCall env pat body inner_arg)

-- Other prims are determined by their args
optFun env (PrimFun f) e
  = optPrimFun (optEnvInScope env) f e

optFun _ (UserFun {}) _
  = Nothing

-----------------------
optPrimFun :: InScopeSet -> PrimFun -> TExpr -> Maybe TExpr

-- Constant folding.
-- TODO: match precision to target machine
optPrimFun _ op (Tuple [Konst (KFloat k1), Konst (KFloat k2)])
  = Just . Konst . KFloat $
    case op of
      "ts_add" -> k1 + k2
      "ts_scale" -> k1 * k2
      s -> errorFor s
  where errorFor s = error $ unlines $
          [ "Failed constant folding [" ++ s ++ "]."
          , "This error exists to prompt you, the ksc user,"
          , "to go ahead and add a constant folding rule for"
          , "this operation to ksc.  See also"
          , ""
          , "    https://github.com/microsoft/knossos-ksc/pull/61/commits/29c2ab04568e17b953d3fe942aba5881ab15e1f8#r309892713"
          ]

-- RULE: (e1 : ()) + (e2 : ()) = ()
-- The type () contains only one value (), which is a zero of the type
-- We use () as the tangent type for non-differentiatable types
optPrimFun _ "ts_add" (Tuple [e1, e2])
  | TypeTuple [] <- typeof e1
  , TypeTuple [] <- typeof e2
  = Just (Tuple [])

-- RULE: (a1,a2) + (b1,b2) = (a1+a2, b1+b2)
optPrimFun _ "ts_add" (Tuple [Tuple es1, Tuple es2])
  | length es1 == length es2
  = Just (Tuple (zipWith pAdd es1 es2))

-- RULE: x+0 = 0+x = x
optPrimFun _ "ts_add" (Tuple [x, y]) =
    if isKZero y then
      Just x
    else if isKZero x then
      Just y
    else
      Nothing

-- RULE: scale 0 y = 0
optPrimFun _ "ts_scale" (Tuple [x, y])
  | isKZero x || isKZero y
  -- We use the type of y because the two typing rule for scale in
  -- Prim.hs is
  --
  -- scale: (Float, t) -> t
  = Just $ mkZero y
  | otherwise
  = Nothing

-- RULE: size (build (n, _)) = n
optPrimFun _ "size" (Call build (Tuple [n,_]))
  | build `isThePrimFun` "build"
  = Just n

-- RULE: size (constVec (n, _)) = n
optPrimFun _ "size" (Call constVec (Tuple [n,_]))
  | constVec `isThePrimFun` "constVec"
  = Just n

-- RULE: index j (build n f) = f j
optPrimFun _ "index" (Tuple [ ei, arr ])
  | Just (_, i, e) <- isBuild_maybe arr
  = Just (mkLet i ei e)

-- RULE: index j (constVec (n, v)) = v
optPrimFun _ "index" (Tuple [_, Call constVec (Tuple [_, v])])
  | constVec `isThePrimFun` "constVec"
  = Just v

-- RULE: sum (build n (\i. if (i==ej) then v else 0)
--  = let i = ej in v

-- RULE: deltaVec n i 0 = zero (build n (\i . 0))
optPrimFun _ "deltaVec" (Tuple [n, _i, val])
  | isKZero val
  = Just $ pConstVec n val

-- RULE: zero (Int) = 0
optPrimFun _ "zero" (Konst (KInteger _))
  = Just (Konst (KInteger 0))

-- RULE: zero (Float) = 0.0
optPrimFun _ "zero" (Konst (KFloat _))
  = Just (Konst (KFloat 0))

optPrimFun _ "sum"         arg           = optSum arg
optPrimFun _ "build"       (Tuple [sz, Lam i e2]) = optBuild sz i e2
optPrimFun _ "sumbuild"    (Tuple [sz, Lam i e2]) = optSumBuild sz i e2
optPrimFun env "lmApply"   (Tuple [e1,e2])        = optLMApply env (AD BasicAD Fwd) e1 e2
optPrimFun env "lmApplyR"  (Tuple [e1,e2])        = optLMApply env (AD BasicAD Rev) e2 e1
optPrimFun env "lmApplyT"  (Tuple [e1,e2])        = optLMApply env (AD TupleAD Fwd) e1 e2
optPrimFun env "lmApplyTR" (Tuple [e1,e2])        = optLMApply env (AD TupleAD Rev) e2 e1
optPrimFun _ "lmCompose"   (Tuple [f,g])  = optLMCompose f g

optPrimFun _ "lmVCat" (Tuple es)
  | Just prs <- mapM isLMZero_maybe es
  , (s:_, ts) <- unzip prs
  = Just $ lmZero s (mkTuple ts)

-- Add(0, x) = x = Add(x, 0)
optPrimFun _ "lmAdd" (Tuple [p,q])
  | isLMZero p = Just q
  | isLMZero q = Just p

-- Add(Scale(x), Scale(y)) = Scale(Add(x,y))
  | Call scale1 (Tuple [t1, x]) <- p
  , Call scale2 (Tuple [t2, y]) <- q
  , scale1 `isThePrimFun` "lmScale"
  , scale2 `isThePrimFun` "lmScale"
  , typeof t1 == typeof t2
  = Just $ lmScale (typeof t1) (pAdd x y)

-- Add(HCat(p1, p2, ...), HCat(q1, q2, ...)) = Hcat(Add(p1, q1), Add(p2, q2), ...)
optPrimFun _ "lmAdd" (Tuple [ Call hcat1 (Tuple ps)
                            , Call hcat2 (Tuple qs) ] )
  | hcat1 `isThePrimFun` "lmHCat"
  , hcat2 `isThePrimFun` "lmHCat"
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
  , scale1 `isThePrimFun` "lmScale"
  , scale2 `isThePrimFun` "lmScale"
  , typeof t1 == typeof t2
  = Just $ lmScale (typeof t1) (pMulff x y)

  -- (f . g) . h   =>   f . (g . h)
  | Call lmcomp (Tuple [p1,p2]) <- f
  , lmcomp `isThePrimFun` "lmCompose"
  = optLMCompose p1 (lmCompose p2 g)

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call hcat (Tuple qs) <- g
  , hcat `isThePrimFun` "lmHCat"
  = Just (lmHCat (map (lmCompose f) qs))

  -- (m1 `hcat` m2) . (m3 `vcat` m4)  =>  (m1 . m3) + (m2 . m4)
  | Call hcat (Tuple ps) <- f
  , Call vcat (Tuple qs) <- g
  , hcat `isThePrimFun` "lmHCat"
  , vcat `isThePrimFun` "lmVCat"
  = traceWhenUnequal "H o V" (length ps) (length qs) $
    Just (lmAdds (zipWith lmCompose ps qs))

  | otherwise
  = Nothing

-----------------------
inlineCall :: OptEnv
           -> Pat -> TExpr  -- Function parameters and body
           -> TExpr            -- Arguments
           -> TExpr
inlineCall _ (VarPat tv) body arg
  = mkLet tv arg body

inlineCall env (TupPat tvs) body arg
  = mkLets (fresh_tvs `zip` args) $
    -- See Note [Avoid name clashes in inlineCall]
    mkLets [ (tv, Var fresh_tv)
           | (tv,fresh_tv) <- tvs `zip` fresh_tvs
           , tv /= fresh_tv ]
    body
  where
    args = splitTuple arg (length tvs)
    (_, fresh_tvs) = notInScopeTVs (optEnvInScope env) tvs

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
  = Just $ sumOfConstVec n v

-- RULE: sum (build n (\i. e)) = (sumbuild n (\i. e))

-- RULE: sum (diag sz f)  =  build sz f
optSum (Call diag (Tuple [sz, f]))
  | diag `isThePrimFun` "diag"
  = Just $ pBuild sz f

-- RULE: sum (deltaVec sz i e) = e
optSum (Call deltaVec (Tuple [_, _, e]))
  | deltaVec `isThePrimFun` "deltaVec"
  = Just e

optSum _ = Nothing

-----------------------
sumOfConstVec :: TExpr -> TExpr -> TExpr
sumOfConstVec n v = case typeof v of
  TypeInteger -> pMulii n v
  _ -> pScale (pToFloat n) v

-----------------------
optBuild :: TExpr -> TVar -> TExpr -> Maybe TExpr

-- RULE: build sz (\i. <zero>)  =  <zero>
{-
optBuild _ _ e
  | isKZero e
  = Just $ pZero (.. the build)
-}

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
  , delta `isThePrimFun` "delta"
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
  , deltaVec `isThePrimFun` "deltaVec"
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
  | delta `isThePrimFun` "delta"
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
  | deltaVec `isThePrimFun` "deltaVec"
  , i == i1
  -- TODO n == sz
  = Just $ pBuild n (Lam i e)

optSumBuild _ _ _ = Nothing


-----------------------
optGradFun :: HasCallStack => InScopeSet -> ADPlan
                           -> Type -> FunId -> TExpr -> Maybe TExpr
-- Inline the definitions for grad(+), grad(*) etc
optGradFun _ _ _ (UserFun {}) _
  = Nothing

-- From here on we have primitives or selection

optGradFun env TupleAD ty f args
  | TypeTuple [res_ty, lm_ty] <- ty
  , Just opt_grad <- optGradFun env BasicAD lm_ty f new_args
  = Just $
    mkLets binds $
    Tuple [ Call (TFun res_ty (Fun f)) new_args, opt_grad ]
  | otherwise
  = Nothing
  where
    (binds, [new_args]) = makeAtomic False env [args]

optGradFun _ BasicAD  _ (SelFun i n) args = optGradSel  i n args
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

optGradSel :: Int -> Int -> TExpr -> Maybe TExpr
--   sel 2 3 :: (a,b,c) -> b
-- D$sel 2 3 :: (a,b,c) -> (a,b,c) -o b
--
-- D$sel 2 3 (a,b,c)  --->   lmHCat [ 0 :: S1 -o S2
--                                  , 1 :: S2 -o S2
--                                  , 0 :: S3 -o S2 ]
-- NB: this works regardless of the value argument;
--     the gradient is the same everywhere
optGradSel i n arg
  | TypeTuple tys <- typeof arg
  , length tys == n
  , let tyi = tys !! (i-1)
        ti  = pSel i n arg
  = Just $
    lmHCat [ if i == j then lmOne tyi
                       else lmZero (pSel j n arg) ti
           | j <- [1..n] ]

optGradSel _ _ arg = trace ("GradSel failed" ++ show arg) Nothing

optGradPrim :: HasCallStack => Type -> PrimFun -> TExpr -> Maybe TExpr
-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optGradPrim _ "ts_add" arg
  | Tuple arg' <- arg
  , [t1, t2] <- map typeof arg'
  = Just (lmHCat [lmOne t1, lmOne t2])

optGradPrim _ "sum" e
  | TypeVec t <- typeof e
  = Just (lmBuildT (pSize e) (Lam (TVar TypeSize $ Simple "sum$i")
                             (lmOne t)))

optGradPrim _ "size" e
  = Just $ lmZero e zeroInt

optGradPrim _ "index" (Tuple [i,v])
  = Just (lmHCat [ lmZero i vi
                 , lmBuildT (pSize v) (Lam ii (lmDelta vi (Var ii) i)) ])
  where
    ii = TVar TypeInteger $ Simple "primDindex$i"
    vi = pIndex i v


optGradPrim (TypeLM a _) "$trace" _ = Just (lmOne a)
optGradPrim (TypeLM a _) "$copydown" _ = Just (lmOne a)
optGradPrim (TypeLM a _) "ts_neg" _ = Just (lmScale a (kTFloat $ -1.0))
optGradPrim _ f     _ = optTrace("No opt for grad of " ++ f) Nothing

---------------
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
optLMApply env (AD BasicAD dir) (Call (TFun _ (Fun (PrimFun f))) es) dx
  = optLMApplyCall env dir f es dx

-- Looking at:   D$f(e1, e2) `lmApply` dx
--   f :: S1 S2 -> T
--   D$f :: S1 S2 -> ((S1,S2) -o T)
--   fwd$f :: S1 S2 S1_t S2_t -> T_t
optLMApply _ (AD adp1 Fwd) (Call (TFun (TypeLM _ t) (GradFun f adp2)) es) dx
  | adp1 == adp2
  = Just (Call grad_fun es_dx)
  where
    grad_fun = TFun (tangentType t) (DrvFun f (AD adp1 Fwd))
    es_dx = Tuple [es, dx]

-- Looking at:   dr `lmApplyR` D$f(e1, e2)
--   f :: S1 S2 -> T
--   D$f :: S1 S2 -> ((S1,S2) -o T)
--   rev$f :: S1 S2 T_ -> (S1_t,S2_t)
optLMApply _ (AD adp1 Rev) (Call (TFun (TypeLM s _) (GradFun f adp2)) es) dx
  | adp1 == adp2
  = Just (Call grad_fun es_dx)
  where
    grad_fun = TFun (tangentType s) (DrvFun f (AD adp1 Rev))
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
-- Optimise (lmApply (fun arg) dx)
-- Only for the BasicAD form
optLMApplyCall :: HasCallStack
               => InScopeSet -> ADDir
               -> String -> TExpr   -- f args :: s -o t
               -> TExpr             -- :: T(s)
               -> Maybe TExpr       -- :: T(t)

-- (lmZero :: s -o t) `apply` (x :: T(s))  = 0 :: T(t)
optLMApplyCall _ dir "lmZero" (Tuple [s, t]) dx
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

optLMApplyCall _ _ "lmOne" t dx
  = traceWhenTypesUnequal "Apply lmOne"
             (tangentType (typeof t)) (typeof dx) $
    Just dx

optLMApplyCall _ dir "lmAdd" (Tuple [f,g]) dx
  = Just (pAdd (lmApply_Dir dir f dx) (lmApply_Dir dir g dx))

optLMApplyCall _ Fwd "lmCompose" (Tuple [f,g]) dx = Just (lmApply f (lmApply g dx))
optLMApplyCall _ Rev "lmCompose" (Tuple [f,g]) dx = Just (lmApplyR (lmApplyR dx f) g)

optLMApplyCall _ _ "lmScale" (Tuple [_ty, x]) dx
  = Just (pScale x dx)

optLMApplyCall _ Fwd "lmVCat" (Tuple es) dx = do_prod Fwd es dx
optLMApplyCall _ Rev "lmVCat" (Tuple es) dx = do_sum  Rev es dx
optLMApplyCall _ Fwd "lmHCat" (Tuple es) dx = do_sum  Fwd es dx
optLMApplyCall _ Rev "lmHCat" (Tuple es) dx = do_prod Rev es dx

optLMApplyCall env Fwd "lmVCatV" e dx = do_prod_v env Fwd e dx
optLMApplyCall env Rev "lmVCatV" e dx = do_sum_v  env Rev e dx
optLMApplyCall env Fwd "lmHCatV" e dx = do_sum_v  env Fwd e dx
optLMApplyCall env Rev "lmHCatV" e dx = do_prod_v env Rev e dx

optLMApplyCall _ dir "lmFold" (Tuple [sZero, Lam i m, Lam i' m', acc, v]) dx =
  do_fold dir sZero i m i' m' acc v dx

optLMApplyCall _ _ _ _ _
  = -- pprTrace ("No opt for LM apply of " ++ show fun)
    --         (ppr arg)
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
  | TypeVec {} <- typeof e
  , let (binds, [ve, vdx]) = makeAtomic True (extendInScopeSet indexTVar env) [e,dx]
  = Just $ mkLets binds $
    pBuild (pSize ve) $ Lam indexTVar $
    lmApply_Dir dir (pIndex (Var indexTVar) ve) vdx

  | otherwise = Nothing

do_sum_v :: InScopeSet -> ADDir -> TExpr -> TExpr -> Maybe TExpr
do_sum_v env dir e dx
  | Just (n, i, body) <- isBuild_maybe e
  , let (binds, [vdx]) = makeAtomic True (extendInScopeSet i env) [dx]
  = Just $ mkLets binds $
    pSumBuild n $ Lam i $
    lmApply_Dir dir body (pIndex (Var i) vdx)
  
  | Just (n, v) <- isConstVec_maybe e
  , let (binds, [vdx]) = makeAtomic True (extendInScopeSet indexTVar env) [dx]
  = Just $ mkLets binds $
    pSumBuild n $ Lam indexTVar $
    lmApply_Dir dir v (pIndex (Var indexTVar) vdx)

  -- (H(m) `lmApply` dx) = sumbuild n (\i. m[i] `lmApply` dx[i])
  | TypeVec {} <- typeof e
  , let (binds, [vm, vdx]) = makeAtomic True (extendInScopeSet indexTVar env) [e,dx]
  = Just $
    mkLets binds $
    pSumBuild (pSize vm) $ Lam indexTVar $
    lmApply_Dir dir (pIndex (Var indexTVar) vm)
                    (pIndex (Var indexTVar) vdx)

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
        optPrimFun emptyInScopeSet "lmAdd"
            (Tuple [lmScale TypeFloat (kTFloat 1.3), lmScale TypeFloat (kTFloat 0.4)])
        `shouldBe`
        Just (lmScale TypeFloat (pAdd (kTFloat 1.3) (kTFloat 0.4)))

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
