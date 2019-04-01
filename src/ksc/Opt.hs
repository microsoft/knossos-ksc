module Opt( optLets, optDef, optDefs, optE, Opt.hspec, simplify, test_opt ) where

import Lang
import LangUtils
import Prim
import Rules
import OptLet
import Annotate( GblSymTab, lookupGblST, extendGblST, emptyGblST )

import Debug.Trace
import Test.Hspec
import Data.List( mapAccumL )

optTrace :: msg -> a -> a
optTrace _msg t = t -- trace msg t

data OptEnv = OptEnv { optRuleBase :: RuleBase
                     , optGblST    :: GblSymTab }

emptyOptEnv :: OptEnv
emptyOptEnv = OptEnv { optRuleBase = mkRuleBase []
                     , optGblST    = emptyGblST }

---------------
optDefs :: HasCallStack => RuleBase -> GblSymTab -> [TDef]
                        -> (GblSymTab, [TDef])
-- Returned GblSymTab contains the optimised definitions
optDefs = mapAccumL . optDef

optDef :: HasCallStack => RuleBase -> GblSymTab -> TDef
                       -> (GblSymTab, TDef)
optDef rb gst (DefX (TFun ty f) args rhs)
  = (extendGblST gst [def'], def')
  where
    def' = DefX (TFun ty f) args (simplify env args rhs)
    env = OptEnv { optRuleBase = rb, optGblST = gst }

simplify :: OptEnv -> [TVar] -> TExpr -> TExpr
simplify env args rhs
  =  -- We run optLets again at the end of the simplify pass to ensure
     -- that variables have unique names.  See
     --
     --     https://github.com/awf/knossos/pull/386#issuecomment-476586918
     optLets args (optE env (optLets args rhs))

---------------
optE :: HasCallStack => OptEnv -> TExpr -> TExpr
optE env
  = go
  where
    go :: HasCallStack => TExpr -> TExpr
    go e | Just e' <- tryRules (optRuleBase env) e = go e'

    go (Tuple es)         = Tuple (map go es)
    go (Var v)            = Var v
    go (Konst k)          = Konst k
    go (Lam v e)          = Lam v (go e)
    go (App e1 e2)        = optApp env (go e1) (go e2)
    go (Assert e1 e2)     = Assert (go e1) (go e2)
    go (Let var rhs body) = mkLet var (go rhs) (go body)
    go (If b t e)         = optIf (go b) (go t) (go e)
    go (Call f arg)       = optCall env f (map go arg)

--------------
optCall :: OptEnv -> TFun -> [TExpr] -> TExpr
optCall env fun opt_arg
  | Just new_e <- rewriteCall env fun opt_arg
  = optE env new_e
  | otherwise
  = Call fun opt_arg

--------------
optApp :: OptEnv -> TExpr -> TExpr -> TExpr
optApp env (Lam v e) a = Let v (optE env a) (optE env e)
optApp _ f a         = App f a

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

rewriteCall :: HasCallStack => OptEnv -> TFun -> [TExpr] -> Maybe TExpr

-- RULE: f( let x = e in b )  =  let x = e in f(b)
-- But be careful with lmTranspose because it needs to transform the
-- RHS of the let (sigh; this seems terribly ad-hoc)
rewriteCall _ fun [Let v r arg]
  | not (fun `isThePrimFun` "lmTranspose")
  = Just (Let v r (Call fun [arg]))

rewriteCall env (TFun _ (Fun fun)) arg
  = optFun env fun arg

rewriteCall _ (TFun ty (GradFun f _)) arg
  = optGradFun ty f arg

rewriteCall _ f@(TFun (TypeLM _ _) _) _
  = trace ("NOTE: Unmatched LM call {" ++ show f ++ "}") Nothing

rewriteCall _ _ _
  = Nothing

-----------------------
optFun :: OptEnv -> FunId -> [TExpr] -> Maybe TExpr

-- RULE:  sel_i_n (..., ei, ...)  ==>  ei
optFun _ (SelFun i _) arg
  | [Tuple es] <- arg
  , i <= length es
  = Just (es !! (i-1))

  | otherwise
  = Nothing

optFun env (PrimFun "$inline") arg
  | [Call (TFun _ fun) inner_arg] <- arg
  , Just fun_def <- lookupGblST fun (optGblST env)
  = Just (inlineCall fun_def inner_arg)

optFun _ (PrimFun f) e
  = optPrimFun f e

optFun _ (UserFun {}) _
  = Nothing

-----------------------
optPrimFun :: PrimFun -> [TExpr] -> Maybe TExpr


-- RULE: (a1,a2) + (b1,b2) = (a1+a2, b1+b2)
optPrimFun "+" [Tuple es1, Tuple es2]
  | length es1 == length es2 = Just (Tuple (zipWith pAdd es1 es2))

-- RULE: x+0 = 0+x = x
optPrimFun "+" [x, y] = 
    if isKZero y then
      Just x
    else if isKZero x then
      Just y
    else
      Nothing

-- RULE: x*0 = 0*x = 0
optPrimFun "*" [x, y]
  | isKZero x || isKZero y
  = Just $ pZero y
  | otherwise
  = Nothing

-- RULE: size (build (n, _)) = n
optPrimFun "size" [Call build [n,_]]
  | build `isThePrimFun` "build"
  = Just n

-- RULE: size (x * y) = size(x)
optPrimFun "size" [Call mul [x,_]]
  | mul `isThePrimFun` "*"
  = Just (pSize x)

-- RULE: index j (build n f) = f j
optPrimFun "index" [ ei, arr ]
  | Call build [_, f] <- arr
  , build `isThePrimFun` "build"
  , Lam i e <- f
  = Just (Let i ei e)

-- RULE: sum (build n (\i. if (i==ej) then v else 0)
--  = let i = ej in v


-- RULE: zero (zero v) = zero v
optPrimFun "zero" [Call zero [e]]
  | zero `isThePrimFun` "zero"
  = Just $ pZero e

-- RULE: deltaVec n i (\ j . 0) = zero (build n (\i . 0))
optPrimFun "deltaVec" [n, _i, val]
  | isKZero val
  = Just $ pZero $ pBuild n (Lam (TVar TypeInteger Dummy) val)

-- RULE: zero (Int) = 0
optPrimFun "zero" [Konst (KInteger _)]
  = Just (Konst (KInteger 0))

-- RULE: zero (Float) = 0.0
optPrimFun "zero" [Konst (KFloat _)]
  = Just (Konst (KFloat 0))

optPrimFun "sum"         [arg]          = optSum arg
optPrimFun "build"       [sz, Lam i e2] = optBuild sz i e2
optPrimFun "sumbuild"    [sz, Lam i e2] = optSumBuild sz i e2
optPrimFun "lmBuild"     [sz, Lam i e2] = optLMBuild sz i e2
optPrimFun "lmApply"     [e1,e2]        = optApplyLM e1 e2
optPrimFun "lmTranspose" [arg]          = optLMTrans arg

optPrimFun "lmCompose" [f,g]
  | isLMOne f  = Just g

  -- 1 o g = g
  | isLMOne g  = Just f

  -- f o 0 = 0
  {-  but where to get a zero of F's output type?
      We can't just check if the types are the same, as the sizes may be different.
  | Call gz [s,t] <- g
  , gz `isThePrimFun` "lmZero"
  = Just ?
  -}

  -- Scale(T, x) . Scale(T, y) = Scale(T, xy )
  | Call scale1 [t1, x] <- f
  , Call scale2 [t2, y] <- g
  , scale1 `isThePrimFun` "lmScale"
  , scale2 `isThePrimFun` "lmScale"
  , typeof t1 == typeof t2
  = Just $ lmScale (typeof t1) (pMul x y)

  -- (f . g) . h   =>   f . (g . h)
  | Call lmcomp [p1,p2] <- f
  , lmcomp `isThePrimFun` "lmCompose"
  = optPrimFun "lmCompose" [p1, lmCompose p2 g]

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call hcat qs <- g
  , hcat `isThePrimFun` "lmHCat"
  = Just (lmHCat (map (lmCompose f) qs))

  | Call hcat ps <- f
  , Call vcat qs <- g
  , hcat `isThePrimFun` "lmHCat"
  , vcat `isThePrimFun` "lmVCat"
  = traceWhenUnequal "H o V" (length ps) (length qs) $
    Just (lmAdds (zipWith lmCompose ps qs))

optPrimFun "lmVCat" es
  | all isLMZero es
  , (s:ss) <- map fstArg es
  , assert (text "lmVCat" <+> pprList ppr es) (all (s ==) ss) True
  , ts <- map sndArg es
  = Just $ lmZero s (Tuple ts)

-- Add(0, x) = x = Add(x, 0)
optPrimFun "lmAdd" [p,q]
  | isLMZero p = Just q
  | isLMZero q = Just p

-- Add(Scale(x), Scale(y)) = Scale(Add(x,y))
  | Call scale1 [t1, x] <- p
  , Call scale2 [t2, y] <- q
  , scale1 `isThePrimFun` "lmScale"
  , scale2 `isThePrimFun` "lmScale"
  , typeof t1 == typeof t2
  = Just $ lmScale (typeof t1) (pAdd x y)

-- Add(HCat(p1, p2, ...), HCat(q1, q2, ...)) = Hcat(Add(p1, q1), Add(p2, q2), ...)
optPrimFun "lmAdd" [ Call hcat1 ps
                   , Call hcat2 qs]
  | hcat1 `isThePrimFun` "lmHCat"
  , hcat2 `isThePrimFun` "lmHCat"
  = Just (lmHCat (zipWith (\ pi qi -> lmAdds [pi, qi]) ps qs))

optPrimFun _ _ = Nothing

-----------------------
inlineCall :: TDef -> [TExpr] -> TExpr
inlineCall def@(DefX _ bndrs body) arg
  = assert (vcat [ppr def, ppr arg]) (length arg == length bndrs) $
    (mkLets (bndrs `zip` arg) body)

-----------------------
optSum :: TExpr -> Maybe TExpr

-- RULE: sum (build n (\i. (e1,e2,...)))
--       = (sum (build n (\i.e1)), sum (build n (\i.e2)), ...)
optSum (Call build [n, Lam i (Tuple es)])
  | build `isThePrimFun` "build"
  = Just $ Tuple (map (pSum . pBuild n . Lam i) es)

-- RULE: sum (build n (\i. e)) = (sumbuild n (\i. e))
optSum (Call build [n, Lam i e])
  | build `isThePrimFun` "build"
  = Just $ pSumBuild n (Lam i e)

-- RULE: sum (diag sz f)  =  build sz f
optSum (Call diag [sz, f])
  | diag `isThePrimFun` "diag"
  = Just $ pBuild sz f

-- RULE: sum (deltaVec sz i e) = e
optSum (Call deltaVec [_, _, e])
  | deltaVec `isThePrimFun` "deltaVec"
  = Just e

optSum _ = Nothing

-----------------------
optBuild :: TExpr -> TVar -> TExpr -> Maybe TExpr

-- RULE: build sz (\i. <zero>)  =  <zero>
{-
optBuild _ _ e
  | isKZero e
  = Just $ pZero (.. the build)
-}

-- RULE: build sz (\i. delta i ex eb)  =  let i = ex in
--                                        deltaVec sz i eb
--       (if i is not free in ex)
-- NB: however, i might be free in eb
optBuild sz i e
  | Call delta [e1,e2,eb] <- e
  , delta `isThePrimFun` "delta"
  , Just ex <- ok_eq e1 e2
  , i `notFreeIn` ex
  = Just $ Let i ex $ pDeltaVec sz (Var i) eb
  where
    -- We want this to work for both (\i. delta i j e)
    --                           and (\j. delta i j e)
    ok_eq (Var v) e2 | v == i = Just e2
    ok_eq e1 (Var v) | v == i = Just e1
    ok_eq _ _ = Nothing

-- RULE: build sz (\i. deltaVec sz i e)   = diag sz (\i. e)
optBuild sz i build_e
  | Call deltaVec [sz2, Var i2, e] <- build_e
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
  , f `isThePrimFun` "*"
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

-- RULE: sumbuild n (\i. e)  =  n * e, when i not free in e
optSumBuild sz i e
  | TVar TypeInteger _ <- i
  , i `notFreeIn` e
  = Just $ pMul sz e

-- RULE: sumbuild n (\i. deltaVec n i \j . e) 
--       = build n (\j . e) 
optSumBuild n i (Call deltaVec [_n1, Var i1, e])
  | deltaVec `isThePrimFun` "deltaVec"
  , i == i1
  -- TODO n == sz
  = Just $ pBuild n (Lam i e)

optSumBuild _ _ _ = Nothing

-----------------------
optLMBuild :: TExpr -> TVar -> TExpr -> Maybe TExpr

{-
-- RULE: build sz (\i. lmZero ty T)  =  lmZero ty (Vec T)
optLMBuild _ _ e
  | isLMZero e
  , TypeLM s t <- typeof e
  = Just $ lmZero s (TypeVec t)
-}

optLMBuild _ _ _ = Nothing


-----------------------
optGradFun :: HasCallStack => Type -> FunId -> [TExpr] -> Maybe TExpr
-- Inline the definitions for grad(+), grad(*) etc
optGradFun _ (UserFun {}) _   = Nothing
optGradFun ty (PrimFun f) arg = optGradPrim ty f arg
optGradFun _ (SelFun i n) arg = optGradSel i n arg

optGradSel :: Int -> Int -> [TExpr] -> Maybe TExpr
--   sel 2 3 :: (a,b,c) -> b
-- D$sel 2 3 :: (a,b,c) -> (a,b,c) -o b
--
-- D$sel 2 3 (a,b,c)  --->   lmHCat [ 0 :: S1 -o S2
--                                  , 1 :: S2 -o S2
--                                  , 0 :: S3 -o S2 ]
-- NB: this works regardless of the value argument;
--     the gradient is the same everywhere
optGradSel i n [arg]
  | TypeTuple tys <- typeof arg
  , length tys == n
  = let
      tyi = tys !! (i-1)
      ti       = pSel i n arg
    in
      Just (lmHCat [ if i == j then lmOne tyi else lmZero (pSel j n arg) ti
                   | j <- [1..n] ])

optGradSel _ _ arg = trace ("GradSel failed" ++ show arg) Nothing

optGradPrim :: HasCallStack => Type -> PrimFun -> [TExpr] -> Maybe TExpr
-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optGradPrim _ "+" arg
  | [t1, t2] <- map typeof arg
  = Just (lmHCat [lmOne t1, lmOne t2])

optGradPrim _ "-" arg
  | [t1, t2] <- map typeof arg
  = Just (lmHCat [lmOne t1, lmScale t2 $ kTFloat (-1.0)])

optGradPrim _ "*" [x,y]
  | TypeFloat <- typeof x
  , TypeFloat <- typeof y
  = Just (lmHCat [lmScale TypeFloat y, lmScale TypeFloat x])

optGradPrim _ "*" arg
  | [TypeInteger, TypeInteger] <- map typeof arg
  = Just (lmZero (Tuple [zeroInt, zeroInt]) zeroInt)

optGradPrim _ "/"  [x,y]
  | TypeFloat <- typeof x
  , TypeFloat <- typeof y
  = Just (lmHCat [ lmScale TypeFloat (pDiv (kTFloat 1.0) y)
                 , lmScale TypeFloat (pNeg (pDiv x (pMul y y)))])

optGradPrim _  "/" arg
  | [TypeInteger, TypeInteger] <- map typeof arg
  = Just $ lmZero (Tuple [zeroInt, zeroInt]) zeroInt

optGradPrim _ "sum" [e]
  | TypeVec t <- typeof e
  = Just (lmBuildT (pSize e) (Lam (TVar TypeInteger $ Simple "sum$i")
                                  (lmOne t)))

optGradPrim _ "size" [e]
  = Just $ lmZero e zeroInt

optGradPrim _ "index" [i,v]
  = Just (primDindex i v)

optGradPrim _ "$trace" [e] = Just (lmOne $ typeof e)
optGradPrim _ "$rand" _ = Just (lmZero zeroFloat zeroFloat )
optGradPrim _ "to_float" _ = Just (lmZero zeroInt zeroFloat )
optGradPrim _ "lgamma" x = Just (lmScale TypeFloat $ mkPrimCall "digamma" x)
optGradPrim _ "neg" [e] = Just (lmScale (typeof e) (kTFloat $ -1.0))
optGradPrim _ "exp" [e] = Just (lmScale TypeFloat (pExp e))
optGradPrim _ "log" [e] = Just (lmScale TypeFloat (pDiv (kTFloat 1.0) e))
optGradPrim _ f     _ = optTrace("No opt for grad of " ++ f) Nothing

---------------
-- Called for (lmApply lm dx)
optApplyLM :: TExpr -> TExpr -> Maybe TExpr
optApplyLM (Assert e1 e2) dx
  = Just (Assert e1 (lmApply e2 dx))

-- Called for (lmApply (lm* es) dx)
optApplyLM (Call (TFun _ (Fun (PrimFun f))) es) dx
  = optApplyLMCall f es dx

optApplyLM (Let v rhs body) dx
  = Just $
    Let v rhs $
    lmApply body dx

optApplyLM (If b et ef) dx
  = Just $ If b (lmApply et dx) (lmApply ef dx)

-- Looking at:   D$f(e1, e2) `lmApply` dx
--   f :: S1 S2 -> T
--   D$f :: S1 S2 -> ((S1,S2) -o T)
--   fwd$f :: S1 S2 S1_t S2_t -> T_t
optApplyLM (Call (TFun (TypeLM _ t) (GradFun f Fwd)) es) dx
  = Just (Call grad_fun (es ++ dxs))
  where
    grad_fun = TFun (tangentType t) (DrvFun f Fwd)

    -- Unpack dx to match the arity of the function
    -- Remember: fwd$f :: S1 S2 S1_t S2_t -> T_t
    dxs = case es of
            []                  -> []
            [_]                 -> [dx]
            _ | Tuple dxs <- dx -> dxs
              | otherwise       -> pprPanic "optApplyLM" (vcat [ppr es, ppr dx])

-- Looking at:   R$f(e1, e2) `lmApply` dx
--   f :: S1 S2 -> T
--   R$f :: S1 S2 -> (T -o (S1,S2))
--   rev$f :: S1 S2 T_ -> (S1_t,S2_t)
optApplyLM (Call (TFun (TypeLM _ t) (GradFun f Rev)) es) dx
  = Just (Call grad_fun (es ++ [dx]))
  where
    grad_fun = TFun (tangentType t) (DrvFun f Rev)

{-
optApplyLM (Call (TFun (TypeLM _ t) (GradFun (PrimFun f) mode)) e) dx
  = -- trace ("Prim Grad->Der [" ++ f ++ "]")
    Just $ Call (TFun (tangentType t) (DrvFun (PrimFun f) mode)) (Tuple [e, dx])
-}

optApplyLM e _
  = pprTrace "Apply not optimized:" (ppr e)
    Nothing

------------------
-- Optimise (lmApply (fun arg) dx)
optApplyLMCall :: HasCallStack
               => String -> [TExpr] -- f args :: s -o t
               -> TExpr             -- :: S
               -> Maybe TExpr     -- :: T

-- (lmZero :: s -o t) `apply` (x :: T(s))  = 0 :: T(t)
optApplyLMCall "lmZero" [s, t] dx
  = traceWhenTypesUnequal "Apply lmZero" (tangentType (typeof s)) (typeof dx) $
    Just (pTangentZero t)

optApplyLMCall "lmOne" t dx
  = traceWhenTypesUnequal "Apply lmOne" (tangentType (typeofArgs t)) (typeof dx) $
    Just dx

optApplyLMCall "lmAdd"  [f,g] dx
  = Just (pAdd (lmApply f dx) (lmApply g dx))

optApplyLMCall "lmCompose" [f,g] dx
  = Just (lmApply f (lmApply g dx))

optApplyLMCall "lmVCat" es dx
  = Just (Tuple [lmApply e dx | e <- es])

optApplyLMCall "lmHCat" es dx
  | (e1:_) <- es
  , TypeLM _ _ <- typeof e1
  = Just $ foldr1 pAdd $ zipWith apply es [1..]
  where
    n = length es

    apply :: TExpr -> Int -> TExpr
    apply e i = lmApply e (pSel i n dx)

optApplyLMCall "lmScale" [_ty, x] dx
  = Just (pMul x dx)

optApplyLMCall "lmBuild" [n, Lam i m] dx
  = Just (pBuild n (Lam i (lmApply m dx)))

optApplyLMCall "lmBuildT" [n, Lam i m] dx
  = Just (pSum (pBuild n (Lam i (lmApply m (pIndex (Var i) dx)))))

optApplyLMCall _ _ _
  = -- pprTrace ("No opt for LM apply of " ++ show fun)
    --         (ppr arg)
    Nothing


----------------------
optLMTrans :: TExpr -> Maybe TExpr
-- Transpose an expression
optLMTrans (Var (TVar (TypeLM s t) (Grad n d)))
   = Just (Var (TVar (TypeLM t s) (Grad n (flipMode d))))

optLMTrans (Call (TFun (TypeLM s t) (GradFun f mode)) arg)
   = Just (Call (TFun (TypeLM t s) (GradFun f (flipMode mode))) arg)

optLMTrans (Call (TFun _ (Fun (PrimFun fun))) arg)
  = optTransPrim fun arg

optLMTrans (Assert e1 e2)
  = fmap (Assert e1) (optLMTrans e2)

optLMTrans (Let (TVar (TypeLM s t) (Grad n d)) rhs body)
  = Just $ Let (TVar (TypeLM t s) (Grad n (flipMode d))) (lmTranspose rhs) $
    lmTranspose body

optLMTrans (Let var rhs body)
  = Just $ Let var rhs $
    lmTranspose body

optLMTrans (If b t e)
  = Just $ If b (lmTranspose t) (lmTranspose e)

optLMTrans e = error ("Missed lmTranspose " ++ show e) Nothing

optTransPrim :: String -> [TExpr] -> Maybe TExpr
optTransPrim "lmZero"      [s,t]        = Just $ lmZero t s
optTransPrim "lmOne"       [e]          = Just $ lmOne (typeof e)
optTransPrim "lmScale"     [t, e]       = Just $ lmScale (typeof t) e
optTransPrim "lmTranspose" [e]          = Just e
optTransPrim "lmCompose"   [f,g]        = Just (lmCompose (lmTranspose g) (lmTranspose f))
optTransPrim "lmAdd"       [f,g]        = Just (lmAdd (lmTranspose f) (lmTranspose g))
optTransPrim "lmVCat"      es           = Just (lmHCat (map lmTranspose es))
optTransPrim "lmHCat"      es           = Just (lmVCat (map lmTranspose es))
optTransPrim "lmBuild"     [n, Lam i b] = Just (lmBuildT n (Lam i (lmTranspose b)))
optTransPrim "lmBuildT"    [n, Lam i b] = Just (lmBuild n (Lam i (lmTranspose b)))
optTransPrim _ _ = Nothing

--------------------------------------
hspec :: Spec
hspec = do
    describe "optLM tests" $ do
      it "lmAdd(S(x),S(y)) -> S(x+y)" $
        optPrimFun "lmAdd" [lmScale TypeFloat (kTFloat 1.3), lmScale TypeFloat (kTFloat 0.4)]
        `shouldBe`
        Just (lmScale TypeFloat (mkPrimCall2 "+" (kTFloat 1.3) (kTFloat 0.4)))

      it "lmAdd(HCat) = HCat(lmAdd) and some more simplifications" $
        let l1 = lmOne TypeFloat
            f2 = kTFloat 2.0
            l2 = lmScale TypeFloat f2
        in
            optE emptyOptEnv
                 (lmAdd (lmHCat [l1, l2]) (lmHCat [l2, l2]))
            `shouldBe`
            lmHCat [lmAdd l1 l2, lmScale TypeFloat (pAdd f2 f2)]

test_opt:: IO ()
test_opt = Test.Hspec.hspec Opt.hspec
