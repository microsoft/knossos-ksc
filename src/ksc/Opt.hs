module Opt( optLets, optDef, optDefs, optE, simplify, test_opt ) where

import Lang
import LangUtils
import Prim
import Rules
import OptLet
import Annotate( GblSymTab, lookupGblST, extendGblST, emptyGblST )
import Text.PrettyPrint
import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace
import Test.Hspec
import Data.List( mapAccumL )

optTrace msg t = t -- trace msg t

data OptEnv = OptEnv { optRuleBase :: RuleBase
                     , optGblST    :: GblSymTab }

emptyOptEnv :: OptEnv
emptyOptEnv = OptEnv { optRuleBase = mkRuleBase []
                     , optGblST    = emptyGblST }

---------------
optDefs :: HasCallStack => RuleBase -> GblSymTab -> [TDef]
                        -> (GblSymTab, [TDef])
-- Returned GblSymTab contains the optimised definitions
optDefs rb gst defs = mapAccumL (optDef rb) gst defs

optDef :: HasCallStack => RuleBase -> GblSymTab -> TDef
                       -> (GblSymTab, TDef)
optDef rb gst (DefX (TFun ty f) args r)
  = (extendGblST gst [def'], def')
  where
    def' = DefX (TFun ty f) args (simplify env r)
    env = OptEnv { optRuleBase = rb, optGblST = gst }

simplify :: OptEnv -> TExpr -> TExpr
simplify env =  -- id
                -- optE env . optLets . optE env
                optE env . optLets
  -- Note the extra optLets, which gets simple things,
  -- notably lmOne, to their use sites

---------------
optE :: HasCallStack => OptEnv -> TExpr -> TExpr
optE env e
  = go e
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
    go (Call f arg)       = optCall env f (go arg)

--------------
optCall :: OptEnv -> TFun -> TExpr -> TExpr
optCall env fun opt_arg
  | Just new_e <- rewriteCall env fun opt_arg
  = optE env new_e
  | otherwise
  = Call fun opt_arg

--------------
optApp :: OptEnv -> TExpr -> TExpr -> TExpr
optApp env (Lam v e) a = Let v (optE env a) (optE env e)
optApp env f a         = App f a

--------------
optIf :: TExpr -> TExpr -> TExpr -> TExpr
optIf (Konst (KBool True))  t e = t
optIf (Konst (KBool False)) t e = e
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
-- But be careful with lmTranspose because it needs to transform the
-- RHS of the let (sigh; this seems terribly ad-hoc)
rewriteCall _ fun (Let v r arg)
  | not (fun `isThePrimFun` "lmTranspose")
  = Just (Let v r (Call fun arg))

rewriteCall env (TFun ty (Fun fun)) arg
  = optFun env fun arg

rewriteCall _ (TFun ty (GradFun f _)) arg
  = optGradFun ty f arg

rewriteCall _ f@(TFun (TypeLM _ _) _) _
  = trace ("NOTE: Unmatched LM call {" ++ show f ++ "}") $
    Nothing

rewriteCall _ f _
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

optFun env (PrimFun "inline") arg
  | Call (TFun _ fun) inner_arg <- arg
  , Just fun_def <- lookupGblST fun (optGblST env)
  = inlineCall fun_def inner_arg

optFun _ (PrimFun f) e
  = optPrimFun f e

optFun _ (UserFun {}) _
  = Nothing

-----------------------
optPrimFun :: PrimFun -> TExpr -> Maybe TExpr

-- RULE: index j Zero = 0
optPrimFun "index" (Tuple [ ei, arr ])
  | TypeVec (TypeZero t) <- typeof arr
  = Just $ mkZero t

-- RULE: concat((as...), (bs...))
optPrimFun "concat" (Tuple [Tuple es1, Tuple es2]) = Just (Tuple (es1 ++ es2))

-- RULE: (a1,a2) + (b1,b2) = (a1+a2, b1+b2)
optPrimFun "+" (Tuple [Tuple es1, Tuple es2])
  | length es1 == length es2 = Just (Tuple (zipWith pAdd es1 es2))

-- RULE: x+0 = 0+x = x
optPrimFun "+" (Tuple [x, y]) =
    if isKZero y then
      Just x
    else if isKZero x then
      Just y
    else
      Nothing

-- RULE: x*0 = 0*x = 0
optPrimFun "*" arg@(Tuple [x, y])
  | isKZero x || isKZero y
  = Just $ Konst $ KZero $ typeof y
  | otherwise
  = Nothing

-- RULE: size (build (n, _)) = n
optPrimFun "size" (Call build (Tuple [n,_]))
  | build `isThePrimFun` "build"
  = Just n

-- RULE: size (x * y) = size(x)
optPrimFun "size" (Call mul (Tuple [x,_]))
  | mul `isThePrimFun` "*"
  = Just (pSize x)

-- RULE: index j (build n f) = f j
optPrimFun "index" (Tuple [ ei, arr ])
  | Call build (Tuple [_, f]) <- arr
  , build `isThePrimFun` "build"
  , Lam i e <- f
  = Just (Let i ei e)

-- RULE: sum (build n (\i. if (i==ej) then v else 0)
--  = let i = ej in v
optPrimFun "sum"         arg                    = optSum arg
optPrimFun "build"       (Tuple [sz, Lam i e2]) = optBuild sz i e2
optPrimFun "lmBuild"     (Tuple [sz, Lam i e2]) = optLMBuild sz i e2
optPrimFun "lmApply"     (Tuple [e1,e2])        = optApplyLM e1 e2
optPrimFun "lmTranspose" arg                    = optLMTrans arg

optPrimFun "lmCompose" (Tuple [f,g])
  | isLMOne f  = Just g
  | isLMOne g  = Just f
  | isLMZero f || isLMZero g
  , TypeLM _ t <- typeof f
  , TypeLM s _ <- typeof g
  = Just $ lmZero s t

  -- Scale(x) . Scale(y) = Scale( xy )
  | Call scale1 x <- f
  , Call scale2 y <- g
  , scale1 `isThePrimFun` "lmScale"
  , scale2 `isThePrimFun` "lmScale"
  = Just $ lmScale (pMul x y)

  -- (f . g) . h   =>   f . (g . h)
  | Call lmcomp (Tuple [p1,p2]) <- f
  , lmcomp `isThePrimFun` "lmCompose"
  = optPrimFun "lmCompose" (Tuple [p1, lmCompose p2 g])

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call hcat (Tuple qs) <- g
  , hcat `isThePrimFun` "lmHCat"
  = Just (lmHCat (map (lmCompose f) qs))

  | Call hcat (Tuple ps) <- f
  , Call vcat (Tuple qs) <- g
  , hcat `isThePrimFun` "lmHCat"
  , vcat `isThePrimFun` "lmVCat"
  = assertEqualThen "H o V" (length ps) (length qs) $
    Just (lmAdds (zipWith lmCompose ps qs))

optPrimFun "lmVCat" (Tuple es)
  | all isLMZero es
  , Just (ss, ts) <- unzipLMTypes (map typeof es)
  , (s1:ss1) <- ss
  , assertBool (all (== s1) ss1)  -- Typing rule for lmVCat demsnds this
  = Just $ lmZero s1 (TypeTuple ts)

-- Add(0, x) = x = Add(x, 0)
optPrimFun "lmAdd" (Tuple [p,q])
  | isLMZero p = Just q
  | isLMZero q = Just p
-- Add(Scale(x), Scale(y)) = Scale(Add(x,y))
  | Call scale1 x <- p
  , Call scale2 y <- q
  , scale1 `isThePrimFun` "lmScale"
  , scale2 `isThePrimFun` "lmScale"
  = Just $ lmScale (pAdd x y)

-- Add(HCat(p1, p2, ...), HCat(q1, q2, ...)) = Hcat(Add(p1, q1), Add(p2, q2), ...)
optPrimFun "lmAdd" (Tuple [ Call hcat1 (Tuple ps)
                          , Call hcat2 (Tuple qs)])
  | hcat1 `isThePrimFun` "lmHCat"
  , hcat2 `isThePrimFun` "lmHCat"
  = Just (lmHCat (zipWith (\ pi qi -> lmAdds [pi, qi]) ps qs))

optPrimFun _ _ = Nothing

-----------------------
inlineCall :: TDef -> TExpr -> Maybe TExpr
inlineCall def@(DefX _ bndrs body) arg
  | [bndr] <- bndrs
  = Just $ Let bndr arg body
  | Tuple args <- arg
  = assert (ppr def $$ ppr arg) (length args == length bndrs) $
    Just (mkLets (bndrs `zip` args) body)
  | otherwise
  = Nothing

-----------------------
optSum :: TExpr -> Maybe TExpr

-- RULE: sum (build n (\i. (e1,e2,...)))
--       = (sum (build n (\i.e1)), sum (build n (\i.e2)), ...)
optSum (Call build (Tuple [n, Lam i (Tuple es)]))
  | build `isThePrimFun` "build"
  = Just $ Tuple (map (\e -> pSum (pBuild n (Lam i e))) es)

-- RULE: sum (diag sz f)  =  build sz f
optSum (Call diag (Tuple [sz, f]))
  | diag `isThePrimFun` "diag"
  = Just $ pBuild sz f

-- RULE: sum (deltaVec sz i e) = e
optSum (Call deltaVec (Tuple [_, _, e]))
  | deltaVec `isThePrimFun` "deltaVec"
  = Just e

optSum e = Nothing

-----------------------
optBuild :: TExpr -> TVar -> TExpr -> Maybe TExpr

-- RULE: build sz (\i. lmZero ty T)  =  lmZero ty (Vec T)
optBuild sz i e
  | TypeZero ty <- typeof e
  = Just $ Konst $ KZero $ TypeVec ty

-- RULE: build sz (\i. lmZero ty T)  =  lmZero ty (Vec T)
optBuild sz i e
  | isKZero e
  = Just $ Konst $ KZero $ TypeVec (typeof e)

-- RULE: build sz (\i. delta i ex eb)  =  let i = ex in
--                                        deltaVec sz i eb
--       (if i is not free in ex)
-- NB: however, i might be free in eb
optBuild sz i e
  | Call delta (Tuple [e1,e2,eb]) <- e
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
  , f `isThePrimFun` "*"
  , i `notFreeIn` e2
  , is_expensive e2
  = Just $ pMul (pBuild sz (Lam i e1)) e2
  where
      is_expensive (Call _ _) = True
      is_expensive _ = False
--}

optBuild sz i e = Nothing

-----------------------
optLMBuild :: TExpr -> TVar -> TExpr -> Maybe TExpr

-- RULE: build sz (\i. lmZero ty T)  =  lmZero ty (Vec T)
optLMBuild sz i e
  | isLMZero e
  , TypeLM s t <- typeof e
  = Just $ lmZero s (TypeVec t)

optLMBuild sz i e = Nothing


-----------------------
optGradFun :: HasCallStack => Type -> FunId -> TExpr -> Maybe TExpr
-- Inline the definitions for grad(+), grad(*) etc
optGradFun _ (UserFun {}) _   = Nothing
optGradFun ty (PrimFun f)  arg = optGradPrim ty f arg
optGradFun _ (SelFun i n) arg = optGradSel i n arg

optGradSel :: Int -> Int -> TExpr -> Maybe TExpr
-- fst :: (a,b) -> a
-- Dfst(x,y) :: (a,b) -o a
optGradSel i n arg
  | TypeTuple ts <- typeof arg
  , length ts == n
  = Just (lmHCat [ if i == j then lmOne ty else lmZero ty ty
                 | (ty, j) <- ts `zip` [1..] ])

optGradSel _ _ _ = Nothing

optGradPrim :: HasCallStack => Type -> PrimFun -> TExpr -> Maybe TExpr
-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optGradPrim _ "+" arg
  | TypeTuple [t1, t2] <- typeof arg
  = Just (lmHCat [lmOne t1, lmOne t2])

optGradPrim _ "-" arg
  | TypeTuple [t1, t2] <- typeof arg
  = Just (lmHCat [lmOne t1, lmScale $ kTFloat (-1.0)])

optGradPrim _ "*" (Tuple [x,y])
  | TypeFloat <- typeof x
  , TypeFloat <- typeof y
  = Just (lmHCat [lmScale y, lmScale x])

optGradPrim _ "*" arg
  | let arg_ty = typeof arg
  , TypeTuple [TypeInteger, TypeInteger] <- arg_ty
  = Just (lmZero arg_ty TypeInteger)

optGradPrim _ "/"  (Tuple [x,y])
  | TypeFloat <- typeof x
  , TypeFloat <- typeof y
  = Just (lmHCat [ lmScale (pDiv (kTFloat 1.0) y)
                 , lmScale (pNeg (pDiv x (pMul y y)))])

optGradPrim _  "/" arg
  | let arg_ty = typeof arg
  , TypeTuple [TypeInteger, TypeInteger] <- arg_ty
  = Just $ lmZero arg_ty TypeInteger

optGradPrim _ "sum" e
  | TypeVec t <- typeof e
  = Just (lmBuildT (pSize e) (Lam (TVar TypeInteger $ Simple "sum$i")
                                  (lmOne t)))

optGradPrim _ "size" e
  = Just $ lmZero (typeof e) TypeInteger

optGradPrim _ "index" (Tuple [i,v])
  = Just (primDindex i v)

optGradPrim _ "$trace" e = Just (lmOne $ typeof e)
optGradPrim _ "$rand" e = Just (lmZero TypeFloat TypeFloat)
optGradPrim _ "neg" e = Just (lmScale (kTFloat $ -1.0))
optGradPrim _ "exp" e = Just (lmScale (pExp e))
optGradPrim _ "log" e = Just (lmScale (pDiv (kTFloat 1.0) e))
optGradPrim _ f     _ = optTrace("No opt for grad of " ++ f) $ Nothing

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

optApplyLM (Call (TFun (TypeLM s t) (GradFun (UserFun f) mode)) e) dx
  = trace ("User Grad->Der [" ++ f ++ "]")
    Just $ Call (TFun t (DrvFun (UserFun f) mode)) (pConcat e dx)

optApplyLM (Call (TFun (TypeLM s t) (GradFun (PrimFun f) mode)) e) dx
  = trace ("Prim Grad->Der [" ++ f ++ "]")
    Just $ Call (TFun t (DrvFun (PrimFun f) mode)) (Tuple [e, dx])

optApplyLM e dx
  = pprTrace "Apply not optimized:" (ppr e)
    Nothing

------------------
-- Optimise (lmApply (fun arg) dx)
optApplyLMCall :: HasCallStack =>
                  String -> TExpr   -- f args :: s -o t
               -> TExpr             -- :: S
               -> Maybe (TExpr)     -- :: T

-- (lmZero :: s -o t) `apply` (x :: s)  = 0 :: t
optApplyLMCall "lmZero" (Tuple [s, t]) dx
  = assertTypesEqualThen "Apply lmZero" (typeof s) (typeof dx) $
    Just (Konst $ KZero $ typeof t)

optApplyLMCall "lmOne" _ dx
  = -- assertTypesEqualThen "Apply lmOne" (typeof t) (typeof dx) $
    Just dx

optApplyLMCall "lmAdd"  (Tuple [f,g]) dx
  = Just (pAdd (lmApply f dx) (lmApply g dx))

optApplyLMCall "lmCompose" (Tuple [f,g]) dx
  = Just (lmApply f (lmApply g dx))

optApplyLMCall "lmVCat" (Tuple es) dx
  = Just (Tuple [lmApply e dx | e <- es])

optApplyLMCall "lmHCat" (Tuple es) dx
  | (e1:_) <- es
  , TypeLM _ t <- typeof e1
  = Just (foldr add (Konst $ KZero $ t) (es `zip` [1..]))
  where
    n = length es

    add :: (TExpr, Int) -> TExpr -> TExpr
    add (e,i) z = pAdd (lmApply e (pSel i n dx)) z

optApplyLMCall "lmScale" x dx
  = Just (pMul x dx)

optApplyLMCall "lmBuild" (Tuple [n, Lam i m]) dx
  = Just (pBuild n (Lam i (lmApply m dx)))

optApplyLMCall "lmBuildT" (Tuple [n, Lam i m]) dx
  = Just (pSum (pBuild n (Lam i (lmApply m (pIndex (Var i) dx)))))

optApplyLMCall fun arg dx
  = pprTrace ("No opt for LM apply of " ++ show fun)
             (ppr arg)
             Nothing


----------------------
optLMTrans :: TExpr -> Maybe TExpr
-- Transpose an expression
optLMTrans (Var (TVar (TypeLM s t) (Grad n d)))
   = Just (Var (TVar (TypeLM t s) (Grad n (flipMode d))))

optLMTrans (Call (TFun (TypeLM s t) (GradFun f mode)) arg)
   = Just (Call (TFun (TypeLM t s) (GradFun f (flipMode mode))) arg)

optLMTrans (Call (TFun ty (Fun (PrimFun fun))) arg)
  = optTransPrim fun arg

optLMTrans (Assert e1 e2)
  = fmap (Assert e1) (optLMTrans e2)

optLMTrans (Let var@(TVar (TypeLM s t) (Grad n d)) rhs body)
  = Just $ Let (TVar (TypeLM t s) (Grad n (flipMode d))) (lmTranspose rhs) $
    lmTranspose body

optLMTrans (Let var rhs body)
  = Just $ Let var rhs $
    lmTranspose body

optLMTrans (If b t e)
  = Just $ If b (lmTranspose t) (lmTranspose e)

optLMTrans e = error ("Missed lmTranspose " ++ show e) $
               Nothing

optTransPrim :: String -> TExpr -> Maybe TExpr
optTransPrim "lmZero"      (Tuple [s,t])        = Just $ lmZero (typeof t) (typeof s)
optTransPrim "lmOne"       e                    = Just $ lmOne (typeof e)
optTransPrim "lmScale"     e                    = Just $ lmScale e
optTransPrim "lmTranspose" e                    = Just e
optTransPrim "lmCompose"   (Tuple [f,g])        = Just (lmCompose (lmTranspose g) (lmTranspose f))
optTransPrim "lmAdd"       (Tuple [f,g])        = Just (lmAdd (lmTranspose f) (lmTranspose g))
optTransPrim "lmVCat"      (Tuple es)           = Just (lmHCat (map lmTranspose es))
optTransPrim "lmHCat"      (Tuple es)           = Just (lmVCat (map lmTranspose es))
optTransPrim "lmBuild"     (Tuple [n, Lam i b]) = Just (lmBuildT n (Lam i (lmTranspose b)))
optTransPrim "lmBuildT"    (Tuple [n, Lam i b]) = Just (lmBuild n (Lam i (lmTranspose b)))
optTransPrim f a = Nothing

--------------------------------------
test_opt:: IO ()
test_opt =
  hspec $ do
    describe "optLM tests" $ do
      it "lmAdd(S(x),S(y)) -> S(x+y)" $
        optPrimFun "lmAdd" (Tuple [lmScale $ kTFloat 1.3, lmScale $ kTFloat 0.4])
        `shouldBe`
        Just (lmScale (mkPrimCall2 "+" (kTFloat 1.3) (kTFloat 0.4)))

      it "lmAdd(HCat) = HCat(lmAdd) and some more simplifications" $
        let l1 = lmOne TypeFloat
            f2 = kTFloat 2.0
            l2 = lmScale f2
        in
            optE emptyOptEnv
                 (lmAdd (lmHCat [l1, l2]) (lmHCat [l2, l2]))
            `shouldBe`
            lmHCat [lmAdd l1 l2, lmScale (pAdd f2 f2)]
