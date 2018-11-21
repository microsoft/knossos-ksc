module Opt( optLets, optDef, optDefs, optE, simplify, test_opt ) where

import Lang
import Prim
import Rules
import OptLet
import Text.PrettyPrint
import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace
import Test.Hspec

optTrace msg t = t -- trace msg t

---------------
optDefs :: HasCallStack => ST -> [TDef] -> [TDef]
optDefs env defs = map (optDef env) defs

optDef :: HasCallStack => ST -> TDef -> TDef
optDef env (DefX (TFun ty f) args r) =
            DefX (TFun ty f) args $
            simplify env r

simplify :: ST -> TExpr -> TExpr
simplify env =  -- optE env . optLets . optE env .
                optE env . optLets
  -- Note the extra optLets, which gets simple things,
  -- notably lmOne, to their use sites

---------------
optE :: HasCallStack => ST -> TExpr -> TExpr
optE env e
  = go e
  where
    go :: TExpr -> TExpr
    go e | Just e' <- tryRules e = go e'

    go (Tuple es)          = Tuple (map go es)
    go (Var v)             = Var v
    go (Konst k)           = Konst k
    go (Lam v ty e)        = Lam v ty (go e)
    go (App e1 e2)         = optApp env (go e1) (go e2)
    go (Assert e1 e2)      = Assert (go e1) (go e2)
    go (Let var rhs body)  = mkLet var (go rhs) (go body)
    go (If b t e)          = optIf (go b) (go t) (go e)
    go (Call (TFun ty f) arg) =
                          case optCall (TFun ty f) opt_arg of
                            Nothing -> Call (TFun ty f) opt_arg
                            Just r  -> go r
                          where
                            opt_arg = go arg

--------------
optApp :: ST -> TExpr -> TExpr -> TExpr
optApp env (Lam v ty e) a = Let v (optE env a) (optE env e)
optApp env f a            = App f a

--------------
optIf :: TExpr -> TExpr -> TExpr -> TExpr
optIf (Konst (KBool True))  t e = t
optIf (Konst (KBool False)) t e = e
optIf (Let v r b)           t e = Let v r   (optIf b t e)
optIf (Assert e1 e2)        t e = Assert e1 (optIf e2 t e)
optIf e_cond e_then e_else
  | Just (ei, ej) <- isEqualityCall e_cond
  , Konst KZero   <- e_else
  = pDelta ei ej e_then
optIf b                     t e = If b t e

--------------
-- 'optCall' performs one rewrite, returning (Just e) if
-- it succeeds in rewriting, and Nothing if not
--
-- The same goes for optFun, optGradFun, etc

optCall :: HasCallStack => TFun -> TExpr -> Maybe TExpr

-- RULE: f( let x = e in b )  =  let x = e in f(b)
optCall fun (Let v r arg) =
  Just (Let v r (Call fun arg))

optCall (TFun ty (LMFun "lmApply")) (Tuple [f,a]) =
  optApplyLM ty f a

optCall (TFun _  (LMFun "lmApply")) other_args =
  error $ "lmApply to unexpected args " ++ (show other_args)

optCall (TFun ty (Fun f)) arg =
  optFun ty f arg

optCall (TFun (TypeLM s t) (LMFun lm)) arg =
  optLM s t lm arg

optCall (TFun (TypeLM s t) (GradFun f Fwd)) arg =
  optGradFun s t f arg

optCall f _ =
  trace ("NOTE: Unmatched call {" ++ show f ++ "}") $
  Nothing

-----------------------
optFun :: Type -> FunId -> TExpr -> Maybe TExpr
-- RULE:  sel_i_n (..., ei, ...)  ==>  ei
optFun _ (SelFun i _) (Tuple es)
  | i <= length es = Just (es !! (i-1))

-- RULE: x+0 = 0+x = x
optFun _ (SFun "+") (Tuple [x, Konst KZero]) = Just x
optFun _ (SFun "+") (Tuple [Konst KZero, y]) = Just y

-- RULE: (a1,a2) + (b1,b2) = (a1+a2, b1+b2)
optFun _ (SFun "+") (Tuple [Tuple es1, Tuple es2])
  | length es1 == length es2 = Just (Tuple (zipWith pAdd es1 es2))

-- RULE: x*0 = 0*x = 0
optFun _ (SFun "*") (Tuple [x, Konst KZero]) = Just (Konst KZero)
optFun _ (SFun "*") (Tuple [Konst KZero, y]) = Just (Konst KZero)

-- RULE: size (build (n, _)) = n
optFun _ (SFun "size") (Call (TFun _ (Fun (SFun "build"))) (Tuple [n,_]))
  = Just n

-- RULE: size (x * y) = size(x)
optFun ty (SFun "size") (Call (TFun _ (Fun (SFun "*"))) (Tuple [x,_]))
  = Just (mkTCall1 ty (Fun (SFun "size")) x)

-- RULE: index j (build n f) = f j
optFun ty (SFun "index") (Tuple [ ei, arr ])
  | Call (TFun tyv (Fun (SFun "build"))) (Tuple [_, f]) <- arr
  , Lam i ty2 e <- f
  = Just (Let i ei e)

-- RULE: sum (build n (\i. if (i==ej) then v else 0)
--  = let i = ej in v
optFun ty (SFun "sum")   arg = optSum ty arg
optFun ty (SFun "build") (Tuple [sz, Lam i tyi e2]) = optBuild ty sz i tyi e2

optFun _ _ _ = Nothing

-----------------------
optSum :: Type -> TExpr -> Maybe TExpr

-- RULE: sum (build n (\i. (e1,e2,...)))
--       = (sum (build n (\i.e1)), sum (build n (\i.e2)), ...)
optSum ty (Call (TFun _ (Fun (SFun "build"))) (Tuple [n, Lam i tyi (Tuple es)]))
   = Just $ Tuple (map (\e -> pSum (pBuild n (Lam i tyi e))) es)

-- RULE: sum (diag sz f)  =  build sz f
optSum ty (Call (TFun _ (Fun (SFun "diag"))) (Tuple [sz, f]))
  = Just $ pBuild sz f

-- RULE: sum (deltaVec sz i e) = e
optSum ty (Call (TFun _ (Fun (SFun "deltaVec"))) (Tuple [_, _, e]))
  = Just e

optSum ty e = Nothing

-----------------------
optBuild :: Type -> TExpr -> TVar -> Type -> TExpr -> Maybe TExpr

-- RULE: build sz (\i. delta i ex eb)  =  let i = ex in
--                                        deltaVec sz i eb
--       (if i is not free in ex)
-- NB: however, i might be free in eb
optBuild ty sz i tyi e
  | Call (TFun _ (Fun (SFun "delta"))) (Tuple [e1,e2,eb]) <- e
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
optBuild ty sz i tyi build_e
  | Call (TFun _ (Fun (SFun "deltaVec"))) (Tuple [sz2, Var i2, e]) <- build_e
  , i  == i2
  = Just $ pDiag sz sz2 (Lam i tyi e)

{-
-- RULE: build sz (\i. f e1 ... eN ...)  =
--         let tN = eN in
--         build sz (\i. f e1 .. tN ... )
-- { if i is not free in eN }
optBuild ty sz i tyi e
  | Call tf@(TFun _ (Fun (SFun f))) (Tuple [e1,e2]) <- e
  , is_expensive e2 -- try to apply only to "expensive" operations
  , i `notFreeIn` e2
  = Just $ Let tmp e2 $ pBuild sz (Lam i tyi (Call tf (Tuple [e1, Var tmp])))
  where
    tmp = newVarNotInT (typeof e2) (pBuild sz (Lam i tyi e)) -- slightly inefficient to reassemble outer expr here
    is_expensive (Var _) = False
    is_expensive (Konst _) = False
    is_expensive _ = False
-}

-- build sz (\i. e1 * e2)  = (build sz (\i.e1)) * e2
-- { if i is not free in e2 }
optBuild ty sz i tyi e
  | Call (TFun _ (Fun (SFun "*"))) (Tuple [e1,e2]) <- e
  , i `notFreeIn` e2
  , is_expensive e2
  = Just (Call (TFun ty (Fun (SFun "mul$Vec<R>$R"))) (Tuple [pBuild sz (Lam i tyi e1), e2]))
  where
      is_expensive (Call _ _) = True
      is_expensive _ = False

optBuild ty sz i tyi e = Nothing


-----------------------
optGradFun :: HasCallStack => Type -> Type -> FunId -> TExpr -> Maybe TExpr
-- Inline the definitions for grad(+), grad(*) etc

-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optGradFun s t (SFun "+") _ = Just (lmHCat [lmOne t, lmOne t])

optGradFun s t (SFun "*") (Tuple [x,y])
  = Just (lmHCat [lmScale t y, lmScale t x])

optGradFun s t (SFun "/") (Tuple [x,y])
  = Just (lmHCat [ lmScale t (pDiv (kTFloat 1.0) y)
                  , lmScale t (pNeg (pDiv x (pMul y y)))])
-- fst :: (a,b) -> a
-- Dfst(x,y) :: (a,b) -o a
optGradFun s t (SelFun i n) (Tuple es) = Just (lmHCat [ if i == j then lmOne t else lmZero (typeof (es!!(j-1))) t
                                                       | j <- [1..n] ])

optGradFun s t (SFun "sum") e
  = Just (lmBuildT (pSize e) (Lam (TVar TypeInteger $ Simple "si") TypeInteger (lmOne t)))


optGradFun s t (SFun "size") e
  = Just $ lmZero s t

optGradFun s t (SFun "index") (Tuple [i,v])
  = Just (primDindex i v)

optGradFun s t (SFun "neg") e = Just (lmScale t (kTFloat $ -1.0))

optGradFun s t (SFun "exp") e = Just (lmScale t (pExp e))

optGradFun s t (SFun "log") e = Just (lmScale t (pDiv (kTFloat 1.0) e))

optGradFun _ _ f _ = optTrace("No opt for " ++ pps f ) $ Nothing


--------------
optLM :: HasCallStack => Type -> Type -> String -> TExpr -> Maybe TExpr

optLM s t "lmTranspose" m = optTrans m

optLM s t "lmCompose" (Tuple [f,g])
  | isLMOne f  = Just g
  | isLMOne g  = Just f
  | isLMZero f = Just $ lmZero s t
  | isLMZero g = Just $ lmZero s t

  -- Scale(x) . Scale(y) = Scale( xy )
  | Call (TFun tyf (LMFun "lmScale")) (Tuple [t1,x]) <- f
  , Call (TFun tyg (LMFun "lmScale")) (Tuple [t2,y]) <- g
  = Just $ lmScale t (pMul x y)

  -- (f . g) . h   =>   f . (g . h)
  | Call (TFun tyc (LMFun "lmCompose")) (Tuple [p1,p2]) <- f
  = optLM s t "lmCompose" (Tuple [p1, lmCompose p2 g])

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call (TFun tyg (LMFun "lmHCat")) (Tuple qs) <- g
  = Just (lmHCat (map (lmCompose f) qs))

  | Call (TFun tyf (LMFun "lmHCat")) (Tuple ps) <- f
  , Call (TFun tyg (LMFun "lmVCat")) (Tuple qs) <- g
  = assertEqualThen "H o V" (length ps) (length qs) $
    Just (lmAdds (zipWith lmCompose ps qs))

optLM s t "lmVCat" (Tuple es)
  | all isLMZero es = Just $ lmZero s t

-- Add(0, x) = x = Add(x, 0)
optLM s t "lmAdd" (Tuple [p,q])
  | isLMZero p = Just q
  | isLMZero q = Just p
-- Add(Scale(x), Scale(y)) = Scale(Add(x,y))
  | Call (TFun typ (LMFun "lmScale")) (Tuple [t1, x]) <- p
  , Call (TFun tyq (LMFun "lmScale")) (Tuple [t2, y]) <- q
  = Just $ lmScale t (pAdd x y)

-- Add(HCat(p1, p2, ...), HCat(q1, q2, ...)) = Hcat(Add(p1, q1), Add(p2, q2), ...)
optLM s t "lmAdd" (Tuple [Call (TFun _ (LMFun "lmHCat")) (Tuple ps), Call (TFun _ (LMFun "lmHCat")) (Tuple qs)])
  = Just (lmHCat (zipWith (\ pi qi -> lmAdds [pi, qi]) ps qs))

optLM s t fun arg = optTrace("No optLM for " ++ fun ++ "(" ++ pps arg ++ ")" ) $ Nothing

---------------
optApplyLM :: Type -> TExpr -> TExpr -> Maybe TExpr
optApplyLM ty (Assert e1 e2) dx
  = Just (Assert e1 (lmApply e2 dx))

optApplyLM ty (Call (TFun (TypeLM s' t') (LMFun f)) es) dx
  = optApplyLMCall f es dx

optApplyLM ty (Let v rhs body) dx
  = Just $
    Let v rhs $
    lmApply body dx

optApplyLM ty (If b et ef) dx
  = Just $ If b (lmApply et dx) (lmApply ef dx)

optApplyLM ty e dx
  = trace ("Apply not optimized: " ++ (take 20 $ show $ ppr e) ++ "...") Nothing

------------------
optApplyLMCall ::
                String -> TExpr   -- f args :: s -o t
             -> TExpr             -- :: S
             -> Maybe (TExpr)     -- :: T
-- Optimise (lmApply (fun arg) dx)
optApplyLMCall "lmZero" _ dx = Just (Konst KZero)
optApplyLMCall "lmOne"  _ dx = Just dx
optApplyLMCall "lmAdd"  (Tuple [f,g]) dx
  = Just (pAdd (lmApply f dx) (lmApply g dx))

optApplyLMCall "lmCompose" (Tuple [f,g]) dx
  = Just (lmApply f (lmApply g dx))

optApplyLMCall "lmVCat" (Tuple es) dx
  = Just (Tuple [lmApply e dx | e <- es])

optApplyLMCall "lmHCat" (Tuple es) dx
  = Just (foldr add (Konst KZero) (es `zip` [1..]))
  where
    n = length es

    add :: (TExpr, Int) -> TExpr -> TExpr
    add (e,i) z = pAdd (lmApply e (pSel i n dx)) z

optApplyLMCall "lmScale" (Tuple [t'',x]) dx
  = Just (pMul x dx)

optApplyLMCall "lmBuild" (Tuple [n, Lam i tyi m]) dx
  = Just (pBuild n (Lam i tyi (lmApply m dx)))

optApplyLMCall "lmBuildT" (Tuple [n, Lam i tyi m]) dx
  = Just (pSum (pBuild n (Lam i tyi (lmApply m (pIndex (Var i) dx)))))

optApplyLMCall fun arg dx
  = trace ("No opt for " ++ (show fun) ++ "(" ++ pps arg ++ ".") Nothing


----------------------
optTrans :: TExpr -> Maybe TExpr
-- Transpose an expression
optTrans (Var (TVar (TypeLM s t) (Grad n d))) =
   Just (Var (TVar (TypeLM t s) (Grad n (flipMode d))))
optTrans (Var _) = Nothing
optTrans (Call (TFun (TypeLM s t) (LMFun f)) a) = optTransCallLM s t f a
optTrans (Assert e1 e2)   = fmap (Assert e1) (optTrans e2)
optTrans (Let (TVar (TypeLM s t) (Grad n d)) rhs body)
  = Just $ Let (TVar (TypeLM t s) (Grad n (flipMode d))) (lmTranspose rhs) $
    lmTranspose body
optTrans (Let var rhs body)
  = Just $ Let var rhs $
    lmTranspose body
optTrans (If b t e)
  = Just $ If b (lmTranspose t) (lmTranspose e)
optTrans e = error ("optTrans: " ++  show e)

optTransCallLM :: Type -> Type -> String -> TExpr -> Maybe TExpr
optTransCallLM s t "lmZero" e  = Just $ lmZero t s
optTransCallLM s t "lmOne"  e  = Just $ lmOne t
optTransCallLM s t "lmScale" (Tuple [t'',e]) = assertEqualThen "otc" t (typeof t'') $
                                               Just $ lmScale t e

optTransCallLM s t "lmTranspose" e = Just e
optTransCallLM s t "lmCompose" (Tuple [f,g])
 = Just (lmCompose (lmTranspose g) (lmTranspose f))
optTransCallLM s t "lmAdd" (Tuple [f,g])
  = Just (lmAdd (lmTranspose f) (lmTranspose g))
optTransCallLM s t "lmVCat" (Tuple es)
  = Just (lmHCat (map lmTranspose es))
optTransCallLM s t "lmHCat" (Tuple es)
  = Just (lmVCat (map lmTranspose es))
optTransCallLM s t "lmBuild" (Tuple [n, Lam i tyi b])
  = Just (lmBuildT n (Lam i tyi (lmTranspose b)))
optTransCallLM s t "lmBuildT" (Tuple [n, Lam i tyi b])
  = Just (lmBuild n (Lam i tyi (lmTranspose b)))

optTransCallLM s t f a = Nothing

--------------------------------------
test_opt:: IO ()
test_opt =
  hspec $ do
    describe "optLM tests" $ do
      it "lmAdd(S(x),S(y)) -> S(x+y)" $
        optLM TypeFloat TypeFloat "lmAdd" (Tuple [lmScale TypeFloat $ kTFloat 1.3, lmScale TypeFloat $ kTFloat 0.4])
        `shouldBe`
        Just (lmScale TypeFloat (mkTCall2 TypeFloat (Fun (SFun "+")) (kTFloat 1.3) (kTFloat 0.4)))

      it "lmAdd(HCat) = HCat(lmAdd) and some more simplifications" $
        let l1 = lmOne TypeFloat
            f2 = kTFloat 2.0
            l2 = lmScale TypeFloat f2
        in
            optE stCreate (lmAdd (lmHCat [l1, l2]) (lmHCat [l2, l2]))
            `shouldBe`
            lmHCat [lmAdd l1 l2, lmScale TypeFloat (pAdd f2 f2)]
