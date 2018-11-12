module Opt( optLets, optDef, optDefs, optE, simplify, test_opt ) where

import Lang
import Prim
import OptLet
import Text.PrettyPrint
import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace
import Test.Hspec

---------------
optDefs :: [TDef] -> [TDef]
optDefs defs = map optDef defs

optDef :: TDef -> TDef
optDef (DefX f as r) = DefX f as (simplify r)

simplify :: TExpr -> TExpr
simplify =  optE . optLets . optE . optLets
  -- Note the extra optLets, which gets simple things,
  -- notably lmOne, to their use sites

---------------
optE :: TExpr -> TExpr
optE (Tuple es)         = Tuple (map optE es)
optE (Var v)            = Var v
optE (Konst k)          = Konst k
optE (Lam v ty e)        = Lam v ty (optE e)
optE (App e1 e2)        = optApp (optE e1) (optE e2)
optE (Assert e1 e2)     = Assert (optE e1) (optE e2)
optE (Let var rhs body) = Let var (optE rhs) (optE body)
optE (If b t e)         = optIf (optE b) (optE t) (optE e)
optE e@(Call tfun arg)   = case optCall tfun opt_arg of
                            Nothing -> Call tfun opt_arg
                            Just r  -> -- pprTrace "optCall"
                                       --   (vcat [ text "Before:" <+> ppr e
                                       --         , text "After: " <+> ppr r])
                                       optE r
                          where
                            opt_arg = optE arg

--------------
optApp :: TExpr -> TExpr -> TExpr
optApp (Lam v ty e) a = Let v (optE a) (optE e)
optApp f a            = App f a

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

optCall (TFun ty (Fun f)) arg =
  optFun ty f arg

optCall (TFun (TypeLM s t) (LMFun lm)) arg =
  case optLM s t lm arg of
  Just e -> Just $ assertEqualThen "optLM" (TypeLM s t) (typeof e) $ e
  Nothing -> Nothing

optCall (TFun (TypeLM s t) (GradFun f Fwd)) arg = 
  case optGradFun s t f arg of
  Just e -> Just $ assertEqualThen "optGradFun" (TypeLM s t) (typeof e) $ e
  Nothing -> Nothing

optCall _ _ = Nothing

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
optBuild :: Type -> TExpr -> TVar Var -> Type -> TExpr -> Maybe TExpr

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
optGradFun s t (SFun "+") _ = Just (lmHCat (lmOne t) (lmOne t))

optGradFun s t (SFun "*") (Tuple [x,y])
  = Just (lmHCats [lmScale t y, lmScale t x])

optGradFun s t (SFun "/") (Tuple [x,y])
  = Just (lmHCats [ lmScale t (pDiv (kTFloat 1.0) y)
                  , lmScale t (pNeg (pDiv x (pMul y y)))])
-- fst :: (a,b) -> a
-- Dfst(x,y) :: (a,b) -o a
optGradFun s t (SelFun i n) (Tuple es) = Just (lmHCats [ if i == j then lmOne t else lmZero (typeof (es!!(j-1))) t
                                                       | j <- [1..n] ])

optGradFun s t (SFun "sum") e
  = Just (lmBuildT (pSize e) (Lam (TVar TypeInteger $ Simple "si") TypeInteger (lmOne t)))

optGradFun s t (SFun "size") e
  = Just $ lmZero s t

optGradFun s t (SFun "index") (Tuple [i,v])
  = Just (lmHCats [ lmZero TypeInteger t
                  , lmBuildT (pSize v) (Lam ii TypeInteger 
                                           (lmDelta t (Var ii) i)) ])
    where ii = TVar TypeInteger $ Simple "ii"

optGradFun s t (SFun "neg") e = Just (lmScale t (kTFloat $ -1.0))

optGradFun s t (SFun "exp") e = Just (lmScale t (pExp e))

optGradFun s t (SFun "log") e = Just (lmScale t (pDiv (kTFloat 1.0) e))

optGradFun _ _ _ _ = Nothing


--------------
optLM :: HasCallStack => Type -> Type -> String -> TExpr -> Maybe TExpr
optLM s t "lmApply" (Tuple [f,a]) = optApplyLM s t f a

optLM s t "lmTranspose" m = optTrans m

optLM s t "lmCompose" (Tuple [f,g])
  | isLMOne f  = Just g
  | isLMOne g  = Just f
  | isLMZero f = Just $ lmZero s t
  | isLMZero g = Just $ lmZero s t

  -- Scale(x) . Scale(y) = Scale( xy )
  | Call (TFun tyf (LMFun "lmScale")) x <- f
  , Call (TFun tyg (LMFun "lmScale")) y <- g
  = Just (lmScale t (pMul x y))

  -- (f . g) . h   =>   f . (g . h)
  | Call (TFun tyc (LMFun "lmCompose")) (Tuple [p1,p2]) <- f
  = optLM s t "lmCompose" (Tuple [p1, lmCompose p2 g])

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call (TFun tyg (LMFun "lmHCat")) (Tuple qs) <- g
  = Just (lmHCats (map (lmCompose f) qs))

  | Call (TFun tyf (LMFun "lmHCat")) (Tuple ps) <- f
  , Call (TFun tyg (LMFun "lmVCat")) (Tuple qs) <- g
  , length ps == length qs
  = Just (lmAdds (zipWith lmCompose ps qs))

optLM s t "lmVCat" (Tuple es)
  | all isLMZero es = Just $ lmZero s t

-- Add(0, x) = x = Add(x, 0)
optLM s t "lmAdd" (Tuple [p,q])
  | isLMZero p = Just q
  | isLMZero q = Just p
-- Add(Scale(x), Scale(y)) = Scale(Add(x,y))
  | Call (TFun typ (LMFun "lmScale")) x <- p
  , Call (TFun tyq (LMFun "lmScale")) y <- q
  = Just $ lmScale t (pAdd x y)

-- Add(HCat(p1, p2, ...), HCat(q1, q2, ...)) = Hcat(Add(p1, q1), Add(p2, q2), ...)
optLM s t "lmAdd" (Tuple [Call (TFun _ (LMFun "lmHCat")) (Tuple ps), Call (TFun _ (LMFun "lmHCat")) (Tuple qs)])
  = Just (lmHCats (zipWith (\ pi qi -> lmAdds [pi, qi]) ps qs))

optLM s t fun arg = Nothing

test_opt:: IO ()
test_opt =
  hspec $ do
    describe "optLM tests" $ do
      it "lmAdd(S(x),S(y)) -> S(x+y)" $
        optLM TypeFloat TypeFloat "lmAdd" (Tuple [lmScale TypeFloat $ kTFloat 1.3, lmScale TypeFloat $ kTFloat 0.4])
        `shouldBe`
        Just (lmScale TypeFloat (mkTCall2 TypeFloat (Fun (SFun "+")) (kTFloat 1.3) (kTFloat 0.4)))

      it "lmAdd(HCat) = HCat(lmAdd)" $
        optE (lmAdd (lmHCats (map kTInt [1,2,3])) (lmHCats (map kTInt [11,22,33])))
        `shouldBe`
        lmHCats [lmAdd (kTInt 1) (kTInt 11), lmAdd (kTInt 2) (kTInt 22), lmAdd (kTInt 3) (kTInt 33)]

---------------
optApplyLM :: Type -> Type -> TExpr -> TExpr -> Maybe TExpr
optApplyLM s t (Assert e1 e2) dx
  = Just (Assert e1 (lmApply e2 dx))

optApplyLM s t (Call (TFun (TypeLM ss s1) (LMFun f)) es) dx
  = assertEqualThen ("lmApply o " ++ show f) s s1
    optApplyLMCall s t f es dx

optApplyLM s t (Let v rhs body) dx
  = Just $
    Let v rhs $
    lmApply body dx

optApplyLM s t (If b et ef) dx
  = Just $ If b (lmApply et dx) (lmApply ef dx)

optApplyLM s t e dx
  = Nothing

------------------
optApplyLMCall :: Type -> Type    -- s t
             -> String -> TExpr   -- f args :: s -o t
             -> TExpr             -- :: S
             -> Maybe (TExpr)     -- :: T
-- Optimise (lmApply (fun arg) dx)
optApplyLMCall s t "lmZero" _ dx = Just (Konst KZero)
optApplyLMCall s t "lmOne"  _ dx = Just dx
optApplyLMCall s t "lmAdd"  (Tuple [f,g]) dx
  = Just (pAdd (lmApply f dx) (lmApply g dx))

optApplyLMCall s t "lmCompose" (Tuple [f,g]) dx
  = Just (lmApply f (lmApply g dx))

optApplyLMCall s t "lmVCat" (Tuple es) dx
  = Just (Tuple [lmApply e dx | e <- es])

optApplyLMCall s t "lmHCat" (Tuple es) dx
  = Just (foldr add (Konst KZero) (es `zip` [1..]))
  where
    n = length es

    add :: (TExpr, Int) -> TExpr -> TExpr
    add (e,i) z = pAdd (lmApply e (pSel i n dx)) z

optApplyLMCall s t "lmScale" x dx
  = Just (pMul x dx)

optApplyLMCall s t "lmBuild" (Tuple [n, Lam i tyi m]) dx
  = Just (pBuild n (Lam i tyi (lmApply m dx)))

optApplyLMCall s t "lmBuildT" (Tuple [n, Lam i tyi m]) dx
  = Just (pSum (pBuild n (Lam i tyi (lmApply m (pIndex (Var i) dx)))))

optApplyLMCall s t fun arg dx
  = Nothing


----------------------
optTrans :: TExpr -> Maybe TExpr
-- Transpose an expression
optTrans (Var (TVar (TypeLM s t) (Grad n d))) = Just (Var (TVar (TypeLM t s) (Grad n (flipMode d))))
optTrans (Var _) = Nothing
optTrans (Call (TFun (TypeLM s t) (LMFun f)) a)       = optTransCallLM s t f a
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
optTransCallLM s t "lmZero" _  = Just $ lmZero t s
optTransCallLM s t "lmOne"  _  = Just $ lmOne s
optTransCallLM s t "lmScale" e = Just $ lmScale s e

optTransCallLM s t "lmTranspose" e = Just e
optTransCallLM s t "lmCompose" (Tuple [f,g])
 = Just (lmCompose (lmTranspose g) (lmTranspose f))
optTransCallLM s t "lmAdd" (Tuple [f,g])
  = Just (lmAdd (lmTranspose f) (lmTranspose g))
optTransCallLM s t "lmVCat" (Tuple es)
  = Just (lmHCats (map lmTranspose es))
optTransCallLM s t "lmHCat" (Tuple es)
  = Just (lmVCats (map lmTranspose es))
optTransCallLM s t "lmBuild" (Tuple [n, Lam i tyi b])
  = Just (lmBuildT n (Lam i tyi (lmTranspose b)))
optTransCallLM s t "lmBuildT" (Tuple [n, Lam i tyi b])
  = Just (lmBuild n (Lam i tyi (lmTranspose b)))

optTransCallLM s t f a = Nothing

