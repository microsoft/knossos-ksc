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
optDefs :: [Def] -> [Def]
optDefs defs = map optDef defs

optDef :: Def -> Def
optDef (Def f as r) = Def f as (simplify r)

simplify :: Expr -> Expr
simplify =  optE . optLets . optE . optLets
  -- Note the extra optLets, which gets simple things,
  -- notably lmOne, to their use sites

{- with that extra optE:

fun logsumexp`(x, dr)
  = assert (size( x ) == size( y ))
    build( size( x ),
           \i. exp( index( i, x ) )
               * ((1.0 / sum( build( size( x ), \i. exp( index( i, x ) ) ) ))
                  * dr) )

  and without
fun logsumexp`(x, dr)
  = assert (size( x ) == size( y ))
    sum( build( size( x ),
                \i. deltaVec( size( x ),
                              i,
                              exp( index( i, x ) )
                              * ((1.0 / sum( build( size( x ), \i. exp( index( i, x ) ) ) ))
                                 * dr) ) ) )

  -}
---------------
optE :: TExpr a -> TExpr a
optE (Tuple es)         = Tuple (map optE es)
optE (Var v)            = Var v
optE (Konst k)          = Konst k
optE (Lam v e)          = Lam v (optE e)
optE (App e1 e2)        = optApp (optE e1) (optE e2)
optE (Assert e1 e2)     = Assert (optE e1) (optE e2)
optE (Let var rhs body) = Let var (optE rhs) (optE body)
optE (If b t e)         = optIf (optE b) (optE t) (optE e)
optE e@(Call fun arg)   = case optCall fun opt_arg of
                            Nothing -> Call fun opt_arg
                            Just r  -> -- pprTrace "optCall"
                                       --   (vcat [ text "Before:" <+> ppr e
                                       --         , text "After: " <+> ppr r])
                                       optE r
                        where
                          opt_arg = optE arg

--------------
optApp :: TExpr (a->b) -> TExpr a -> TExpr b
optApp (Lam v e) a = Let v (optE a) (optE e)
optApp f a         = App f a

--------------
optIf :: TExpr Bool -> TExpr r -> TExpr r -> TExpr r
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

optCall :: Fun
        -> Expr   -- Argument, already optimised
        -> Maybe Expr
-- RULE: f( let x = e in b )  =  let x = e in f(b)
optCall fun (Let v r arg)   = Just (Let v r (Call fun arg))

optCall (LMFun lm)      arg = optLM lm arg
optCall (Fun f)         arg = optFun f arg
optCall (GradFun f Fwd) arg = optGradFun f arg
optCall _ _ = Nothing

-----------------------
optFun :: FunId -> Expr -> Maybe Expr
-- RULE:  sel_i_n (..., ei, ...)  ==>  ei
optFun (SelFun i _) (Tuple es)
  | i <= length es = Just (es !! (i-1))

-- RULE: x+0 = 0+x = x
optFun (SFun "+") (Tuple [x, Konst KZero]) = Just x
optFun (SFun "+") (Tuple [Konst KZero, y]) = Just y

-- RULE: (a1,a2) + (b1,b2) = (a1+a2, b1+b2)
optFun (SFun "+") (Tuple [Tuple es1, Tuple es2])
  | length es1 == length es2 = Just (Tuple (zipWith pAdd es1 es2))

-- RULE: x*0 = 0*x = 0
optFun (SFun "*") (Tuple [x, Konst KZero]) = Just (Konst KZero)
optFun (SFun "*") (Tuple [Konst KZero, y]) = Just (Konst KZero)

-- RULE: size (build (n, _)) = n
optFun (SFun "size") (Call (Fun (SFun "build")) (Tuple [n,_]))
  = Just n

-- RULE: size (x * y) = size(x)
optFun (SFun "size") (Call (Fun (SFun "*")) (Tuple [x,_]))
  = Just (mkSCall1 "size" x)

-- RULE: index j (build n f) = f j
optFun (SFun "index") (Tuple [ ei, arr ])
  | Call (Fun (SFun "build")) (Tuple [_, f]) <- arr
  , Lam i e <- f
  = Just (Let i ei e)

-- RULE: sum (build n (\i. if (i==ej) then v else 0)
--  = let i = ej in v
optFun (SFun "sum")   arg = optSum arg
optFun (SFun "build") (Tuple [sz, Lam i e2]) = optBuild sz i e2

optFun _ _ = Nothing

-----------------------
optSum :: TExpr (Vector a) -> Maybe (TExpr a)

-- RULE: sum (build n (\i. (e1,e2,...)))
--       = (sum (build n (\i.e1)), sum (build n (\i.e2)), ...)
optSum (Call (Fun (SFun "build")) (Tuple [n, Lam i (Tuple es)]))
   = Just $ Tuple (map (\e -> pSum (pBuild n (Lam i e))) es)

-- RULE: sum (diag sz f)  =  build sz f
optSum (Call (Fun (SFun "diag")) (Tuple [sz, f]))
  = Just $ pBuild sz f

-- RULE: sum (deltaVec sz i e) = e
optSum (Call (Fun (SFun "deltaVec")) (Tuple [_, _, e]))
  = Just e

optSum e = Nothing

-----------------------
optBuild :: TExpr Int -> Var -> TExpr a -> Maybe (TExpr (Vector a))

-- RULE: build sz (\i. delta i ex eb)  =  let i = ex in
--                                        deltaVec sz i eb
--       (if i is not free in ex)
-- NB: however, i might be free in eb
optBuild sz i e
  | Call (Fun (SFun "delta")) (Tuple [e1,e2,eb]) <- e
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
  | Call (Fun (SFun "deltaVec")) (Tuple [sz2, Var i2, e]) <- build_e
  , sz == sz2
  , i  == i2
  = Just $ pDiag sz (Lam i e)

-- RULE: build sz (\i. f e1 ... eN ...)  =
--         let tN = eN in
--         build sz (\i. f e1 .. tN ... )
-- { if i is not free in eN }
optBuild sz i e
  | Call (Fun (SFun f)) (Tuple [e1,e2]) <- e
  , is_expensive e2 -- try to apply only to "expensive" operations
  , i `notFreeIn` e2
  = Just $ Let tmp e2 $ pBuild sz (Lam i (Call (Fun (SFun f)) (Tuple [e1, (Var tmp)])))
  where
    tmp = newVarNotIn (pBuild sz (Lam i e)) -- slightly inefficient to reassemble outer expr here
    is_expensive (Var _) = False
    is_expensive (Konst _) = False
    is_expensive _ = False

-- build sz (\i. e1 * e2)  = (build sz (\i.e1)) * e2
-- { if i is not free in e2 }
optBuild sz i e
  | Call (Fun (SFun "*")) (Tuple [e1,e2]) <- e
  , i `notFreeIn` e2
  , is_expensive e2
  = Just (Call (Fun (SFun "mul$Vec<R>$R")) (Tuple [pBuild sz (Lam i e1), e2]))
  where
      is_expensive (Call _ _) = True
      is_expensive _ = False

optBuild sz i e = Nothing


-----------------------
optGradFun :: FunId -> Expr -> Maybe Expr
-- Inline the definitions for grad(+), grad(*) etc

-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optGradFun (SFun "+") _ = Just (lmHCat [lmOne, lmOne])

optGradFun (SFun "*") (Tuple [x,y])
  = Just (lmHCat [lmScale y, lmScale x])

optGradFun (SFun "/") (Tuple [x,y])
  = Just (lmHCat [ lmScale (pDiv (kInt 1) y)
                 , lmScale (pNeg (pDiv x (pMul y y)))])
-- fst :: (a,b) -> a
-- Dfst(x,y) :: (a,b) -o a
optGradFun (SelFun i n) _ = Just (lmHCat [ if i == j then lmOne else lmZero
                                          | j <- [1..n] ])

optGradFun (SFun "sum") e
  = Just (lmBuildT (pSize e) (Lam (Simple "si") lmOne))

optGradFun (SFun "index") (Tuple [i,v])
  = Just (lmHCat [ lmZero
                 , lmBuildT (pSize v) (Lam (Simple "ii") (lmDelta (Var (Simple "ii")) i)) ])

optGradFun (SFun "neg") e = Just (lmScale (kFloat $ -1.0))

optGradFun (SFun "exp") e = Just (lmScale (pExp e))

optGradFun (SFun "log") e = Just (lmScale (pDiv (kFloat 1.0) e))

optGradFun _ _ = Nothing


--------------
optLM :: String -> Expr -> Maybe Expr
optLM "lmApply" (Tuple [f,a]) = optApply f a

optLM "lmTranspose" m = optTrans m

optLM "lmCompose" (Tuple [f,g])
  | isLMOne f  = Just g
  | isLMOne g  = Just f
  | isLMZero f = Just lmZero
  | isLMZero g = Just lmZero

  -- Scalar(x) . Scalar(y) = Scalar( xy )
  | Call (LMFun "lmScale") x <- f
  , Call (LMFun "lmScale") y <- g
  = Just (lmScale (pMul x y))

  -- (f . g) . h   =>   f . (g . h)
  | Call (LMFun "lmCompose") (Tuple [p1,p2]) <- f
  = optLM "lmCompose" (Tuple [p1, lmCompose p2 g])

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call (LMFun "lmHCat") (Tuple qs) <- g
  = Just (lmHCat (map (lmCompose f) qs))

  | Call (LMFun "lmHCat") (Tuple ps) <- f
  , Call (LMFun "lmVCat")  (Tuple qs) <- g
  , length ps == length qs
  = Just (lmAdds (zipWith lmCompose ps qs))

optLM "lmVCat" (Tuple es)
  | all isLMZero es = Just lmZero

-- Add(0, x) = x = Add(x, 0)
optLM "lmAdd" (Tuple [p,q])
  | isLMZero p = Just q
  | isLMZero q = Just p
-- Add(Scale(x), Scale(y)) = Scale(Add(x,y))
  | Call (LMFun "lmScale") x <- p
  , Call (LMFun "lmScale") y <- q
  = Just (lmScale (pAdd x y))

-- Add(HCat(p1, p2, ...), HCat(q1, q2, ...)) = Hcat(Add(p1, q1), Add(p2, q2), ...)
optLM "lmAdd" (Tuple [Call (LMFun "lmHCat") (Tuple ps), Call (LMFun "lmHCat") (Tuple qs)])
  = Just (lmHCat (zipWith (\ pi qi -> lmAdds [pi, qi]) ps qs))

optLM fun arg = Nothing

test_opt:: IO ()
test_opt =
  hspec $ do
    describe "optLM tests" $ do
      it "lmAdd(S(x),S(y)) -> S(x+y)" $
        optLM "lmAdd" (Tuple [lmScale $ kFloat 1.3, lmScale $ kFloat 0.4])
        `shouldBe`
        Just (lmScale (mkSCall2 "+" (kFloat 1.3) (kFloat 0.4)))

      it "lmAdd(HCat) = HCat(lmAdd)" $
        optE (lmAdd (lmHCat (map kInt [1,2,3])) (lmHCat (map kInt [11,22,33])))
        `shouldBe`
        lmHCat [lmAdd (kInt 1) (kInt 11), lmAdd (kInt 2) (kInt 22), lmAdd (kInt 3) (kInt 33)]

---------------
optApply :: TExpr (LM a b) -> TExpr a -> Maybe (TExpr b)
optApply (Assert e1 e2) dx
  = Just (Assert e1 (lmApply e2 dx))

optApply (Call f es) dx
  = optApplyCall f es dx

optApply (Let v rhs body) dx
  = Just $
    Let v rhs $
    lmApply body dx

optApply (If b t e) dx
  = Just $ If b (lmApply t dx) (lmApply e dx)

optApply e dx
  = Nothing

------------------
optApplyCall :: Fun -> Expr      -- f args :: s -o t
             -> TExpr s          -- :: S
             -> Maybe (TExpr t)  -- :: T
-- Optimise (lmApply (fun arg) dx)
optApplyCall (LMFun "lmZero") _  dx = Just (Konst KZero)
optApplyCall (LMFun "lmOne")  _ dx = Just dx
optApplyCall (LMFun "lmAdd")  (Tuple [f,g]) dx
  = Just (pAdd (lmApply f dx) (lmApply g dx))

optApplyCall (LMFun "lmCompose") (Tuple [f,g]) dx
  = Just (lmApply f (lmApply g dx))

optApplyCall (LMFun "lmVCat") (Tuple es) dx
  = Just (Tuple [lmApply e dx | e <- es])

optApplyCall (LMFun "lmHCat") (Tuple es) dx
  = Just (foldr add (Konst KZero) (es `zip` [1..]))
  where
    n = length es

    add :: (Expr, Int) -> Expr -> Expr
    add (e,i) z = pAdd (lmApply e (pSel i n dx)) z

optApplyCall (LMFun "lmScale") x dx
  = Just (pMul x dx)

optApplyCall (LMFun "lmBuild") (Tuple [n, Lam i m]) dx
  = Just (pBuild n (Lam i (lmApply m dx)))

optApplyCall (LMFun "lmBuildT") (Tuple [n, Lam i m]) dx
  = Just (pSum (pBuild n (Lam i (lmApply m (pIndex (Var i) dx)))))

optApplyCall fun arg dx
  = Nothing


----------------------
optTrans :: TExpr (LM a b) -> Maybe (TExpr (LM b a))
-- Transpose an expression
optTrans (Var (Grad n d)) = Just (Var (Grad n (flipMode d)))
optTrans (Var (Simple v)) = Nothing
optTrans (Call f a)       = optTransCall f a
optTrans (Assert e1 e2)   = fmap (Assert e1) (optTrans e2)
optTrans (Let (Grad n d) rhs body)
  = Just $ Let (Grad n (flipMode d)) (lmTranspose rhs) $
    lmTranspose body
optTrans (Let var rhs body)
  = Just $ Let var rhs $
    lmTranspose body
optTrans (If b t e)
  = Just $ If b (lmTranspose t) (lmTranspose e)
optTrans e = error ("optTrans: " ++  show e)

optTransCall :: Fun -> Expr -> Maybe Expr
optTransCall (LMFun "lmZero") _  = Just lmZero
optTransCall (LMFun "lmOne")  _  = Just lmOne
optTransCall (LMFun "lmScale") e = Just (lmScale e)

optTransCall (LMFun "lmTranspose") e = Just e
optTransCall (LMFun "lmCompose") (Tuple [f,g])
  = Just (lmCompose (lmTranspose g) (lmTranspose f))
optTransCall (LMFun "lmAdd") (Tuple [f,g])
  = Just (lmAdd (lmTranspose f) (lmTranspose g))
optTransCall (LMFun "lmVCat") (Tuple es)
  = Just (lmHCat (map lmTranspose es))
optTransCall (LMFun "lmHCat") (Tuple es)
  = Just (lmVCat (map lmTranspose es))
optTransCall (LMFun "lmBuild") (Tuple [n, Lam i b])
  = Just (lmBuildT n (Lam i (lmTranspose b)))
optTransCall (LMFun "lmBuildT") (Tuple [n, Lam i b])
  = Just (lmBuild n (Lam i (lmTranspose b)))

optTransCall f a = Nothing

