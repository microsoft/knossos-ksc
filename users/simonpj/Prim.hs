module Prim where

import Data.Array

import Lang

type Nat = Int
type Vector t = Array Nat t

data LM a b   -- Linear maps

lm :: String -> Type -> Type -> [Expr] -> Expr
lm f s t args = mkCallF (TypeLM s t) (LMFun f) args

lmZero :: Type -> Type -> Expr
lmZero s t = lm "lmZero" s t []

lmOne :: Type -> Expr
lmOne t = lm "lmOne" t t []

lmAdd :: Expr -> Expr -> Expr
lmAdd f@(Expr (TypeLM s1 t1) _) g@(Expr (TypeLM s2 t2) _) = assertEqualThen "lmAdd s" s1 s2 $ assertEqualThen "lmAdd t" t1 t2 $ lm "lmAdd" s1 t1 [f,g]

lmAdds :: [TExpr (LM a b)]-> TExpr (LM a b)
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmScale :: Type -> TExpr Float -> TExpr (LM Float Float)
lmScale t f = lm "lmScale" t t [f]

lmCompose :: TExpr (LM b c) -> TExpr (LM a b) -> TExpr (LM a c)
lmCompose f@(Expr (TypeLM b c) _) g@(Expr (TypeLM a b1) _) = assertEqualThen "lmCompose" b b1 $ lm "lmCompose" a c [f,g]

lmVCat :: TExpr (LM a b1) -> TExpr (LM a b2) -> TExpr (LM a (b1,b2))
lmVCat f@(Expr (TypeLM a b1) _) g@(Expr (TypeLM a' b2) _) = assertEqualThen "lmVCat" a a' $ lm "lmVCat" a (TypeTuple [b1, b2]) [f,g]

lmVCats :: [TExpr (LM a bi)] -> TExpr (LM a bs)
lmVCats es = lm "lmVCat" (typeOfSrc $ head es) (TypeTuple $ map typeOfDst es) es

lmHCat :: TExpr (LM a1 b) -> TExpr (LM a2 b) -> TExpr (LM (a1,a2) b)
lmHCat f@(Expr (TypeLM a1 b) _) g@(Expr (TypeLM a2 b') _) = assertEqualThen "lmVCat" b b' $ lm "lmHCat" (TypeTuple [a1, a2]) b [f,g]

lmHCats :: [TExpr (LM a bi)] -> TExpr (LM a bs)
lmHCats es = lm "lmHCat" (TypeTuple $ map typeOfSrc es) (typeOfDst $ head es) es

lmTranspose :: TExpr (LM a b) -> TExpr (LM b a)
lmTranspose m@(Expr (TypeLM a b) _) = lm "lmTranspose" b a [m]

lmApply :: TExpr (LM a b) -> TExpr a -> TExpr b
lmApply m@(Expr (TypeLM a b) _) arg@(Expr a' _) = assertEqualThen "lmApply" a a' $ mkCallF b (LMFun "lmApply") [m, arg]

lmBuild :: TExpr Int -> TExpr (Int -> LM s t) -> TExpr (LM s (Vector t))
lmBuild e f@(Expr (TypeLambda TypeInteger (TypeLM s t)) _) = lm "lmBuild" s (TypeVec t) [e, f]

lmBuildT :: TExpr Int -> TExpr (Int -> LM t s) -> TExpr (LM (Vector t) s)
lmBuildT e f@(Expr (TypeLambda TypeInteger (TypeLM t s)) _) = lm "lmBuildT" (TypeVec t) s [e, f]

lmDelta :: Type -> TExpr Nat -> TExpr Nat -> TExpr (LM a a)
lmDelta t i j = mkIf (pEqual i j) (lmOne t) (lmZero t t)

isLMOne, isLMZero :: Expr -> Bool
isLMOne (Expr _ (Call (TFun ty (LMFun "lmOne")) _)) = True
isLMOne _ = False

isLMZero (Expr _ (Call (TFun ty (LMFun "lmOne")) _)) = True
isLMZero _ = False

-----------------------
-- Assertion

assertEqual :: TExpr a -> TExpr a -> TExpr b -> TExpr b
assertEqual x y body
  = mkAssert (mkSCall2 TypeBool "==" x y) body

isEqualityCall :: TExpr Bool -> Maybe (TExpr a, TExpr a)
isEqualityCall (Expr _ (Call (TFun TypeBool (Fun (SFun "=="))) (Expr _ (Tuple [e1,e2]))))
  = Just (e1,e2)
isEqualityCall _ = Nothing

-----------------------
-- Delta and diag

pDelta :: TExpr a -> TExpr a -> TExpr b -> TExpr b
-- delta i j e  =  if i==j then e else zero
pDelta ei ej e@(Expr ty _) = mkSCall3 ty "delta" ei ej e

pDeltaVec :: TExpr Int -> TExpr Int -> TExpr a -> TExpr (Vector a)
-- deltaVec size i e = build size (\j. delta i j e)
pDeltaVec sz ei e@(Expr ty _) = mkSCall3 (TypeVec ty) "deltaVec" sz ei e

pDiag :: TExpr Int -> (TExpr (Int -> a)) -> TExpr (Vector (Vector a))
-- diag sz (\i. e) = build sz (\i. deltaVec sz i e)
pDiag sz d@(Expr (TypeLambda TypeInteger a) _) = mkSCall2 (TypeVec (TypeVec a)) "diag" sz d

---------------------------
-- "User-defined" functions
---------------------------
pAdd, pMul :: Expr -> Expr -> Expr
pAdd a b = mkSCall2 (typeOf a) "+" a b
pMul a b = mkSCall2 (typeOf a) "*" a b
pDiv a b = mkSCall2 (typeOf a) "/" a b
pNeg x   = mkSCall1 (typeOf x) "neg" x
pExp x   = mkSCall1 (typeOf x) "exp" x
pLog x   = mkSCall1 (typeOf x) "log" x
pEqual a b = mkSCall2 TypeBool "==" a b

pBuild :: TExpr Nat -> TExpr (Nat -> t) -> TExpr (Vector t)
pBuild n@(Expr TypeInteger _) f@(Expr (TypeLambda TypeInteger t) _) = mkSCall2 (TypeVec t) "build" n f

pIndex :: TExpr Int -> TExpr (Vector a) -> TExpr a
pIndex i e@(Expr (TypeVec t) _) = mkSCall2 t "index" i e

pSum :: TExpr (Vector a) -> TExpr a
pSum e@(Expr (TypeVec t) _) = mkSCall1 t "sum" e

pSize :: TExpr (Vector a) -> TExpr Nat
pSize e = mkSCall1 TypeInteger "size" e

pSel :: Int -> Int -> Expr -> Expr
pSel i n e@(Expr (TypeTuple ts) _) = mkCallF (ts!!(i-1)) (Fun (SelFun i n)) [e]

pFst,pSnd :: Expr -> Expr
pFst   = pSel 1 2
pSnd   = pSel 2 2

