module Prim where

import Data.Array

import Lang

type Nat = Int
type Vector t = Array Nat t

data LM a b   -- Linear maps

lm :: String -> [TExpr (LM a b)] -> TExpr (LM a b)
lm f args = Call (LMFun f) (Tuple args)

lmZero :: TExpr (LM a b)
lmZero = lm "lmZero" []

lmOne :: TExpr (LM a a)
lmOne = lm "lmOne" []

lmAdd :: TExpr (LM a b) -> TExpr (LM a b) -> TExpr (LM a b)
lmAdd f g = lm "lmAdd" [f,g]

lmAdds :: [TExpr (LM a b)]-> TExpr (LM a b)
lmAdds [] = lmZero
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmScale :: TExpr Float -> TExpr (LM Float Float)
lmScale f = lm "lmScale" [f]

lmCompose :: TExpr (LM b c) -> TExpr (LM a b) -> TExpr (LM a c)
lmCompose f g = lm "lmCompose" [f,g]

lmVCat :: [Expr] -> Expr
-- Written as "x" in the paper
-- TExpr (LM a b1) -> TExpr (LM a b2) -> TExpr (LM a (b1,b2))
lmVCat = lm "lmVCat"

lmHCat :: [Expr] -> Expr
-- Written as "bow-tie" in the paper
-- TExpr (LM a1 b) -> TExpr (LM a2 b) -> TExpr (LM (a1,a2) b)
lmHCat = lm "lmHCat"

lmTranspose :: TExpr (LM a b) -> TExpr (LM b a)
lmTranspose m = Call (LMFun "lmTranspose") m

lmApply :: TExpr (LM a b) -> TExpr a -> TExpr b
lmApply lm arg = Call (LMFun "lmApply") (Tuple [lm, arg])

lmBuild :: TExpr Int -> TExpr (Int -> LM s t) -> TExpr (LM s (Vector t))
lmBuild e f = Call (LMFun "lmBuild") (Tuple [e, f])

lmBuildT :: TExpr Int -> TExpr (Int -> LM t s) -> TExpr (LM (Vector t) s)
lmBuildT e f = Call (LMFun "lmBuildT") (Tuple [e, f])

lmDelta :: TExpr Nat -> TExpr Nat -> TExpr (LM a a)
lmDelta i j = If (pEqual i j) lmOne lmZero

isLMOne, isLMZero :: Expr -> Bool
isLMOne (Call (LMFun "lmOne") _) = True
isLMOne _ = False

isLMZero (Call (LMFun "lmZero") _) = True
isLMZero _ = False

gradSelFun :: Int -> Int -> Expr
-- (gradSelFun i n) selects the i'th component of a n-tuple
-- Special case for 1-tuples
-- Result expr has type (t1, ..., tn) -o ti
gradSelFun i 1 = lmOne
gradSelFun i n = Call (GradFun (SelFun i n) Fwd) (Tuple [])

-----------------------
-- Assertion

assertEqual :: TExpr a -> TExpr a -> TExpr b -> TExpr b
assertEqual x y body
  = Assert (mkSCall2 "==" x y) body

isEqualityCall :: TExpr Bool -> Maybe (TExpr a, TExpr a)
isEqualityCall (Call (Fun (SFun "==")) (Tuple [e1,e2]))
  = Just (e1,e2)
isEqualityCall _ = Nothing

-----------------------
-- Delta and diag

pDelta :: TExpr a -> TExpr a -> TExpr b -> TExpr b
-- delta i j e  =  if i==j then e else zero
pDelta ei ej e = mkSCall3 "delta" ei ej e

pDeltaVec :: TExpr Int -> TExpr Int -> TExpr a -> TExpr (Vector a)
-- deltaVec size i e = build size (\j. delta i j e)
pDeltaVec sz ei e = mkSCall3 "deltaVec" sz ei e

pDiag :: TExpr Int -> (TExpr (Int -> a)) -> TExpr (Vector (Vector a))
-- diag sz (\i. e) = build sz (\i. deltaVec sz i e)
pDiag sz d = mkSCall2 "diag" sz d

---------------------------
-- "User-defined" functions
---------------------------
pAdd, pMul :: TExpr Float -> TExpr Float -> TExpr Float
pAdd a b = mkSCall2 "+" a b
pMul a b = mkSCall2 "*" a b
pDiv a b = mkSCall2 "/" a b
pNeg x   = mkSCall1 "neg" x
pExp x   = mkSCall1 "exp" x
pLog x   = mkSCall1 "log" x
pEqual a b = mkSCall2 "==" a b

pBuild :: TExpr Nat -> TExpr (Nat -> t) -> TExpr (Vector t)
pBuild n f = mkSCall2 "build" n f

pIndex :: TExpr Int -> TExpr (Vector a) -> TExpr a
pIndex i e = mkSCall2 "index" i e

pSum :: TExpr (Vector Float) -> TExpr Float
pSum e = mkSCall1 "sum" e

pSize :: TExpr (Vector Float) -> TExpr Nat
pSize e = mkSCall1 "size" e

pSel :: Int -> Int -> Expr -> Expr
pSel i n x = Call (Fun (SelFun i n)) x

pFst,pSnd :: Expr -> Expr
pFst   = pSel 1 2
pSnd   = pSel 2 2

