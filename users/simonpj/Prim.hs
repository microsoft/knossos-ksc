module Prim where

import Data.Array

import Lang

type Nat = Int
type Vector t = Array Nat t

data LM a b   -- Linear maps

lmZero :: TExpr (LM a b)
lmZero = Call (LMFun "lmZero") (Tuple [])

lmOne :: TExpr (LM a a)
lmOne = Call (LMFun "lmOne") (Tuple [])

lmAdd :: TExpr (LM a b) -> TExpr (LM a b) -> TExpr (LM a b)
lmAdd f g = Call (LMFun "lmAdd") (Tuple [f,g])

lmAdds :: [TExpr (LM a b)]-> TExpr (LM a b)
lmAdds [] = lmZero
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmScale :: TExpr Float -> TExpr (LM Float Float)
lmScale f = Call (LMFun "lmScale") f

lmCompose :: TExpr (LM b c) -> TExpr (LM a b) -> TExpr (LM a c)
lmCompose f g = Call (LMFun "lmCompose") (Tuple [f,g])

lmVCat :: [Expr] -> Expr
-- Written as "x" in the paper
-- TExpr (LM a b1) -> TExpr (LM a b2) -> TExpr (LM a (b1,b2))
lmVCat fs = Call (LMFun "lmVCat") (Tuple fs)

lmHCat :: [Expr] -> Expr
-- Written as "bow-tie" in the paper
-- TExpr (LM a1 b) -> TExpr (LM a2 b) -> TExpr (LM (a1,a2) b)
lmHCat fs = Call (LMFun "lmHCat") (Tuple fs)

lmTranspose :: TExpr (LM a b) -> TExpr (LM b a)
lmTranspose m = Call (LMFun "lmTranspose") m

lmApply :: TExpr (LM a b) -> TExpr a -> TExpr b
lmApply lm arg = Call (LMFun "lmApply") (Tuple [lm, arg])

lmBuild :: TExpr Int -> TExpr (Int -> LM s t) -> TExpr (LM s (Vector t))
lmBuild e f = Call (LMFun "lmBuild") (Tuple [e, f])

lmBuildT :: TExpr Int -> TExpr (Int -> LM t s) -> TExpr (LM (Vector t) s)
lmBuildT e f = Call (LMFun "lmBuildT") (Tuple [e, f])

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
pAdd, pMul :: TExpr Float -> TExpr Float -> TExpr Float
pAdd a b = Call (Fun (SFun "+")) (Tuple [a,b])
pMul a b = Call (Fun (SFun "*")) (Tuple [a,b])
pDiv a b = Call (Fun (SFun "/")) (Tuple [a,b])
pNeg x   = Call (Fun (SFun "neg")) x

pIndex :: TExpr (Vector a) -> TExpr Int -> TExpr a
pIndex e i = Call (Fun (SFun "index")) (Tuple [e,i])

pSum :: TExpr (Vector Float) -> TExpr Float
pSum e = Call (Fun (SFun "sum")) e

pSize :: TExpr (Vector Float) -> TExpr Nat
pSize e = Call (Fun (SFun "size")) e

pDelta :: TExpr Nat -> TExpr Nat -> TExpr Float
pDelta i j = Call (Fun (SFun "delta")) (Tuple [i,j])

pIf :: TExpr Bool -> TExpr t -> TExpr t -> TExpr t
pIf b t e = Call (Fun (SFun "if")) (Tuple [b,t,e])

pSel :: Int -> Int -> Expr -> Expr
pSel i n x = Call (Fun (SelFun i n)) x

pFst,pSnd :: Expr -> Expr
pFst   = pSel 1 2
pSnd   = pSel 2 2

