module Prim where

import Lang

data LM a b   -- Linear maps

lmZero :: TExpr (LM a b)
lmZero = Call (LMFun "lmZero") (Tuple [])

lmOne :: TExpr (LM a a)
lmOne = Call (LMFun "lmOne") (Tuple [])

lmAdd :: TExpr (LM a b) -> TExpr (LM a b) -> TExpr (LM a b)
lmAdd f g = Call (LMFun "lmAdd") (Tuple [f,g])

lmScalar :: TExpr Float -> TExpr (LM Float Float)
lmScalar f = Call (LMFun "lmScalar") f

lmCompose :: TExpr (LM b c) -> TExpr (LM a b) -> TExpr (LM a c)
lmCompose f g = Call (LMFun "lmCompose") (Tuple [f,g])

lmPair :: [Expr] -> Expr
-- TExpr (LM a b1) -> TExpr (LM a b2) -> TExpr (LM a (b1,b2))
lmPair fs = Call (LMFun "lmPair") (Tuple fs)

lmCross :: [Expr] -> Expr
-- TExpr (LM a1 b) -> TExpr (LM a2 b) -> TExpr (LM (a1,a2) b)
lmCross fs = Call (LMFun "lmCross") (Tuple fs)

lmTranspose :: TExpr (LM a b) -> TExpr (LM b a)
lmTranspose m = Call (LMFun "lmTranspose") m

lmApply :: TExpr (LM a b) -> TExpr a -> TExpr b
lmApply lm arg = Call (LMFun "lmApply") (Tuple [lm, arg])

isLMOne, isLMZero :: Expr -> Bool
isLMOne (Call (LMFun "lmOne") _) = True
isLMOne _ = False

isLMZero (Call (LMFun "lmZero") _) = True
isLMZero _ = False

gradSelFun :: Int -> Int -> Expr
-- Result expr has type (t1, ..., tn) -o ti
gradSelFun i 1 = lmOne
gradSelFun i n = Call (GradFun (SelFun i n) Fwd) (Tuple [])

-----------------------
pAdd, pMul :: TExpr Float -> TExpr Float -> TExpr Float
pAdd a b = Call (Fun (SFun "+")) (Tuple [a,b])
pMul a b = Call (Fun (SFun "*")) (Tuple [a,b])
pDiv a b = Call (Fun (SFun "/")) (Tuple [a,b])
pNeg x   = Call (Fun (SFun "neg")) x

pSel :: Int -> Int -> Expr -> Expr
pSel i n x = Call (Fun (SelFun i n)) x

pFst,pSnd :: Expr -> Expr
pFst   = pSel 1 2
pSnd   = pSel 2 2

