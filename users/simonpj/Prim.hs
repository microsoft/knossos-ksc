module Prim where

import Lang

data LM a b   -- Linear maps

lmZero :: TExpr (LM a b)
lmZero = Call (LMFun "lmZero") (Tuple [])

lmOne :: TExpr (LM a a)
lmOne = Call (LMFun "lmOne") (Tuple [])

lmFloat :: TExpr Float -> TExpr (LM Float Float)
lmFloat f = Call (LMFun "lmFloat") f

lmCompose :: TExpr (LM b c) -> TExpr (LM a b) -> TExpr (LM a c)
lmCompose f g = Call (LMFun "lmCompose") (Tuple [f,g])

lmPair :: TExpr (LM a b1) -> TExpr (LM a b2)
       -> TExpr (LM a (b1,b2))
lmPair f g = Call (LMFun "lmPair") (Tuple [f,g])

lmCross :: TExpr (LM a1 b) -> TExpr (LM a2 b)
       -> TExpr (LM (a1,a2) b)
lmCross f g = Call (LMFun "lmCross") (Tuple [f,g])

lmTranspose :: TExpr (LM a b) -> TExpr (LM b a)
lmTranspose m = Call (LMFun "lmTranspose") m

lmApply :: TExpr (LM a b) -> TExpr a -> TExpr b
lmApply lm arg = Call (LMFun "lmApply") (Tuple [lm, arg])

isLMOne, isLMZero :: Expr -> Bool
isLMOne (Call (LMFun "lmOne") _) = True
isLMOne _ = False

isLMZero (Call (LMFun "lmZero") _) = True
isLMZero _ = False

-----------------------
pAdd, pMul :: TExpr Float -> TExpr Float -> TExpr Float
pAdd a b = Call (Fun "+") (Tuple [a,b])
pMul a b = Call (Fun "*") (Tuple [a,b])
pFst x   = Call (Fun "fst") x
pSnd x   = Call (Fun "snd") x

