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

-----------------------

splus, stimes :: TExpr Float -> TExpr Float -> TExpr Float
splus  a b = Call (Fun "+") (Tuple [a,b])
stimes a b = Call (Fun "*") (Tuple [a,b])

