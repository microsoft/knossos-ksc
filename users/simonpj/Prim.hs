module Prim where

import Lang

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

pSel :: Int -> Int -> Expr -> Expr
pSel i n x = Call (Fun (SelFun i n)) x

pFst,pSnd :: Expr -> Expr
pFst   = pSel 1 2
pSnd   = pSel 2 2

