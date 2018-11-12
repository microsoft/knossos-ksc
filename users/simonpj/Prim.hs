module Prim where

import GHC.Stack
import Debug.Trace

import Data.Array

import Lang

type Nat = Int
type Vector t = Array Nat t

data LM a b   -- Linear maps

lm :: String -> Type -> Type -> [TExpr] -> TExpr
lm f s t args = mkTCall (TypeLM s t) (LMFun f) args

lmZero :: Type -> Type -> TExpr
lmZero s t = lm "lmZero" s t []

lmOne :: Type -> TExpr
lmOne t = lm "lmOne" t t []

lmAdd :: HasCallStack => TExpr -> TExpr -> TExpr
lmAdd f g = 
  let (TypeLM s1 t1) = typeof f
      (TypeLM s2 t2) = typeof g in
  assertEqualThen "lmAdd s" s1 s2 $ 
  assertEqualThen ("lmAdd(\n" ++ (show $ ppr f) ++ ",\n\n\n\n\n " ++ (show $ ppr g) ++ ")")   t1 t2 $ 
  lm "lmAdd" s1 t1 [f,g]

lmAdds :: HasCallStack => [TExpr]-> TExpr
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmScale :: Type -> TExpr -> TExpr
lmScale t f = 
  -- FIXME assertEqualThen "lmScale" (typeof f) (TypeFloat) $
  lm "lmScale" t t [f]

lmCompose :: HasCallStack => TExpr -> TExpr -> TExpr
lmCompose f g = 
  let (TypeLM b c) = typeof f
      (TypeLM a b1) = typeof g in
  assertEqualThen ("lmCompose(" ++ show (ppr f) ++ ", " ++ show (ppr g)) b1 b $ 
  lm "lmCompose" a c [f,g]

lmVCat :: TExpr -> TExpr -> TExpr
lmVCat f g = 
  let (TypeLM a b1) = typeof f
      (TypeLM a' b2) = typeof g in
  assertEqualThen "lmVCat" a a' $
  lm "lmVCat" a (TypeTuple [b1, b2]) [f,g]

lmVCats :: [TExpr] -> TExpr
lmVCats es = lm "lmVCat" (typeofSrc $ head es) (TypeTuple $ map typeofDst es) es

lmHCat :: TExpr -> TExpr -> TExpr 
lmHCat f g =
  let (TypeLM a1 b) = typeof f
      (TypeLM a2 b') = typeof g in
  assertEqualThen "lmHCat" b b' $
  lm "lmHCat" (TypeTuple [a1, a2]) b [f,g]

lmHCats :: [TExpr] -> TExpr 
lmHCats es = lm "lmHCat" (TypeTuple $ map typeofSrc es) (typeofDst $ head es) es

lmTranspose :: TExpr -> TExpr 
lmTranspose m = lm "lmTranspose" (typeofDst m) (typeofSrc m) [m]

lmApply :: HasCallStack => TExpr -> TExpr -> TExpr
lmApply m arg = 
  let (TypeLM a b) = typeof m in
  assertEqualThen ("lmApply " ++ (show $ ppr m) ) a (typeof arg) $ 
  mkTCall b (LMFun "lmApply") [m, arg]

lmBuild :: HasCallStack => TExpr -> TExpr -> TExpr
lmBuild e f = 
  case typeof f of
  TypeLambda TypeInteger (TypeLM s t) -> lm "lmBuild" s (TypeVec t) [e, f]
  ty -> error $ "uexpected " ++ show ty ++ "\n" ++ (show $ ppr f)

lmBuildT :: TExpr -> TExpr -> TExpr 
lmBuildT e f = 
  let (TypeLambda TypeInteger (TypeLM t s)) = typeof f in
  lm "lmBuildT" (TypeVec t) s [e, f]

lmDelta :: Type -> TExpr -> TExpr -> TExpr
lmDelta t i j = If (pEqual i j) (lmOne t) (lmZero t t)

isLMOne, isLMZero :: TExpr -> Bool
isLMOne (Call (TFun ty (LMFun "lmOne")) _) = True
isLMOne _ = False

isLMZero (Call (TFun ty (LMFun "lmOne")) _) = True
isLMZero _ = False

-----------------------
-- Assertion

isEqualityCall :: TExpr -> Maybe (TExpr, TExpr)
isEqualityCall (Call (TFun TypeBool (Fun (SFun "=="))) (Tuple [e1,e2]))
  = Just (e1,e2)
isEqualityCall _ = Nothing

-----------------------
-- Delta and diag

pDelta :: TExpr -> TExpr -> TExpr -> TExpr
-- delta i j e  =  if i==j then e else zero
pDelta ei ej e = mkTCall (typeof e) (mkFun "delta") [ei, ej, e]

pDeltaVec :: TExpr -> TExpr -> TExpr -> TExpr
-- deltaVec size i e = build size (\j. delta i j e)
pDeltaVec sz ei e = mkTCall (TypeVec $ typeof e) (mkFun "deltaVec") [sz, ei, e]

pDiag :: TExpr -> TExpr -> TExpr -> TExpr
-- diag rows cols (\i. e) = build row (\i. deltaVec cols i e)
pDiag rows cols d = 
  let (TypeLambda TypeInteger a) = typeof d in
  mkTCall (TypeVec (TypeVec a)) (mkFun "diag") [rows, cols, d]

---------------------------
-- "User-defined" functions
---------------------------
pAdd, pMul, pDiv, pEqual :: TExpr -> TExpr -> TExpr
pAdd a b =   mkTCall2 (typeof a) (mkFun "+") a b
pMul a b =   mkTCall2 (typeof a) (mkFun "*") a b
pDiv a b =   mkTCall2 (typeof a) (mkFun "/") a b
pNeg x   =   mkTCall1 (typeof x) (mkFun "neg") x
pExp x   =   mkTCall1 (typeof x) (mkFun "exp") x
pLog x   =   mkTCall1 (typeof x) (mkFun "log") x
pEqual a b = mkTCall2 TypeBool (mkFun "==") a b

pBuild :: TExpr -> TExpr -> TExpr
pBuild n f =
  let (TypeLambda TypeInteger t) = typeof f in
  mkTCall2 (TypeVec t) (mkFun "build") n f 
            
pIndex :: TExpr -> TExpr -> TExpr
pIndex i e = mkTCall2 t (mkFun "index") i e where (TypeVec t) = typeof e

pSum :: TExpr -> TExpr
pSum e = mkTCall1 t (mkFun "sum") e where (TypeVec t) = typeof e

pSize :: TExpr -> TExpr
pSize e = mkTCall1 TypeInteger (mkFun "size") e

pSel :: Int -> Int -> TExpr -> TExpr
pSel i n e = Call (TFun (ts!!(i-1)) (Fun (SelFun i n))) e where (TypeTuple ts) = typeof e

pFst,pSnd :: TExpr -> TExpr
pFst   = pSel 1 2
pSnd   = pSel 2 2

