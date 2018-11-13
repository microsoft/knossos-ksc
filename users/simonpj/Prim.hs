module Prim where

import GHC.Stack
import Debug.Trace

import Data.Array

import Lang

dummyVar :: Type -> TExpr
dummyVar t = Var $ TVar t (Dummy)

-- Call to a LMFun, with given return type 
lmf :: TypeLM -> String -> [TExpr] -> TExpr
lmf t name args = mkTCall (TypeLM t) (LMFun $ name) args

-- Call to a LMFun, name derived from the return type
lm :: TypeLM -> [TExpr] -> TExpr
lm t args = lmf t (nameOf t) args

lmZero :: Type -> Type -> TExpr
lmZero s t = lm (LMZero s t) [dummyVar s, dummyVar t]

lmOne :: Type -> TExpr
lmOne t = lm (LMOne t) [dummyVar t]

lmAdd :: HasCallStack => TExpr -> TExpr -> TExpr
lmAdd f g = 
  let (TypeLM tyf) = typeof f
      (TypeLM tyg) = typeof g in
  assertEqualThen "lmAdd s" (typeofSrc tyf) (typeofSrc tyg) $ 
  assertEqualThen "lmAdd t" (typeofDst tyf) (typeofDst tyg) $ 
  lm (LMAdd tyf tyg) [f,g]

lmAdds :: HasCallStack => [TExpr]-> TExpr
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmScale :: HasCallStack => Type -> TExpr -> TExpr
lmScale t f = 
  --assertEqualThen "lmScale second arg should be TypeFloat" (typeof f) TypeFloat $
  lm (LMScale t) [dummyVar t, f]

lmCompose :: HasCallStack => TExpr -> TExpr -> TExpr
lmCompose f g = 
  let (TypeLM tyf) = typeof f
      (TypeLM tyg) = typeof g in
  assertEqualThen ("lmCompose(" ++ show (ppr f) ++ ", " ++ show (ppr g)) (typeofDst tyg) (typeofSrc tyf) $ 
  lm (LMCompose tyf tyg) [f,g]

lmVCat :: HasCallStack => [TExpr] -> TExpr
lmVCat es =
  assertAllEqualThen  "lmVCat" (map typeofSrc tys) $ 
  lm (LMVCat tys) es
  where 
    tys = map getLM $ map typeof $ es

lmHCat :: HasCallStack => [TExpr] -> TExpr 
lmHCat es = 
  assertAllEqualThen "lmHCat" (map typeofDst tys) $ 
  lm (LMHCat tys) es
  where 
    tys = map getLM $ map typeof $ es

lmTranspose :: TExpr -> TExpr 
lmTranspose m = lm (LMTranspose ty) [m]
  where (TypeLM ty) = typeof m

lmApply :: HasCallStack => TExpr -> TExpr -> TExpr
lmApply m arg = 
  let (TypeLM tym) = typeof m in
  assertEqualThen ("lmApply " ++ (show $ ppr m) ) (typeofSrc tym) (typeof arg) $ 
  mkTCall (typeofDst tym) (LMFun "lmApply") [m, arg]

lmBuild :: HasCallStack => TExpr -> TExpr -> TExpr
lmBuild n f = 
    case typeof f of
    TypeLambda TypeInteger (TypeLM tyf) -> lm (LMBuild tyf) [n, f]
    ty -> error $ "uexpected " ++ show ty ++ "\n" ++ (show $ ppr f)

lmBuildT :: TExpr -> TExpr -> TExpr 
lmBuildT n f = 
  case typeof f of
  TypeLambda TypeInteger (TypeLM tyf) -> lm (LMBuildT tyf) [n, f]
  ty -> error $ "uexpected " ++ show ty ++ "\n" ++ (show $ ppr f)

lmDelta :: Type -> TExpr -> TExpr -> TExpr
lmDelta t i j = If (pEqual i j) (lmScale t $ kTFloat 1.0) (lmScale t $ kTFloat 0.0)

isLMOne, isLMZero :: TExpr -> Bool
isLMOne (Call (TFun (TypeLM (LMOne _)) _) _) = True
isLMOne _ = False

isLMZero (Call (TFun (TypeLM (LMZero _ _)) _) _) = True
isLMZero _ = False

primDindex :: TExpr -> TExpr -> TExpr
primDindex i v = lmHCat [ lmZero TypeInteger t
                    , lmBuildT (pSize v) (Lam ii TypeInteger (lmDelta t (Var ii) i)) ]
            where ii = TVar TypeInteger $ Simple "ii"
                  (TypeVec t) = typeof v


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

