module Prim where

import GHC.Stack
import Debug.Trace

import Data.Array

import Lang

dummyVar :: Type -> TExpr
dummyVar t = Var $ TVar t (Dummy)

-- Call to a LMFun, with given return type
lm :: Type -> Type -> String -> [TExpr] -> TExpr
lm (TypeTuple [s]) t name args = error $ "Tupled {" ++ name ++ "}"
lm s t name args = mkTCall (TypeLM s t) (LMFun name) args

lmZero :: Type -> Type -> TExpr
lmZero s t = lm s t "lmZero" [dummyVar s, dummyVar t]

lmOne :: Type -> TExpr
lmOne t = lm t t "lmOne" [dummyVar t]

lmScale :: HasCallStack => Type -> TExpr -> TExpr
lmScale t f =
  assertEqualThen "lmScale second arg should be TypeFloat" (typeof f) TypeFloat $
  lm t t "lmScale" [dummyVar t, f]

lmAdd :: HasCallStack => TExpr -> TExpr -> TExpr
lmAdd f g =
  let (TypeLM s1 t1) = typeof f
      (TypeLM s2 t2) = typeof g in
  assertEqualThen "lmAdd s" s1 s2 $
  assertEqualThen "lmAdd t" t1 t2 $
  lm s1 t1 "lmAdd" [f,g]

lmAdds :: HasCallStack => [TExpr]-> TExpr
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmCompose :: HasCallStack => TExpr -> TExpr -> TExpr
lmCompose f g =
  let (TypeLM t r) = typeof f
      (TypeLM s t1) = typeof g in
  assertEqualThen ("lmCompose:\n\n" ++ pps f ++ "\n\n" ++ pps g) t t1 $
  lm s r "lmCompose" [f,g]

lmVCat :: HasCallStack => [TExpr] -> TExpr
lmVCat [e] = error "unexpected"
lmVCat es =
  let s = assertAllEqualRet "lmVCat" (map typeofSrcLM tys)
      t = TypeTuple $ map typeofDstLM tys in
  lm s t "lmVCat" es
  where
    tys = map getLM $ map typeof $ es

lmHCat :: HasCallStack => [TExpr] -> TExpr
lmHCat [e] = error "unexpected"
lmHCat es =
  let t = assertAllEqualRet "lmHCat" (map typeofDstLM tys)
      s = TypeTuple $ map typeofSrcLM tys in
  lm s t "lmHCat" es
  where
    tys = map getLM $ map typeof $ es

lmTranspose :: TExpr -> TExpr
lmTranspose m = lm t s "lmTranspose" [m]
  where (TypeLM s t) = typeof m

lmApply :: HasCallStack => TExpr -> TExpr -> TExpr
lmApply m arg =
  let (TypeLM s t) = typeof m in
  --assertEqualThen ("lmApply(" ++ pps m ++ "," ++ pps arg ++ ")" ) s (typeof arg) $
  mkTCall t (LMFun "lmApply") [m, arg]

lmBuild :: HasCallStack => TExpr -> TExpr -> TExpr
lmBuild n f =
    case typeof f of
    TypeLambda TypeInteger (TypeLM s t) -> lm s (TypeVec t) "lmBuild" [n, f]
    ty -> error $ "uexpected " ++ show ty ++ "\n" ++ pps f

lmBuildT :: TExpr -> TExpr -> TExpr
lmBuildT n f =
  case typeof f of
  TypeLambda TypeInteger (TypeLM s t) -> lm (TypeVec s) t "lmBuildT" [n, f]
  ty -> error $ "uexpected " ++ show ty ++ "\n" ++ pps f

lmDelta :: Type -> TExpr -> TExpr -> TExpr
lmDelta t i j = If (pEqual i j) (lmOne t) (lmZero t t)

isLMOne, isLMZero :: TExpr -> Bool
isLMOne (Call (TFun (TypeLM _ _) (LMFun "lmOne")) _) = True
isLMOne _ = False

isLMZero (Call (TFun (TypeLM _ _) (LMFun "lmZero")) _) = True
isLMZero _ = False

primDindex :: TExpr -> TExpr -> TExpr
primDindex i v = lmHCat [ lmZero TypeInteger t
                    , lmBuildT (pSize v) (Lam ii (lmDelta t (Var ii) i)) ]
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
