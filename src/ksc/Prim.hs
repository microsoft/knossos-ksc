module Prim where

import Lang
import GHC.Stack
import Debug.Trace
import Data.Array
import Text.PrettyPrint

--------------------------------------------
--  Simple call construction
--------------------------------------------

primCall :: PrimFun -> Type -> TExpr -> TExpr
primCall fun res_ty arg
  = Call (TFun res_ty (Fun (PrimFun fun))) arg

mkPrimCall :: PrimFun -> TExpr -> TExpr
mkPrimCall fun arg
  = primCall fun res_ty arg
  where
    res_ty = primCallResultTy fun (typeof arg)

mkPrimCall2 :: String -> TExpr -> TExpr -> TExpr
mkPrimCall2 f a b = mkPrimCall f (Tuple [a, b])

mkPrimCall3 :: String -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall3 f a b c = mkPrimCall f (Tuple [a, b, c])

--------------------------------------------
--  Building simple calls
--------------------------------------------

lmZero :: Type -> Type -> TExpr
lmZero s t = mkPrimCall2 "lmZero" (mkDummy s) (mkDummy t)

lmOne :: Type -> TExpr
lmOne t = mkPrimCall "lmOne" (mkDummy t)

lmScale :: HasCallStack => TExpr -> TExpr
lmScale e = mkPrimCall "lmScale" e

lmAdd :: HasCallStack => TExpr -> TExpr -> TExpr
lmAdd f g = mkPrimCall2 "lmAdd" f g

lmAdds :: HasCallStack => [TExpr]-> TExpr
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmHCat :: HasCallStack => [TExpr] -> TExpr
lmHCat es = mkPrimCall "lmHCat" (Tuple es)

lmVCat :: HasCallStack => [TExpr] -> TExpr
lmVCat es = mkPrimCall "lmVCat" (Tuple es)

lmTranspose :: TExpr -> TExpr
lmTranspose m = mkPrimCall "lmTranspose" m

lmCompose :: TExpr -> TExpr -> TExpr
lmCompose f g = mkPrimCall2 "lmCompose" f g

lmApply :: HasCallStack => TExpr -> TExpr -> TExpr
lmApply f x = mkPrimCall2 "lmApply" f x

lmBuild :: HasCallStack => TExpr -> TExpr -> TExpr
lmBuild n f = mkPrimCall2 "lmBuild" n f

lmBuildT :: HasCallStack => TExpr -> TExpr -> TExpr
lmBuildT n f = mkPrimCall2 "lmBuildT" n f

isThePrimFun :: TFun -> String -> Bool
isThePrimFun (TFun _ (Fun (PrimFun f1))) f2 = f1 == f2
isThePrimFun _ _ = False

isLMOne, isLMZero :: TExpr -> Bool
isLMOne (Call f e) = f `isThePrimFun` "lmOne"
isLMOne _ = False

isLMZero (Call f e) = f `isThePrimFun` "lmZero"
isLMZero _ = False


--TODO: make this work with lmone lmzero again
lmDelta :: Type -> TExpr -> TExpr -> TExpr
lmDelta t i j = If (pEqual i j) (lmScale $ kTFloat 1.0) (lmScale $ kTFloat 0.0)
 
primDindex :: TExpr -> TExpr -> TExpr
primDindex i v = lmHCat [ lmZero TypeInteger t
                        , lmBuildT (pSize v) (Lam ii (lmDelta t (Var ii) i)) ]
             where ii = TVar TypeInteger $ Simple "ii"
                   TypeVec t = typeof v

isEqualityCall :: TExpr -> Maybe (TExpr, TExpr)
isEqualityCall (Call f (Tuple [e1,e2]))
  | f `isThePrimFun` "==" = Just (e1,e2)
isEqualityCall _          = Nothing

-----------------------
-- Delta and diag

pDelta :: TExpr -> TExpr -> TExpr -> TExpr
-- delta i j e  =  if i==j then e else zero
pDelta ei ej e = mkPrimCall3 "delta" ei ej e

pDeltaVec :: TExpr -> TExpr -> TExpr -> TExpr
-- deltaVec size i e = build size (\j. delta i j e)
pDeltaVec sz ei e = mkPrimCall3 "deltaVec" sz ei e

pDiag :: TExpr -> TExpr -> TExpr -> TExpr
-- diag rows cols (\i. e) = build row (\i. deltaVec cols i e)
pDiag rows cols d = mkPrimCall3 "diag" rows cols d

---------------------------
-- "User-defined" functions
---------------------------
pAdd, pMul, pDiv, pEqual :: HasCallStack => TExpr -> TExpr -> TExpr
pAdd a b   = mkPrimCall2 "+" a b
pMul a b   = mkPrimCall2 "*" a b
pDiv a b   = mkPrimCall2 "/" a b
pEqual a b = mkPrimCall2 "==" a b
pNeg x     = mkPrimCall "neg" x
pExp x     = mkPrimCall "exp" x
pLog x     = mkPrimCall "log" x

pBuild :: TExpr -> TExpr -> TExpr
pBuild n f = mkPrimCall2 "build" n f

pIndex :: TExpr -> TExpr -> TExpr
pIndex i e = mkPrimCall2 "index" i e

pSum :: TExpr -> TExpr
pSum e = mkPrimCall "sum" e

pSize :: TExpr -> TExpr
pSize e = mkPrimCall "size" e

pSel :: Int -> Int -> TExpr -> TExpr
pSel i n e = Call (TFun (ts !! (i-1))
                        (Fun (SelFun i n))) e
           where
             TypeTuple ts = typeof e

pFst,pSnd :: TExpr -> TExpr
pFst   = pSel 1 2
pSnd   = pSel 2 2


---------------------------------------------
--       Types of primitive functions
--
--  For each primitve, we give its type
--  And this is the /only/ place we do this
---------------------------------------------

primCallResultTy :: HasCallStack => PrimFun -> Type -> Type
primCallResultTy fun arg_ty
  = case primCallResultTy_maybe fun arg_ty of
      Just res_ty -> res_ty
      Nothing -> pprTrace "primCallResultTy: Could not determine result type for"
                          (text fun <+> text " @ " <+> ppr arg_ty) $
                 TypeUnknown

primCallResultTy_maybe :: PrimFun -> Type -> Maybe Type
primCallResultTy_maybe fun
  = case fun of
      "lmZero"      -> lmZeroResultTy
      "lmOne"       -> lmOneResultTy
      "lmApply"     -> lmApplyResultTy
      "lmVCat"      -> lmVCatResultTy
      "lmHCat"      -> lmHCatResultTy
      "lmTranspose" -> lmTransposeResultTy
      "lmCompose"   -> lmComposeResultTy
      "lmAdd"       -> lmAddResultTy
      "lmScale"     -> lmScaleResultTy
      "lmBuild"     -> lmBuildResultTy
      "lmBuildT"    -> lmBuildTResultTy
      _             -> simplePrimResultTy fun

selCallResultTy_maybe :: Int -> Type -> Maybe Type
selCallResultTy_maybe i arg_ty
  = case arg_ty of
      TypeTuple tys -> Just (tys !! (i - 1))
      TypeVec t     -> Just t
      _             -> Nothing

lmApplyResultTy, lmTransposeResultTy, lmScaleResultTy,
  lmHCatResultTy, lmVCatResultTy, lmBuildResultTy,
  lmBuildTResultTy, lmComposeResultTy, lmAddResultTy,
  lmZeroResultTy, lmOneResultTy
  :: Type -> Maybe Type

lmZeroResultTy ty
  | TypeTuple [s, t] <- ty
  = Just (TypeLM s t)
  | otherwise = Nothing

lmOneResultTy ty
  = Just (TypeLM ty ty)

lmApplyResultTy ty
  | TypeTuple [TypeLM s t, s1] <- ty
  , assertBool (s == s1)
  = Just t
  | otherwise = Nothing

lmTransposeResultTy ty
  | TypeLM s t <- ty
  = Just (TypeLM t s)
  | otherwise = Nothing

lmBuildResultTy ty
  | TypeTuple [TypeInteger, TypeLambda TypeInteger (TypeLM s t)] <- ty
  = Just (TypeLM s (TypeVec t))
  | otherwise = Nothing

lmBuildTResultTy ty
  | TypeTuple [TypeInteger, TypeLambda TypeInteger (TypeLM s t)] <- ty
  = Just (TypeLM (TypeVec s) t)
  | otherwise = Nothing

lmComposeResultTy ty
  | TypeTuple [TypeLM b1 c, TypeLM a b2] <- ty
  , assertBool (b1 == b2)
  = Just (TypeLM a c)
  | otherwise = Nothing

lmAddResultTy ty
  | TypeTuple [TypeLM s1 t1, TypeLM s2 t2] <- ty
  , assertBool (s1 == s2)
  , assertBool (t1 == t2)
  = Just (TypeLM s1 t1)
  | otherwise = Nothing

lmScaleResultTy ty
  | TypeFloat <- ty
  = Just (TypeLM TypeFloat TypeFloat)
  | otherwise
  = Nothing

lmVCatResultTy ty
  | TypeTuple tys <- ty
  , Just (ss, ts) <- unzipLMTypes tys
  , (s1:ss1) <- ss
  , assertBool $ all (== s1) ss1
  = Just (TypeLM s1 (TypeTuple ts))
  | otherwise = Nothing

lmHCatResultTy ty
  | TypeTuple tys <- ty
  , Just (ss, ts) <- unzipLMTypes tys
  , (t1:ts1) <- ts
  -- TODO: cope with mixtures of T and Zero T, assertBool $ all (== t1) ts1
  = Just (TypeLM (TypeTuple ss) t1)
  | otherwise = Nothing

simplePrimResultTy :: String -> Type -> Maybe Type
simplePrimResultTy fun arg_ty
  = case (fun, arg_ty) of
      ("inline"   , t                                      ) -> Just t
      ("$trace"   , t                                      ) -> Just t
      ("$rand"    , TypeFloat                              ) -> Just TypeFloat
      ("pr"       , _                                      ) -> Just TypeInteger
      ("build"    , TypeTuple [_, TypeLambda TypeInteger t]) -> Just (TypeVec t)
      ("index"    , TypeTuple [_, TypeVec t]               ) -> Just t
      ("size"     , TypeVec _                              ) -> Just TypeInteger
      ("sum"      , TypeVec t                              ) -> Just t
      ("to_float" , TypeInteger                            ) -> Just TypeFloat

      -- Addition is special: it can add any two things of the same type
      ("+"        , TypeTuple [t1, t2]                     )
                  | t1 == t2                                 -> Just t2

      -- Multiplication is special: it takes a Float on the left,
      -- but anything on the right
      ("*"        , TypeTuple [TypeFloat, t2]              ) -> Just t2
      ("*"        , TypeTuple [TypeInteger, t2]            ) -> Just t2

      ("neg"      , t                                      ) -> Just t
      ("exp"      , TypeFloat                              ) -> Just TypeFloat
      ("log"      , TypeFloat                              ) -> Just TypeFloat
      ("-"        , TypeTuple [t1, t2]                     ) -> Just t1
      ("/"        , TypeTuple [TypeFloat, TypeFloat]       ) -> Just TypeFloat  -- Not known to work for non-float types

      ("=="       , _                                      ) -> Just TypeBool
      ("!="       , _                                      ) -> Just TypeBool
      ("<"        , _                                      ) -> Just TypeBool
      (">"        , _                                      ) -> Just TypeBool
      ("delta"    , TypeTuple [TypeInteger, TypeInteger, t]) -> Just t
      ("deltaVec" , TypeTuple [TypeInteger, TypeInteger, t]) -> Just (TypeVec t)
      ("diag"     , TypeTuple [ TypeInteger, TypeInteger
                              , TypeLambda TypeInteger t ])  -> Just (TypeVec (TypeVec t))
      _ -> Nothing

isPrimFun :: String -> Bool
isPrimFun f = f `elem` [ "inline", "$trace", "$rand", "pr", "build", "index", "size", "sum", "to_float"
                       , "neg", "exp", "log", "+", "-", "*", "/"
                       , "==", "!=", "<", ">", "delta", "deltaVec", "diag"
                       , "lmApply", "lmVCat", "lmHCat", "lmTranspose"
                       , "lmCompose", "lmAdd", "lmScale", "lmBuild", "lmBuildT" ]

mkFun :: String -> Fun
mkFun f | isPrimFun f = Fun (PrimFun f)
        | otherwise   = Fun (UserFun f)
