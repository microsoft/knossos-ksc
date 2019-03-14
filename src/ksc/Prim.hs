{-# OPTIONS_GHC -Wno-unused-matches #-}

module Prim where

import Lang
import GHC.Stack
import Control.Monad( zipWithM )

--------------------------------------------
--  Simple call construction
--------------------------------------------

primCall :: PrimFun -> Type -> TExpr -> TExpr
primCall fun res_ty arg
  = Call (TFun res_ty (Fun (PrimFun fun))) arg

mkPrimCall :: HasCallStack => PrimFun -> TExpr -> TExpr
mkPrimCall fun arg
  = primCall fun res_ty arg
  where
    res_ty = primFunCallResultTy fun (typeof arg)

mkPrimCall2 :: HasCallStack => String -> TExpr -> TExpr -> TExpr
mkPrimCall2 f a b = mkPrimCall f (Tuple [a, b])

mkPrimCall3 :: HasCallStack => String -> TExpr -> TExpr -> TExpr -> TExpr
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
lmAdds [] = error "lmAdds of empty list (perhaps this should return lmZero?)"
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


lmDelta :: Type -> TExpr -> TExpr -> TExpr
lmDelta t i j = If (pEqual i j) (lmOne t) (lmZero t t)

primDindex :: TExpr -> TExpr -> TExpr
primDindex i v = lmHCat [ lmZero TypeInteger t
                        , lmBuildT (pSize v) (Lam ii (lmDelta t (Var ii) i)) ]
             where ii = TVar TypeInteger $ Simple "primDindex$i"
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

pNeg, pExp, pLog :: HasCallStack => TExpr -> TExpr
pNeg x = mkPrimCall "neg" x
pExp x = mkPrimCall "exp" x
pLog x = mkPrimCall "log" x

pBuild :: TExpr -> TExpr -> TExpr
pBuild n f = mkPrimCall2 "build" n f

pIndex :: TExpr -> TExpr -> TExpr
pIndex i e = mkPrimCall2 "index" i e

pSum :: TExpr -> TExpr
pSum e = mkPrimCall "sum" e

pSumBuild :: TExpr -> TExpr -> TExpr
pSumBuild n f = mkPrimCall2 "sumbuild" n f

pSize :: TExpr -> TExpr
pSize e = case typeof e of
          TypeVec _ -> mkPrimCall "size" e
          _ -> error $ "Size of non-vector " ++ pps e

pSel :: Int -> Int -> TExpr -> TExpr
pSel i n e = Call (TFun (ts !! (i-1))
                        (Fun (SelFun i n))) e
           where
             TypeTuple ts = typeof e

pFst,pSnd :: TExpr -> TExpr
pFst   = pSel 1 2
pSnd   = pSel 2 2

ensureTuple :: TExpr -> TExpr
ensureTuple x = case typeof x of
    TypeTuple _ -> x
    _ -> Tuple [x]


---------------------------------------------
--       Types of primitive functions
--
--  For each primitve, we give its type
--  And this is the /only/ place we do this
---------------------------------------------

primCallResultTy_maybe :: HasCallStack => Fun -> Type
                       -> Either SDoc Type
primCallResultTy_maybe fun arg_ty
  = case fun of
      Fun (PrimFun f)
         | Just ty <- primFunCallResultTy_maybe f arg_ty
         -> Right ty
         | otherwise
         -> Left (text "Ill-typed call to:" <+> ppr fun)

      Fun (SelFun i _) -> selCallResultTy_maybe i arg_ty

      GradFun f dir
        -> case primCallResultTy_maybe (Fun f) arg_ty of
            Right res_ty -> case dir of
                              Fwd -> Right (TypeLM arg_ty res_ty)
                              Rev -> Right (TypeLM res_ty arg_ty)
            Left err -> Left err

      DrvFun f Fwd    -- f :: S -> T, then fwd$f :: (S,S) -> T
        | TypeTuple ss <- arg_ty
        , let n_s = length ss
        , even n_s
        , let s_ty = case ss of
                       [s1,s2] -> s2
                       _       -> TypeTuple (take (n_s `div` 2) ss)
        , Right res_ty <- primCallResultTy_maybe (Fun f) s_ty
        -> Right res_ty
        | otherwise
        -> Left (text "Ill-typed call to:" <+> ppr fun)

      DrvFun f Rev    -- f :: S -> T, then rev$f :: (S,T) -> T
        -> pprPanic "primFunCallResultTy" (ppr fun <+> ppr arg_ty)
           -- How do we split up that tuple?

      Fun (UserFun _) -> Left (text "Not in scope:" <+> ppr fun)


primFunCallResultTy :: HasCallStack => PrimFun -> Type -> Type
primFunCallResultTy fun arg_ty
  = case primFunCallResultTy_maybe fun arg_ty of
      Just res_ty -> res_ty
      Nothing -> -- pprTrace "primCallResultTy: Could not determine result type for"
                 --         (text fun <+> text " @ " <+> ppr arg_ty) $
                 TypeUnknown

primFunCallResultTy_maybe :: PrimFun -> Type -> Maybe Type
primFunCallResultTy_maybe fun
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

selCallResultTy_maybe :: Int -> Type -> Either SDoc Type
selCallResultTy_maybe i arg_ty
  = case arg_ty of
      TypeTuple tys -> Right (tys !! (i - 1))
      TypeVec t     -> Right t
      _             -> Left (text "Bad argument to selector")

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
  , assertBool (tangentType s `eqType` s1)
  = Just (tangentType t)
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

simplePrimResultTy :: HasCallStack => String -> Type -> Maybe Type
-- Addition is special: it can add any two things of the same type
simplePrimResultTy "+" (TypeTuple [t1, t2])
   = add t1 t2
  where
    add :: Type -> Type -> Maybe Type
    add TypeInteger TypeInteger   = Just TypeInteger
    add TypeFloat   TypeFloat     = Just TypeFloat
    add t  (TypeTuple [])   = Just t
    add (TypeVec t1) (TypeVec t2) = do { tr <- add t1 t2
                                       ; return (TypeVec tr) }
    add t1 (TypeZero t2) = Just t1
    add (TypeZero t1) t2 = Just t2
    add (TypeTuple t1s) (TypeTuple t2s) = do { ts <- zipWithM add t1s t2s
                                             ; return (TypeTuple ts) }
    add t1 t2 = Nothing

simplePrimResultTy fun arg_ty
  = case (fun, arg_ty) of
      ("$inline"  , t                                      ) -> Just t
      ("$trace"   , t                                      ) -> Just t
      ("$rand"    , TypeFloat                              ) -> Just TypeFloat
      ("pr"       , _                                      ) -> Just TypeInteger
      ("build"    , TypeTuple [TypeInteger, TypeLambda TypeInteger t]) -> Just (TypeVec t)
      ("sumbuild" , TypeTuple [TypeInteger, TypeLambda TypeInteger t]) -> Just t
      ("index"    , TypeTuple [TypeInteger, TypeVec t]     ) -> Just t
      ("size"     , TypeVec _                              ) -> Just TypeInteger
      ("sum"      , TypeVec t                              ) -> Just t
      ("sum"      , TypeZero (TypeVec t)                   ) -> Just (TypeZero t)
      ("to_float" , TypeInteger                            ) -> Just TypeFloat

      -- arithmetic ops.   See special case for "+" above
      ("*"        , TypeTuple [TypeFloat,   t]             ) -> Just t
      ("*"        , TypeTuple [TypeInteger, TypeInteger]   ) -> Just TypeInteger
      ("/"        , TypeTuple [TypeFloat,   TypeFloat]     ) -> Just TypeFloat
      ("/"        , TypeTuple [TypeInteger, TypeInteger]   ) -> Just TypeInteger
      ("-"        , TypeTuple [TypeFloat,   TypeFloat]     ) -> Just TypeFloat
      ("-"        , TypeTuple [TypeInteger, TypeInteger]   ) -> Just TypeInteger

      ("neg"      , t                                      ) -> Just t
      ("exp"      , TypeFloat                              ) -> Just TypeFloat
      ("log"      , TypeFloat                              ) -> Just TypeFloat
      ("lgamma"   , TypeFloat                              ) -> Just TypeFloat
      ("digamma"  , TypeFloat                              ) -> Just TypeFloat

      ("=="       , _                                      ) -> Just TypeBool
      ("!="       , _                                      ) -> Just TypeBool
      ("<"        , _                                      ) -> Just TypeBool
      (">"        , _                                      ) -> Just TypeBool
      ("abs"      , _                                      ) -> Just TypeFloat
      ("max"      , _                                      ) -> Just TypeFloat
      ("delta"    , TypeTuple [TypeInteger, TypeInteger, t]) -> Just t
      ("deltaVec" , TypeTuple [TypeInteger, TypeInteger, t]) -> Just (TypeVec t)
      ("diag"     , TypeTuple [ TypeInteger, TypeInteger
                              , TypeLambda TypeInteger t ])  -> Just (TypeVec (TypeVec t))
      _ -> Nothing

isPrimFun :: String -> Bool
isPrimFun f = f `elem` [ "inline", "$trace", "$rand", "pr", "build", "sumbuild"
                       , "index", "size", "sum", "to_float"
                       , "neg", "exp", "log", "lgamma", "digamma", "+", "-", "*", "/"
                       , "==", "!=", "<", ">", "delta", "deltaVec", "diag"
                       , "lmApply", "lmVCat", "lmHCat", "lmTranspose"
                       , "lmCompose", "lmAdd", "lmScale", "lmBuild", "lmBuildT"
                       , "abs", "max" ]

mkFun :: String -> Fun
mkFun f | isPrimFun f = Fun (PrimFun f)
        | otherwise   = Fun (UserFun f)
