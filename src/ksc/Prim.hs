{-# OPTIONS_GHC -Wno-unused-matches #-}

module Prim where

import Lang
import GHC.Stack
import Control.Monad( zipWithM )

--------------------------------------------
--  Simple call construction
--------------------------------------------

primCall :: PrimFun -> Type -> [TExpr] -> TExpr
primCall fun res_ty arg
  = Call (TFun res_ty (Fun (PrimFun fun))) arg

mkPrimCall :: HasCallStack => PrimFun -> [TExpr] -> TExpr
mkPrimCall fun arg
  = primCall fun res_ty arg
  where
    res_ty = primFunCallResultTy fun (map typeof arg)

mkPrimCall1 :: HasCallStack => String -> TExpr -> TExpr
mkPrimCall1 f a = mkPrimCall f [a]

mkPrimCall2 :: HasCallStack => String -> TExpr -> TExpr -> TExpr
mkPrimCall2 f a b = mkPrimCall f [a, b]

mkPrimCall3 :: HasCallStack => String -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall3 f a b c = mkPrimCall f [a, b, c]

--------------------------------------------
--  Building simple calls
--------------------------------------------

-- lmZero might look as if it should take a Type and cons up a dummy of that type,
-- but types such as Vec need constructor arguments such as size, 
-- so are consed from a template 
lmZero :: TExpr -> TExpr -> TExpr
lmZero s t = mkPrimCall2 "lmZero" s t

lmOne :: Type -> TExpr
lmOne t = mkPrimCall1 "lmOne" (mkDummy t)

lmScale :: HasCallStack => Type -> TExpr -> TExpr
lmScale t e = mkPrimCall2 "lmScale" (mkDummy t) e

lmAdd :: HasCallStack => TExpr -> TExpr -> TExpr
lmAdd f g = mkPrimCall2 "lmAdd" f g

lmAdds :: HasCallStack => [TExpr]-> TExpr
lmAdds [] = error "lmAdds of empty list (perhaps this should return lmZero?)"
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmHCat :: HasCallStack => [TExpr] -> TExpr
lmHCat [e] = e
lmHCat es  = mkPrimCall "lmHCat" es

lmVCat :: HasCallStack => [TExpr] -> TExpr
lmVCat [e] = e
lmVCat es  = mkPrimCall "lmVCat" es

lmTranspose :: TExpr -> TExpr
lmTranspose m = mkPrimCall1 "lmTranspose" m

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

fstArg, sndArg :: TExpr -> TExpr
fstArg (Call _ [e,_]) = e
fstArg e = error $ "fstArg on non-duple" ++ pps e

sndArg (Call _ [_,e]) = e
sndArg e = error $ "sndArg on non-duple" ++ pps e


lmDelta :: TExpr -> TExpr -> TExpr -> TExpr
lmDelta t i j = If (pEqual i j) (lmOne $ typeof t) (lmZero t t)

primDindex :: TExpr -> TExpr -> TExpr
primDindex i v = lmHCat [ lmZero i vi
                        , lmBuildT (pSize v) (Lam ii (lmDelta vi (Var ii) i)) ]
             where ii = TVar TypeInteger $ Simple "primDindex$i"
                   vi = pIndex i v

isEqualityCall :: TExpr -> Maybe (TExpr, TExpr)
isEqualityCall (Call f [e1,e2])
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
pAdd, pMul, pDiv, pSub, pEqual :: HasCallStack => TExpr -> TExpr -> TExpr
pAdd a b   = mkPrimCall2 "+" a b
pMul a b   = mkPrimCall2 "*" a b
pDiv a b   = mkPrimCall2 "/" a b
pSub a b   = mkPrimCall2 "-" a b
pEqual a b = mkPrimCall2 "==" a b

pNeg, pExp, pLog, pZero, pTangentZero :: HasCallStack => TExpr -> TExpr
pNeg x = mkPrimCall1 "neg" x
pExp x = mkPrimCall1 "exp" x
pLog x = mkPrimCall1 "log" x
pZero x = mkPrimCall1 "zero" x
pTangentZero x = mkPrimCall1 "tangent_zero" x

pBuild :: TExpr -> TExpr -> TExpr
pBuild n f = mkPrimCall2 "build" n f

pIndex :: TExpr -> TExpr -> TExpr
pIndex i e = mkPrimCall2 "index" i e

pSum :: TExpr -> TExpr
pSum e = mkPrimCall1 "sum" e

pSumBuild :: TExpr -> TExpr -> TExpr
pSumBuild n f = mkPrimCall2 "sumbuild" n f

pSize :: TExpr -> TExpr
pSize e = case typeof e of
          TypeVec _ -> mkPrimCall1 "size" e
          _ -> error $ "Size of non-vector " ++ pps e

pSel :: Int -> Int -> TExpr -> TExpr
pSel i n e = Call (TFun (ts !! (i-1))
                        (Fun (SelFun i n))) [e]
           where
             TypeTuple ts = typeof e

pFst,pSnd :: TExpr -> TExpr
pFst   = pSel 1 2
pSnd   = pSel 2 2

pDot :: TExpr -> TExpr -> TExpr
pDot = mkPrimCall2 "dot"

pNorm :: TExpr -> TExpr
pNorm = mkPrimCall1 "norm"

pTangentAdd :: TExpr -> TExpr -> TExpr
pTangentAdd = mkPrimCall2 "tangent_add"

pToTangent :: TExpr -> TExpr
pToTangent = mkPrimCall1 "to_tangent"

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

primCallResultTy_maybe :: HasCallStack => Fun -> [Type]
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
                              Fwd -> Right (TypeLM (mkTupleTy arg_ty) res_ty)
                              Rev -> Right (TypeLM res_ty (mkTupleTy arg_ty))
            Left err -> Left err

      DrvFun f Fwd    -- f :: S1 S2 -> T, then fwd$f :: S1 S2 S1_t S2_t -> T_t
        | let n_s = length arg_ty
        , even n_s
        , let s_tys = take (n_s `div` 2) arg_ty
        , Right t_ty <- primCallResultTy_maybe (Fun f) s_tys
        -> Right (tangentType t_ty)
        | otherwise
        -> Left (text "Ill-typed call to:" <+> ppr fun)

      DrvFun f Rev    -- f :: S1 S2 -> T, then rev$f :: S1 S2 T_t -> (S1_t, S2_t)
        | let s_tys = dropLast arg_ty
        -> Right (tangentType (mkTupleTy s_tys))

      CheckFun f -> return TypeFloat

      Fun (UserFun _) -> Left (text "Not in scope:" <+> ppr fun)


primFunCallResultTy :: HasCallStack => PrimFun -> [Type] -> Type
primFunCallResultTy fun arg_ty
  = case primFunCallResultTy_maybe fun arg_ty of
      Just res_ty -> res_ty
      Nothing -> pprTrace "primCallResultTy: Could not determine result type for"
                          (text fun <+> text " @ " <+> ppr arg_ty) $
                 TypeUnknown

primFunCallResultTy_maybe :: PrimFun -> [Type] -> Maybe Type
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

selCallResultTy_maybe :: Int -> [Type] -> Either SDoc Type
selCallResultTy_maybe i [TypeTuple arg_tys]
  | i <= length arg_tys
  = Right (arg_tys !! (i - 1))
selCallResultTy_maybe _ _ = Left (text "Bad argument to selector")

lmApplyResultTy, lmTransposeResultTy, lmScaleResultTy,
  lmHCatResultTy, lmVCatResultTy, lmBuildResultTy,
  lmBuildTResultTy, lmComposeResultTy, lmAddResultTy,
  lmZeroResultTy, lmOneResultTy
  :: [Type] -> Maybe Type

lmZeroResultTy [s,t] = Just (TypeLM s t)
lmZeroResultTy _     = Nothing

lmOneResultTy [ty] = Just (TypeLM ty ty)
lmOneResultTy _    = Nothing

lmApplyResultTy [TypeLM s t, s1]
  | assertBool (tangentType s `eqType` s1)
  = Just (tangentType t)
lmApplyResultTy _ = Nothing

lmTransposeResultTy [TypeLM s t] = Just (TypeLM t s)
lmTransposeResultTy _ = Nothing

lmBuildResultTy [TypeInteger, TypeLambda TypeInteger (TypeLM s t)]
  = Just (TypeLM s (TypeVec t))
lmBuildResultTy _ = Nothing

lmBuildTResultTy [TypeInteger, TypeLambda TypeInteger (TypeLM s t)]
  = Just (TypeLM (TypeVec s) t)
lmBuildTResultTy _ = Nothing

lmComposeResultTy [TypeLM b1 c, TypeLM a b2]
  | assertBool (b1 == b2)
  = Just (TypeLM a c)
lmComposeResultTy _ = Nothing

lmAddResultTy [TypeLM s1 t1, TypeLM s2 t2]
  | assertBool (s1 == s2)
  , assertBool (t1 == t2)
  = Just (TypeLM s1 t1)
lmAddResultTy _ = Nothing

lmScaleResultTy [t, TypeFloat]
  = Just (TypeLM t t)
lmScaleResultTy _ = Nothing

lmVCatResultTy tys
  | Just (ss, ts) <- unzipLMTypes tys
  , (s1:ss1) <- ss
  , assertBool $ all (== s1) ss1
  = Just (TypeLM s1 (TypeTuple ts))
  | otherwise = Nothing

lmHCatResultTy tys
  | Just (ss, ts) <- unzipLMTypes tys
  , (t1:ts1) <- ts
  -- TODO: cope with mixtures of T and Zero T, assertBool $ all (== t1) ts1
  = Just (TypeLM (TypeTuple ss) t1)
  | otherwise = Nothing

simplePrimResultTy :: HasCallStack => String -> [Type] -> Maybe Type
-- Addition is special: it can add any two things of the same type
simplePrimResultTy "+" [t1, t2]
   = add t1 t2
  where
    add :: Type -> Type -> Maybe Type
    add TypeInteger TypeInteger   = Just TypeInteger
    add TypeFloat   TypeFloat     = Just TypeFloat
    add t  (TypeTuple [])   = Just t
    add (TypeVec t1) (TypeVec t2) = do { tr <- add t1 t2
                                       ; return (TypeVec tr) }
    add (TypeTuple t1s) (TypeTuple t2s) = do { ts <- zipWithM add t1s t2s
                                             ; return (TypeTuple ts) }
    add t1 t2 = Nothing

simplePrimResultTy fun arg_tys
  = case (fun, arg_tys) of
      ("$inline"  , [t]                                    ) -> Just t
      ("$trace"   , [t]                                    ) -> Just t
      ("$rand"    , [TypeFloat]                            ) -> Just TypeFloat
      ("pr"       , _                                      ) -> Just TypeInteger
      ("build"    , [TypeInteger, TypeLambda TypeInteger t]) -> Just (TypeVec t)
      ("sumbuild" , [TypeInteger, TypeLambda TypeInteger t]) -> Just t
      ("index"    , [TypeInteger, TypeVec t]               ) -> Just t
      ("size"     , [TypeVec _]                            ) -> Just TypeInteger
      ("sum"      , [TypeVec t]                            ) -> Just t
      ("to_float" , [TypeInteger]                          ) -> Just TypeFloat
      ("dot"      , [t, t']                                ) | t == t' -> Just TypeFloat
      ("norm"     , [t]                                    ) -> Just TypeFloat
      ("tangent_add", [t, t']                              ) | tangentType t == t' -> Just t
      ("to_tangent",  [t]                                  ) -> Just (tangentType t)

      -- arithmetic ops.   See special case for "+" above
      ("*"        , [TypeFloat,   t]             ) -> Just t
      ("*"        , [TypeInteger, TypeInteger]   ) -> Just TypeInteger
      ("/"        , [TypeFloat,   TypeFloat]     ) -> Just TypeFloat
      ("/"        , [TypeInteger, TypeInteger]   ) -> Just TypeInteger
      ("-"        , [TypeFloat,   TypeFloat]     ) -> Just TypeFloat
      ("-"        , [TypeInteger, TypeInteger]   ) -> Just TypeInteger

      ("zero"     , [t]                                    ) -> Just t
      ("tangent_zero", [t]                                 ) -> Just (tangentType t)
      ("neg"      , [t]                                    ) -> Just t
      ("exp"      , [TypeFloat]                            ) -> Just TypeFloat
      ("log"      , [TypeFloat]                            ) -> Just TypeFloat
      ("lgamma"   , [TypeFloat]                            ) -> Just TypeFloat
      ("digamma"  , [TypeFloat]                            ) -> Just TypeFloat

      ("=="       , _                                      ) -> Just TypeBool
      ("!="       , _                                      ) -> Just TypeBool
      ("<"        , _                                      ) -> Just TypeBool
      (">"        , _                                      ) -> Just TypeBool
      ("abs"      , _                                      ) -> Just TypeFloat
      ("max"      , _                                      ) -> Just TypeFloat
      ("delta"    , [TypeInteger, TypeInteger, t]) -> Just t
      ("deltaVec" , [TypeInteger, TypeInteger, t]) -> Just (TypeVec t)
      ("diag"     , [ TypeInteger, TypeInteger
                    , TypeLambda TypeInteger t ])  -> Just (TypeVec (TypeVec t))
      _ -> Nothing

isPrimFun :: String -> Bool
isPrimFun f = f `elem` [ "inline", "$trace", "$rand", "pr", "build", "sumbuild"
                       , "index", "size", "sum", "to_float"
                       , "neg", "exp", "log", "lgamma", "digamma", "+", "-", "*", "/"
                       , "==", "!=", "<", ">", "delta", "deltaVec", "diag"
                       , "lmApply", "lmVCat", "lmHCat", "lmTranspose"
                       , "lmCompose", "lmAdd", "lmScale", "lmBuild", "lmBuildT"
                       , "abs", "max"
                       -- The dot-product, also known as inner-product
                       -- of vectors (not just TypeVecs)
                       , "dot"
                       -- The Euclidean (L2) norm of a vector (not
                       -- just a TypeVec), i.e. norm(x) = sqrt(dot(x, x))
                       , "norm"
                       -- If x :: s then dx :: tangentType t.
                       -- tangent_add allows us to add them to get
                       -- something of type s.
                       , "tangent_add"
                       -- Maps x :: s its tangent space (tangentType s)
                       , "to_tangent"
                       ]
