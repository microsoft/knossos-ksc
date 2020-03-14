-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE LambdaCase #-}

module Prim where

import Lang
import LangUtils (isTrivial)
import GHC.Stack
import Data.Maybe

--------------------------------------------
--  Simple call construction
--------------------------------------------

primCall :: PrimFun -> Type -> TExpr -> TExpr
primCall fun res_ty
  = Call (TFun res_ty (Fun (PrimFun fun)))

userCall :: String -> Type -> TExpr -> TExpr
userCall fun res_ty
  = Call (TFun res_ty (Fun (UserFun fun)))

mkPrimCall :: HasCallStack => PrimFun -> TExpr -> TExpr
mkPrimCall fun args
  = primCall fun res_ty args
  where
    res_ty = primFunCallResultTy fun args

mkPrimCall1 :: HasCallStack => String -> TExpr -> TExpr
mkPrimCall1 f a = mkPrimCall f a

mkPrimCall2 :: HasCallStack => String -> TExpr -> TExpr -> TExpr
mkPrimCall2 f a b = mkPrimCall f (Tuple [a, b])

mkPrimCall3 :: HasCallStack => String -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall3 f a b c = mkPrimCall f (Tuple [a, b, c])

mkPrimCall4 :: HasCallStack => String -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall4 f a b c d = mkPrimCall f (Tuple [a, b, c, d])

mkPrimCall5 :: HasCallStack => String -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall5 f a b c d e = mkPrimCall f (Tuple [a, b, c, d, e])

mkPrimCall6 :: HasCallStack => String -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall6 f a b c d e g = mkPrimCall f (Tuple [a, b, c, d, e, g])

mkPrimCall7 :: HasCallStack => String -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall7 f a b c d e g h = mkPrimCall f (Tuple [a, b, c, d, e, g, h])

--------------------------------------------
--  Parsing function names
--------------------------------------------

mk_fun :: String -> Fun
-- Parses the print-name of a top-level function into a Fun
-- In particular,
--
--   * Recognises D$f as (Grad f Fwd) etc
--     Keep this in sync with pprFun
--
--   * Distinguishes PrimFun from UserFun
mk_fun f = case find_dollar f of
             Just ("D",   s)  -> GradFun (mk_fun_id s) BasicAD
             Just ("Dt",   s) -> GradFun (mk_fun_id s) TupleAD
             Just ("fwd", s)  -> DrvFun  (mk_fun_id s) (AD BasicAD Fwd)
             Just ("fwdt", s) -> DrvFun  (mk_fun_id s) (AD TupleAD Fwd)
             Just ("rev", s)  -> DrvFun  (mk_fun_id s) (AD BasicAD Rev)
             Just ("revt", s) -> DrvFun  (mk_fun_id s) (AD TupleAD Rev)
             Just ("get", s) -> Fun     (mk_sel_fun s)
             _               -> Fun     (mk_fun_id f)
  where
    mk_fun_id f | isPrimFun f = PrimFun f
                | otherwise   = UserFun f
    find_dollar f = case break (== '$') f of
                       (_, [])  -> Nothing  -- No $
                       (_, [_]) -> Nothing  -- Trailing $
                       (prefix, _ : suffix) -> Just (prefix, suffix)
    mk_sel_fun s = case break (== '$') s of
                     (i,_:n) -> SelFun (read i :: Int) (read n :: Int)
                     _ -> error $ "'get' should have form 'get$i$n', not [get$" ++ s ++ "]"


---------------------------
-- Zeros
---------------------------
mkZero, mkTangentZero :: HasCallStack => TExpr -> TExpr
mkZero        = getZero id
mkTangentZero = getZero tangentType

getZero :: HasCallStack => (Type -> Type) -> TExpr -> TExpr
getZero tangent_type e
  = go e
  where
    go e = case tangent_type e_ty of
            TypeInteger  -> Konst (KInteger 0)
            TypeFloat    -> Konst (KFloat 0.0)
            TypeString   -> Konst (KString "")
            TypeBool     -> Konst (KBool False)
            TypeVec _    -> mkAtomicNoFVs e $ \ e ->
                            pConstVec (pSize e) (go (pIndex (kInt 1) e))
            TypeTuple ts
               | Tuple es <- e
               -> assert (text "splitTuple") (length ts == length es) $
                  Tuple (map go  es)
               | let n = length ts
               -> mkAtomicNoFVs e $ \e ->
                  Tuple $ map go $
                  [ pSel i n e | i <- [1..n] ]
            _ -> pprPanic "mkZero" (ppr e_ty $$ ppr e)
         where
           e_ty = typeof e

-- (mkAtomicNoFVs e body) returns the expression (let a = e in body a)
-- where body :: TExpr -> TExpr is a function expecting an expression
-- The idea is that body might use its argument many types, and we
-- don't want to duplicate e, so we let-bind it instead
--
-- NB1: there's a short-cut when e is trivial (e.g. another variable)
--
-- NB2: we use the same variable name every time.  That's not safe in
--      general, but the k's we use never mention any other variables,
--      so it's fine c.f. the more general Opt.makeAtomic, which does
--      not have this side condition but which requires an in-scope
--      set
mkAtomicNoFVs :: TExpr -> (TExpr -> TExpr) -> TExpr
mkAtomicNoFVs e body
  | isTrivial e = body e
  | otherwise   = Let ev e (body (Var ev))
  where
    ev = TVar (typeof e) argVar

--------------------------------------------
--  Building simple calls
--------------------------------------------

{-  Note [Shapes]
~~~~~~~~~~~~~~~~~
When doing AD we have the 'S' parameter, which we need to build a zero
linear map.  We make it a named type, Shape, because it could be
implemented in various ways.

- Shape = Type     This worked when we had vector sizes in types

- Shape = Expr     The value tuple of function argumetnts

- Shape = Expr     A new value abstracting the shape of the function
                   arguments (e.g. an Int for a vector, describing its
                   size)

Note [lmZero]
~~~~~~~~~~~~~
lmZero should have type  lmZero :: forall s t. s -o t
When applied, via `lmApply` or `lmApplyR`, it should produce
a zero value of type T(t) or T(s) respectively.  Alas, that
is tricky to do, given only the types.  How can we produce a
zero of type (Vec Float) for example?  How big should it be?

Our solution is to pass a /value/ of type s, and one of type t,
to lmZero, and provide mkTangentZero to turn that value into a
zero value.  Painful, but possible.
-}

type Shape = TExpr

lmZero :: Shape -> Shape -> TExpr
lmZero s t = mkPrimCall1 "lmZero" (Tuple [s, t])

lmZero_Dir :: ADDir -> TExpr -> TExpr -> TExpr
lmZero_Dir Fwd s t = lmZero s t
lmZero_Dir Rev s t = lmZero t s

lmOne :: Type -> TExpr
lmOne s = mkPrimCall1 "lmOne" (mkDummy s)

lmScale :: HasCallStack => Type -> TExpr -> TExpr
-- lmScale :: Float -> (s -o s)
lmScale s r = mkPrimCall1 "lmScale" (Tuple [mkDummy s, r])

lmAdd :: HasCallStack => TExpr -> TExpr -> TExpr
lmAdd = mkPrimCall2 "lmAdd"

lmAdds :: HasCallStack => [TExpr]-> TExpr
lmAdds [] = error "lmAdds of empty list (perhaps this should return lmZero?)"
lmAdds [x] = x
lmAdds (x:xs) = lmAdd x (lmAdds xs)

lmHCat :: HasCallStack => [TExpr] -> TExpr
lmHCat [e] = e
lmHCat es  = mkPrimCall "lmHCat" (Tuple es)

lmHCatV :: HasCallStack => TExpr -> TExpr
lmHCatV e  = mkPrimCall1 "lmHCatV" e

lmVCat :: HasCallStack => [TExpr] -> TExpr
lmVCat [e] = e
lmVCat es  = mkPrimCall1 "lmVCat" (Tuple es)

lmVCatV :: HasCallStack => TExpr -> TExpr
lmVCatV e  = mkPrimCall1 "lmVCatV" e

lmTranspose :: TExpr -> TExpr
lmTranspose = mkPrimCall1 "lmTranspose"

lmCompose :: TExpr -> TExpr -> TExpr
lmCompose = mkPrimCall2 "lmCompose"

lmApply :: HasCallStack => TExpr -> TExpr -> TExpr
lmApply = mkPrimCall2 "lmApply"

lmApplyR :: HasCallStack => TExpr -> TExpr -> TExpr
lmApplyR = mkPrimCall2 "lmApplyR"

lmApply_AD :: HasCallStack => ADMode -> TExpr -> TExpr -> TExpr
lmApply_AD (AD BasicAD dir) = lmApply_Dir  dir
lmApply_AD (AD TupleAD dir) = lmApplyT_Dir dir

lmApply_Dir :: HasCallStack => ADDir -> TExpr -> TExpr -> TExpr
lmApply_Dir Fwd e ds = lmApply  e ds
lmApply_Dir Rev e dt = lmApplyR dt e

lmApplyT_Dir :: HasCallStack => ADDir -> TExpr -> TExpr -> TExpr
lmApplyT_Dir Fwd e ds = mkPrimCall1 "lmApplyT"  (Tuple [e, ds])
lmApplyT_Dir Rev e dt = mkPrimCall1 "lmApplyTR" (Tuple [dt, e])

lmBuild :: HasCallStack => TExpr -> TExpr -> TExpr
lmBuild n b = lmVCatV (pBuild n b)

lmBuildT :: HasCallStack => TExpr -> TExpr -> TExpr
lmBuildT n b = lmHCatV (pBuild n b)

lmFold :: HasCallStack => TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
lmFold = mkPrimCall5 "lmFold"

pFFold :: HasCallStack => TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
pFFold = mkPrimCall6 "FFold"

pRFold :: HasCallStack => Type -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
pRFold = mkPrimCall7 "RFold" . mkDummy

lmDummyFold :: HasCallStack => Type -> TExpr
lmDummyFold = mkPrimCall1 "lmDummyFold" . mkDummy

lmBuild_Dir :: ADDir -> TExpr -> TExpr -> TExpr
lmBuild_Dir Fwd = lmBuild
lmBuild_Dir Rev = lmBuildT

lmVCat_Dir :: ADDir -> [TExpr] -> TExpr
lmVCat_Dir Fwd = lmVCat
lmVCat_Dir Rev = lmHCat

lmVCatV_Dir :: ADDir -> TExpr -> TExpr
lmVCatV_Dir Fwd = lmVCatV
lmVCatV_Dir Rev = lmHCatV

lmHCat_Dir :: ADDir -> [TExpr] -> TExpr
lmHCat_Dir Fwd = lmHCat
lmHCat_Dir Rev = lmVCat

lmCompose_Dir :: ADDir -> TExpr -> TExpr -> TExpr
lmCompose_Dir Fwd m1 m2 = m1 `lmCompose` m2
lmCompose_Dir Rev m1 m2 = m2 `lmCompose` m1

isThePrimFun :: TFun -> String -> Bool
isThePrimFun (TFun _ (Fun (PrimFun f1))) f2 = f1 == f2
isThePrimFun _ _ = False

isLMOne :: TExpr -> Bool
isLMOne (Call f _) = f `isThePrimFun` "lmOne"
isLMOne _ = False

isLMZero :: TExpr -> Bool
isLMZero = isJust . isLMZero_maybe

isLMZero_maybe :: TExpr -> Maybe (TExpr, TExpr)
-- Just (a,b) means that the input was indeed (lmZero (a,b))
isLMZero_maybe (Call f args)
  | f `isThePrimFun` "lmZero"
  , (Tuple [a,b]) <- args
  = Just (a,b)
isLMZero_maybe _ = Nothing

isKZero :: TExpr -> Bool
isKZero = \case
  Konst (KInteger 0  ) -> True
  Konst (KFloat   0.0) -> True
  Tuple ts -> all isKZero ts
  Call f (Tuple [_,v]) | f `isThePrimFun` "constVec" -> isKZero v
  _ -> False

isBuild_maybe :: TExpr -> Maybe (TExpr, TVar, TExpr)
isBuild_maybe (Call f (Tuple [n,Lam i e]))
  | f `isThePrimFun` "build"
  = Just (n, i, e)
isBuild_maybe _ = Nothing

lmDelta :: TExpr -> TExpr -> TExpr -> TExpr
lmDelta t i j = If (pEqual i j) (lmOne ty) (lmZero t t)
  where
    ty = typeof t

isEqualityCall :: TExpr -> Maybe (TExpr, TExpr)
isEqualityCall (Call f (Tuple [e1,e2]))
  | f `isThePrimFun` "eq" = Just (e1,e2)
isEqualityCall _          = Nothing

-----------------------
-- Delta and diag

pDelta :: TExpr -> TExpr -> TExpr -> TExpr
-- delta i j e  =  if i==j then e else zero
pDelta i j e = mkPrimCall1 "delta" (Tuple [i, j, e])

pDeltaVec :: TExpr -> TExpr -> TExpr -> TExpr
-- deltaVec size i e = build size (\j. delta i j e)
-- Returns a size-vector with e at index i, and zeros elsewhere
pDeltaVec sz i e = mkPrimCall1 "deltaVec" (Tuple [sz, i, e])

pConstVec :: TExpr -> TExpr -> TExpr
-- constVec size e = build size (\_. e)
pConstVec = mkPrimCall2 "constVec"

pDiag :: TExpr -> TExpr -> TExpr -> TExpr
-- diag rows cols (\i. e) = build row (\i. deltaVec cols i e)
pDiag = mkPrimCall3 "diag"

---------------------------
-- "User-defined" functions
---------------------------
pAdd, pEqual, pScale :: HasCallStack => TExpr -> TExpr -> TExpr
pAdd   = mkPrimCall2 "ts_add"
pEqual = mkPrimCall2 "eq"
pScale = mkPrimCall2 "ts_scale"

pNeg :: HasCallStack => TExpr -> TExpr
pNeg = mkPrimCall1 "ts_neg"

pBuild :: TExpr -> TExpr -> TExpr
pBuild = mkPrimCall2 "build"

pIndex :: TExpr -> TExpr -> TExpr
pIndex = mkPrimCall2 "index"

pSum :: TExpr -> TExpr
pSum = mkPrimCall1 "sum"

pSumBuild :: TExpr -> TExpr -> TExpr
pSumBuild = mkPrimCall2 "sumbuild"

pUnzip :: TExpr -> TExpr
pUnzip = mkPrimCall1 "unzip"

pSize :: TExpr -> TExpr
pSize e = mkPrimCall1 "size" e

pSel :: Int -> Int -> TExpr -> TExpr
pSel i n e = Call (TFun el_ty
                        (Fun (SelFun i n))) e
           where
             el_ty = case typeof e of
                        TypeTuple ts -> ts !! (i-1)
                        _ -> TypeUnknown  -- Better error from Lint

pFst,pSnd :: TExpr -> TExpr
pFst   = pSel 1 2
pSnd   = pSel 2 2

pToFloat :: TExpr -> TExpr
pToFloat from = userCall "to_float" TypeFloat from

pMulii :: TExpr -> TExpr -> TExpr
pMulii x1 x2 = userCall "mul" TypeInteger (Tuple [x1, x2])

pMulff :: TExpr -> TExpr -> TExpr
pMulff x1 x2 = userCall "mul" TypeFloat (Tuple [x1, x2])


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
         -> Left (text "Ill-typed call to primitive:" <+> ppr fun)

      Fun (SelFun i n) -> selCallResultTy_maybe i n arg_ty

      GradFun f adp
        -> case primCallResultTy_maybe (Fun f) arg_ty of
            Left err -> Left err
            Right res_ty -> Right (mkGradType adp arg_ty res_ty)

      DrvFun f adm
        | AD BasicAD Fwd <- adm    -- f :: S1 -> T, then fwd$f :: (S1, S2_t) -> T_t
        , TypeTuple [x, _dx] <- arg_ty
        , Right t_ty <- primCallResultTy_maybe (Fun f) x
        -> Right (tangentType t_ty)

        | AD TupleAD Fwd <- adm    -- f :: S1 -> T, then fwdt$f :: (S1, S2_t) -> (T,T_t)
        , TypeTuple [x, _dx] <- arg_ty
        , Right t_ty <- primCallResultTy_maybe (Fun f) x
        -> Right (TypeTuple [t_ty, tangentType t_ty])

        | AD BasicAD Rev <- adm    -- f :: S1 -> T, then rev$f :: (S1, T_t) -> S1_t
        , TypeTuple [s, _dt] <- arg_ty
        -> Right (tangentType s)
        | otherwise
        -> Left (text "Ill-typed call to:" <+> ppr fun)

      Fun (UserFun _) -> Left (text "Not in scope: user fun:" <+> ppr fun)

selCallResultTy_maybe :: Int -> Int -> Type -> Either SDoc Type
selCallResultTy_maybe i n (TypeTuple arg_tys)
  | i <= length arg_tys
  , n == length arg_tys
  = Right (arg_tys !! (i - 1))
selCallResultTy_maybe _ _ _ = Left (text "Bad argument to selector")

primFunCallResultTy :: HasCallStack => PrimFun -> TExpr -> Type
primFunCallResultTy fun args
  = case primFunCallResultTy_maybe fun (typeof args) of
      Just res_ty -> res_ty
      Nothing -> pprTrace "primCallResultTy: Could not determine result type for"
                          (vcat [ text fun <+> ppr args
                                , ppr (typeof args)])
                 TypeUnknown

---------------------------------------
-- This is the function that does the heavy lifting for primitives

primFunCallResultTy_maybe :: PrimFun -> Type -> Maybe Type

primFunCallResultTy_maybe "fold" args
  | TypeTuple [f,acc,v] <- args
  , TypeLam (TypeTuple [a1, b1]) a2 <- f
  , TypeVec b2 <- v
  , b1 `eqType` b2
  , Just a <- eqTypes a1 [a2, acc]
  = Just a
  | otherwise = Nothing

primFunCallResultTy_maybe "lmFold" args
  | TypeTuple [ds_zero,f,f',acc,v] <- args
  , TypeLam t1 a1 <- f
  , TypeLam t2 (TypeLM (TypeTuple [s1, t3]) a2) <- f'
  , Just t <- eqTypes t1 [t2, t3]
  , TypeTuple [a3, b1] <- t
  , Just a <- eqTypes a1 [a2, a3, acc]
  , Just _ <- eqTypes ds_zero [tangentType s1]
  , v_ty@(TypeVec b2) <- v
  , b2 `eqType` b1
  = Just (TypeLM (TypeTuple [s1, TypeTuple [a, v_ty]]) a)
  | otherwise = Nothing

--- Type checking is not comprehensive because we only ever generate
--- RFold through reverse applying to an lmFold, and we assume that is
--- done correctly.  We could add more comprehensive type checking
--- later if we want.
primFunCallResultTy_maybe "RFold" args
  | TypeTuple [_ty_dv,ty_in,_f,_f',acc,v,_dr] <- args
  = Just (TypeTuple [ ty_in
                    , TypeTuple [ tangentType acc
                                , tangentType v]])
  | otherwise = Nothing

--- Type checking is not comprehensive because we only ever generate
--- FFold through forward applying to an lmFold, and we assume that is
--- done correctly.  We could add more comprehensive type checking
--- later if we want.
primFunCallResultTy_maybe "FFold" args
  | TypeTuple [_f,_acc,_v,_df,dacc,_dv] <- args
  = Just dacc
  | otherwise = Nothing

primFunCallResultTy_maybe "lmDummyFold" args
  = Just args

primFunCallResultTy_maybe fun args
  = case (fun, args) of
      ("lmZero"   , TypeTuple [s, t])                      -> Just (TypeLM s t)
      ("lmOne"    , t)                                     -> Just (TypeLM t t)
      ("lmScale"  , TypeTuple [t, TypeFloat])              -> Just (TypeLM t t)

      ("lmCompose", TypeTuple [TypeLM _ c, TypeLM a _])    -> Just (TypeLM a c)
      ("lmAdd"    , TypeTuple [TypeLM s1 t1, TypeLM _ _])  -> Just (TypeLM s1 t1)
      ("lmTranspose", TypeLM s t)                          -> Just (TypeLM t s)

      ("lmApply"  , TypeTuple [TypeLM s1 t, s2]) | tangentType s1 `eqType` s2 -> Just (tangentType t)
           -- Linar map apply:  lmApply :: (s -o t) -> ds -> dt
      ("lmApplyR" , TypeTuple [t1, TypeLM s t2]) | t1 `eqType` tangentType t2 -> Just (tangentType s)
           -- Reverse apply:  lmApplyR :: dt -> (s -o t) -> ds

      ("lmApplyT" , TypeTuple [TypeTuple [_, TypeLM s1 t], s2])
                                | tangentType s1 `eqType` s2 -> Just (tangentType t)
           -- Tupled version:  lmApplyT :: (r, s -o t) -> ds -> dt

      ("lmVCat"   , TypeTuple tys) | Just (ss,ts) <- unzipLMTypes tys
                                     , (s1:ss1) <- ss
                                     , all (== s1) ss1     -> Just (TypeLM s1 (TypeTuple ts))
      ("lmVCatV"  , TypeVec (TypeLM s t))                  -> Just (TypeLM s (TypeVec t))
      ("lmHCat"   , TypeTuple tys) | Just (ss,ts) <- unzipLMTypes tys
                                     , (t1:ts1) <- ts
                                     , all (== t1) ts1     -> Just (TypeLM (TypeTuple ss) t1)
      ("lmHCatV"  , TypeVec (TypeLM t s))                  -> Just (TypeLM (TypeVec t) s)

      -- ($inline f args) forces f to be inlined here
      ("$inline"  , t)                                     -> Just t

      -- ($check f rev$f s s' ds dt) verifies the derivatives rev$f at s in directions ds,dt.
      -- That is, ds and dt should be near-zero elements of the domain and range tangent spaces
      -- and the returned value dt'*Jacobian(f)*ds should be similar to dt'*(f(s+ds)-f(s))
      --
      -- NB s and s' should be equal, except if s' is not a tuple, in
      -- which case s should be (tuple s')
      ("$check"   , TypeTuple
                      [ TypeLam s t
                      , TypeLam s_dt ds, s', s'0, ds', dt])
                      | s `eqType` case s' of TypeTuple [s'1] -> s'1
                                              _               -> s'
                      , tangentType s `eqType` ds
                      , tangentType s' `eqType` ds'
                      , tangentType t `eqType` dt
                      , s_dt `eqType` (TypeTuple [s'0, dt])
                       -> Just TypeFloat

      -- ($trace e) emits its argument's value to stdout and returns it
      ("$trace"   , t)                                       -> Just t

      ("constVec" , TypeTuple [TypeInteger, t])              -> Just (TypeVec t)
      ("deltaVec" , TypeTuple [TypeInteger, TypeInteger, t]) -> Just (TypeVec t)
      ("diag"     , TypeTuple [TypeInteger,
                                TypeInteger,
                                TypeLam TypeInteger t])      -> Just (TypeVec (TypeVec t))
      ("build"    , TypeTuple
                     [TypeInteger, TypeLam TypeInteger t])   -> Just (TypeVec t)

      -- (pr a b c) prints its arguments to stdout, with banners.  We should deprecate it.
      ("pr"       , _)                                     -> Just TypeInteger
      -- (print a b c) prints its arguments to stdout with no separators
      ("print"    , _)                                     -> Just TypeInteger
      ("sumbuild" , TypeTuple
                     [TypeInteger, TypeLam TypeInteger t]) -> Just t
      ("index"    , TypeTuple [TypeInteger, TypeVec t])    -> Just t
      ("size"     , TypeVec _)                             -> Just TypeSize
      ("sum"      , TypeVec t)                             -> Just t

      ("unzip"    , TypeVec (TypeTuple ts))                -> Just (TypeTuple (map TypeVec ts))

      ("ts_scale" , TypeTuple [TypeFloat,   t]           ) -> Just t
      ("ts_add"   , TypeTuple [t, dt]                    ) -> if dt == tangentType t
                                                                then Just t
                                                                else Nothing
      ("ts_neg"   , t                                    ) -> Just t
      -- For eq and ne we check that the two arguments have the same type
      ("eq"       , TypeTuple [t1, t2]                   )
        | t1 `eqType` t2 -> Just TypeBool
        | otherwise      -> Nothing
      ("ne"       , TypeTuple [t1, t2]                   )
        | t1 `eqType` t2 -> Just TypeBool
        | otherwise      -> Nothing

      ("delta"    , TypeTuple
                     [TypeInteger, TypeInteger, t])        -> Just t
      _ -> Nothing

isPrimFun :: String -> Bool
isPrimFun f = f `elem` [ "$inline"  -- ($inline f args...)        Force inline f at args
                       , "$check"   -- ($check f rev$f x dx df)   Derivative check df' * D$f * dx
                       , "$trace"   -- ($trace f args)            Print and return (f args)
                       , "pr"       -- (pr "msg" 3)               Print "msg\n---3\n"
                       , "print"    -- (print "msg" 3)            Print "msg3"
                       , "build"    -- (build N f)                Build vector [(f i) for i = 1..n]
                       , "sumbuild" -- (sumbuild N f)             (sum (build N f))
                       , "fold"     -- (fold f z v)               (Left) fold over v
                       , "index"
                       , "size"
                       , "sum"
                       , "unzip"   -- Takes a vector of tuples to a tuple of vectors
                       , "ts_neg"
                       , "ts_add"
                       , "eq", "ne", "delta", "deltaVec", "diag", "constVec"
                       , "lmApply", "lmApplyR", "lmApplyT", "lmVCat", "lmHCat", "lmTranspose"
                       , "lmVCatV", "lmHCatV"
                       , "lmCompose", "lmAdd", "lmScale"
                       , "lmZero", "lmOne"
                       ]
