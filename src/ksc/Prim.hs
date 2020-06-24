{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE LambdaCase #-}

module Prim where

import Lang
import LangUtils
import GHC.Stack
import Data.Maybe

{- Note [Primitive functions and user-defined functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "primitive" function, foo, behaves like this:

* Its FunId is (PrimFun "foo")

* It has a polymorphic type.  If its type was monomorphic,
  you could define it via edef in prelude.ks

* Its type is given by primFunCallResultTy_maybe

* It is dealt with specially by the code generator;
  it has no definition in KSC.

* It has derived forms
     GradFun (PrimFun "foo") adp    -- D$foo, etc
     DrvFun (PrimFun "foo") adm     -- fwd$foo, etc
  Their types too are computed by primFunCallResultTy_maybe

* These derived forms are typically inlined in Opt.

A user-defined function, bar, behaves like this:

* Its FunId is (UserFun "bar")

* It may or may not have a definition in KSC.  If it is edef'd it
  doesn't; in that case the code generator must deal with them
  specially.

* User-defined functions can still have custom rewrite rules
  in Opt.hs
-}

--------------------------------------------
--  Simple call construction
--------------------------------------------

primCall :: BuiltinFunName -> Type -> TExpr -> TExpr
primCall fun res_ty
  = Call (TFun res_ty (Fun (PrimFun (Builtin fun))))

userCall :: String -> Type -> TExpr -> TExpr
userCall fun res_ty
  = Call (TFun res_ty (Fun (UserFun fun)))

mkPrimCall :: HasCallStack => BuiltinFunName -> TExpr -> TExpr
mkPrimCall fun args
  = primCall fun res_ty args
  where
    res_ty = primFunCallResultTy fun args

mkPrimCall1 :: HasCallStack => BuiltinFunName -> TExpr -> TExpr
mkPrimCall1 f a = mkPrimCall f a

mkPrimCall2 :: HasCallStack => BuiltinFunName -> TExpr -> TExpr -> TExpr
mkPrimCall2 f a b = mkPrimCall f (Tuple [a, b])

mkPrimCall3 :: HasCallStack => BuiltinFunName -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall3 f a b c = mkPrimCall f (Tuple [a, b, c])

mkPrimCall4 :: HasCallStack => BuiltinFunName -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall4 f a b c d = mkPrimCall f (Tuple [a, b, c, d])

mkPrimCall5 :: HasCallStack => BuiltinFunName -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall5 f a b c d e = mkPrimCall f (Tuple [a, b, c, d, e])

mkPrimCall6 :: HasCallStack => BuiltinFunName -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
mkPrimCall6 f a b c d e g = mkPrimCall f (Tuple [a, b, c, d, e, g])

mkPrimCall7 :: HasCallStack => BuiltinFunName -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr -> TExpr
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
             Just ("fwds", s) -> DrvFun  (mk_fun_id s) (AD SplitAD Fwd)
             Just ("revs", s) -> DrvFun  (mk_fun_id s) (AD SplitAD Rev)
             Just ("get", s) -> Fun     (mk_sel_fun s)
             _               -> Fun     (mk_fun_id f)
  where
    mk_fun_id f | isPrimFun f = PrimFun (Builtin f)
                | otherwise   = UserFun f
    find_dollar f = case break (== '$') f of
                       (_, [])  -> Nothing  -- No $
                       (_, [_]) -> Nothing  -- Trailing $
                       (prefix, _ : suffix) -> Just (prefix, suffix)
    mk_sel_fun s = case break (== '$') s of
                     (i,_:n) -> PrimFun (Sel (read i :: Int) (read n :: Int))
                     _ -> error $ "'get' should have form 'get$i$n', not [get$" ++ s ++ "]"


---------------------------
-- Zeros
---------------------------
mkZero, mkTangentZero :: HasCallStack => TExpr -> TExpr
mkZero        = getZero id
mkTangentZero = getZero tangentType

mkTangentZeroFromType :: Type -> TExpr
-- Only works for types with no vectors,
-- for which needValueForTangentZero says True
mkTangentZeroFromType TypeInteger    = unitExpr
mkTangentZeroFromType TypeFloat      = Konst (KFloat 0.0)
mkTangentZeroFromType TypeString     = unitExpr
mkTangentZeroFromType TypeBool       = unitExpr
mkTangentZeroFromType (TypeTuple ts) = Tuple (map mkTangentZeroFromType ts)
mkTangentZeroFromType ty             = pprTrace "mkTangentZeroFromType" (ppr ty)
                                       Konst (KInteger 99)  -- A distinctive value

needValueForTangentZero :: Type -> Bool
needValueForTangentZero TypeInteger    = False
needValueForTangentZero TypeFloat      = False
needValueForTangentZero TypeString     = False
needValueForTangentZero TypeBool       = False
needValueForTangentZero (TypeTuple ts) = any needValueForTangentZero ts
needValueForTangentZero TypeUnknown    = False
needValueForTangentZero (TypeVec {})   = True
needValueForTangentZero ty             = pprPanic "needValueForTangentZero" (ppr ty)

getZero :: HasCallStack => (Type -> Type) -> TExpr -> TExpr
getZero tangent_type e
  = go e
  where
    go e = case tangent_type (typeof e) of
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
            _ -> pprPanic "mkZero" (ppr (typeof e) $$ ppr e)

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
lmApply_AD (AD SplitAD _)   = error "lmApply_AD:SplitAD"  -- Doesn't make sense

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
pTsAdd, pTsScale :: HasCallStack => TExpr -> TExpr -> TExpr
pTsAdd   = mkPrimCall2 "ts_add"
pTsScale = mkPrimCall2 "ts_scale"

pEqual :: HasCallStack => TExpr -> TExpr -> TExpr
pEqual = mkPrimCall2 "eq"

pNeg :: HasCallStack => TExpr -> TExpr
pNeg = mkPrimCall1 "ts_neg"

pBuild :: TExpr -> TExpr -> TExpr
pBuild = mkPrimCall2 "build"

pIndex :: TExpr -> TExpr -> TExpr
pIndex = mkPrimCall2 "index"

pSum :: TExpr -> TExpr
-- pSum = mkPrimCall1 "sum"
pSum = userCall "sum" TypeFloat

pTupCons :: TExpr -> TExpr -> TExpr
-- Put arg1 on the front of the tuple arg2
pTupCons arg1 arg2
  | Tuple args <- arg2          = Tuple (arg1:args)
  | TypeTuple {} <- typeof arg2 = mkPrimCall2 "tupCons" arg1 arg2
  | otherwise                   = Tuple [arg1,arg2]

pTupHead :: TExpr -> TExpr
pTupHead e | Tuple (e1:_) <- e        = e1
           | TypeTuple es <- typeof e = pSel 1 (length es) e
           | otherwise = pprTrace "pTupHead" (ppr e <+> dcolon <+> ppr (typeof e)) $
                         Konst (KInteger 99)

pTupTail :: TExpr -> TExpr
pTupTail e | Tuple (_:es) <- e       = mkTuple es
           | TypeTuple [_,_] <- e_ty = pSel 2 2 e
           | TypeTuple {}    <- e_ty = mkPrimCall1 "tupTail" e
           | otherwise = pprTrace "pTupTail" (ppr e <+> dcolon <+> ppr e_ty) $
                         Konst (KInteger 88)
           where
             e_ty = typeof e

pSumBuild :: TExpr -> TExpr -> TExpr
pSumBuild = mkPrimCall2 "sumbuild"

pUnzip :: TExpr -> TExpr
pUnzip = mkPrimCall1 "unzip"

pSize :: TExpr -> TExpr
pSize e = mkPrimCall1 "size" e

pToFloat :: TExpr -> TExpr
pToFloat from = userCall "to_float" TypeFloat from

pMulii :: TExpr -> TExpr -> TExpr
pMulii x1 x2 = userCall "mul" TypeInteger (Tuple [x1, x2])

pMulff :: TExpr -> TExpr -> TExpr
pMulff x1 x2 = userCall "mul" TypeFloat (Tuple [x1, x2])


---------------------------------------------
--       Types of primitive functions
--
--  For each primitive, we give its type
--  And this is the /only/ place we do this
---------------------------------------------

primCallResultTy_maybe :: HasCallStack => Fun -> Type
                       -> Either SDoc Type
-- Precondition: isUserFun (funIdOfFun fun) = False
-- So fun_id is PrimFun or SelFun
primCallResultTy_maybe fun arg_ty
  = case fun of
      Fun (PrimFun (Builtin f))
         | Just ty <- primFunCallResultTy_maybe f arg_ty
         -> Right ty
         | otherwise
         -> Left (text "Ill-typed call to primitive:" <+> ppr fun)

      Fun (PrimFun (Sel i n)) -> selCallResultTy_maybe i n arg_ty

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

        | AD SplitAD Fwd <- adm
        , Right t_ty <- primCallResultTy_maybe (Fun f) arg_ty
        , Just x_ty <- xfType f arg_ty
        -> Right (TypeTuple [t_ty, x_ty])

        | AD SplitAD Rev <- adm
        , Just ds <- xrType f arg_ty
        -> Right ds

        | otherwise
        -> Left (text "Ill-typed call to:" <+> ppr fun)

      Fun (UserFun _) -> Left (text "Not in scope: user fun:" <+> ppr fun)

xfType :: FunId -> Type -> Maybe Type
xfType (PrimFun (Builtin fun)) arg = primFunXFType fun arg
xfType _ _ = Nothing

primFunXFType :: BuiltinFunName -> Type -> Maybe Type
primFunXFType fun arg
  = case (fun, arg) of
     ("index", TypeTuple [TypeInteger, TypeVec _]) -> Just (TypeTuple [TypeInteger, TypeInteger])
               -- X = (size of vector, index)
     _ -> pprTrace "primFunXFType: no XType" (ppr fun <+> ppr arg)
          Nothing

xrType :: FunId -> Type -> Maybe Type
xrType (PrimFun (Builtin fun)) arg = primFunXRType fun arg
xrType _ _ = Nothing

primFunXRType :: BuiltinFunName -> Type -> Maybe Type
primFunXRType fun arg
  = case (fun, arg) of
     ("index", TypeTuple [dt, TypeTuple [TypeInteger, TypeInteger]]) -> Just (TypeTuple [unitType, TypeVec dt])
     _ -> pprTrace "primFunXType: no XType" (ppr fun <+> ppr arg)
          Nothing

selCallResultTy_maybe :: Int -> Int -> Type -> Either SDoc Type
selCallResultTy_maybe i n arg_ty
  | i == 1, n == 1
  = Right arg_ty
  | TypeTuple arg_tys <- arg_ty
  , i <= length arg_tys
  , n == length arg_tys
  = Right (arg_tys !! (i - 1))
selCallResultTy_maybe _ _ _ = Left (text "Bad argument to selector")

primFunCallResultTy :: HasCallStack => BuiltinFunName -> TExpr -> Type
primFunCallResultTy fun args
  = case primFunCallResultTy_maybe fun (typeof args) of
      Just res_ty -> res_ty
      Nothing -> pprTrace "primCallResultTy: Could not determine result type for"
                          (vcat [ text fun <+> ppr args
                                , ppr (typeof args)])
                 TypeUnknown

---------------------------------------
-- This is the function that does the heavy lifting for primitives

primFunCallResultTy_maybe :: BuiltinFunName -> Type -> Maybe Type

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

      -- (pr a b c) prints its arguments to stdout, with banners.  We should deprecate it.
      ("pr"       , _)                                     -> Just TypeInteger
      -- (print a b c) prints its arguments to stdout with no separators
      ("print"    , _)                                     -> Just TypeInteger

      -- ($trace e) emits its argument's value to stdout and returns it
      ("$trace"   , t)                                       -> Just t

      -- Polymorphic functions over vectors
      ("constVec" , TypeTuple [TypeInteger, t])              -> Just (TypeVec t)
      ("deltaVec" , TypeTuple [TypeInteger, TypeInteger, t]) -> Just (TypeVec t)
      ("diag"     , TypeTuple [TypeInteger,
                                TypeInteger,
                                TypeLam TypeInteger t])      -> Just (TypeVec (TypeVec t))
      ("build"    , TypeTuple
                     [TypeInteger, TypeLam TypeInteger t])   -> Just (TypeVec t)
      ("sumbuild" , TypeTuple
                     [TypeInteger, TypeLam TypeInteger t]) -> Just t
      ("index"    , TypeTuple [TypeInteger, TypeVec t])    -> Just t
      ("size"     , TypeVec _)                             -> Just TypeSize
--      ("sum"      , TypeVec t)                             -> Just t
      ("unzip"    , TypeVec (TypeTuple ts))                -> Just (TypeTuple (map TypeVec ts))

      -- Polymorphic tangent-space functions
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
      ("tupCons" , TypeTuple [t, TypeTuple ts] ) -> Just (TypeTuple (t:ts))
      ("tupHead" , TypeTuple (t:_)             ) -> Just t
      ("tupTail" , TypeTuple (_:ts)            ) | length ts >= 2
                                                  -> Just (TypeTuple ts)

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
                       , "tupCons", "tupTail"
                       , "size"
--                       , "sum"
                       , "unzip"   -- Takes a vector of tuples to a tuple of vectors
                       , "ts_neg"
                       , "ts_add"
                       , "eq", "ne", "delta", "deltaVec", "diag", "constVec"
                       , "lmApply", "lmApplyR", "lmApplyT", "lmVCat", "lmHCat", "lmTranspose"
                       , "lmVCatV", "lmHCatV"
                       , "lmCompose", "lmAdd", "lmScale"
                       , "lmZero", "lmOne"
                       ]


--------------------------------------------
--  Rewriting primitive calls
--------------------------------------------

rewritePrimCall :: OptEnv -> PrimFun -> Fun -> TExpr
                -> Type    -- Result type of call
                -> Maybe TExpr

-- First deal with GradFun/TupleAD
rewritePrimCall env pf (GradFun f TupleAD) arg tup_res_ty
  | TypeTuple [res_ty, lm_ty] <- tup_res_ty
  , Just opt_grad <- rewritePrimCall env pf (GradFun f BasicAD) new_arg lm_ty
  = Just $
    mkLets binds $
    Tuple [ Call (TFun res_ty (Fun f)) new_arg, opt_grad ]
  where
    (binds, [new_arg]) = makeAtomic False (optEnvInScope env) [arg]

rewritePrimCall _ (Sel i n) fun arg _
  = optSelCall i n fun arg

rewritePrimCall env (Builtin f) fun arg _
  = checkMissingConstantFold f arg $
    case (f,arg) of
      ("index",    Tuple [e1,e2])     -> optIndex     fun e1 e2
      ("sum",      _)                 -> optSum       fun arg
      ("ts_add",   Tuple [e1,e2])     -> optTsAdd     fun e1 e2
      ("ts_scale", Tuple [e1,e2])     -> optTsScale   fun e1 e2
      ("tupTail",  _)                 -> optTupTail   fun arg
      ("lmVCat",   _)                 -> optLMVCat    fun arg
      ("lmAdd",     Tuple [e1,e2])    -> optLMAdd     fun e1 e2
      ("lmCompose", Tuple [e1,e2])    -> optLMCompose fun e1 e2
      ("lmApply",   Tuple [e1,e2])    -> optLMApply fun is (AD BasicAD Fwd) e1 e2
      ("lmApplyR",  Tuple [e1,e2])    -> optLMApply fun is (AD BasicAD Rev) e1 e2
      ("lmApplyT",  Tuple [e1,e2])    -> optLMApply fun is (AD TupleAD Fwd) e1 e2
      ("lmApplyTR", Tuple [e1,e2])    -> optLMApply fun is (AD TupleAD Fwd) e1 e2
      ("$inline",  _)                 -> optInline fun env arg
      ("$trace",  _)                  -> optTrace  fun arg
      ("size",      _)                -> optSize fun arg
      ("deltaVec", Tuple [e1,e2,e3])     -> optDeltaVec fun e1 e2 e3
      ("build",    Tuple [sz, Lam i e2]) -> optBuild    fun sz i e2
      ("sumBuild", Tuple [sz, Lam i e2]) -> optSumBuild fun sz i e2
      _ -> Nothing
  where
    is = optEnvInScope env

checkMissingConstantFold :: BuiltinFunName -> TExpr -> Maybe a -> Maybe a
-- Check for a missing constant-folding opportunity
-- TODO: match precision to target machine
checkMissingConstantFold f args@(Tuple [Konst (KFloat _), Konst (KFloat _)]) Nothing
  = error $ unlines $
          [ "Failed constant folding [" ++ f ++ "]."
          , show args
          , "This error exists to prompt you, the ksc user,"
          , "to go ahead and add a constant folding rule for"
          , "this operation to ksc.  See also"
          , ""
          , "    https://github.com/microsoft/knossos-ksc/pull/61/commits/29c2ab04568e17b953d3fe942aba5881ab15e1f8#r309892713"
          ]

checkMissingConstantFold _ _ result = result

------------- sel$i$n ------------

optSelCall :: Int -> Int -> Fun -> TExpr -> Maybe TExpr

optSelCall i n (Fun {}) arg
  | n == 1, i == 1
  = Just arg

  -- RULE:  sel_i_n (..., ei, ...)  ==>  ei
  | Tuple es <- arg
  , i <= length es
  = Just (es !! (i-1))

  -- RULE:  sel_1_n (tupCons e es)  ==> e
  -- RULE:  sel_i_n (tupCons e es)  ==> sel_(i-1)_(n-1) es
  | Call tupCons (Tuple [e,es]) <- arg
  , tupCons `isThePrimFun` "tupCons"
  = case i of
      1 -> Just e
      _ -> Just (pSel (i-1) (n-1) es)

  | otherwise
  = Nothing

--   sel 2 3 :: (a,b,c) -> b
-- D$sel 2 3 :: (a,b,c) -> (a,b,c) -o b
--
-- D$sel 2 3 (a,b,c)  --->   lmHCat [ 0 :: S1 -o S2
--                                  , 1 :: S2 -o S2
--                                  , 0 :: S3 -o S2 ]
-- NB: this works regardless of the value argument;
--     the gradient is the same everywhere
optSelCall i n (GradFun _ BasicAD) arg
  | TypeTuple tys <- typeof arg
  , length tys == n
  , let tyi = tys !! (i-1)
        ti  = pSel i n arg
  = Just $
    lmHCat [ if i == j then lmOne tyi
                       else lmZero (pSel j n arg) ti
           | j <- [1..n] ]

  | otherwise
  = pprTrace ("GradSel failed") (ppr arg)
    Nothing


optSelCall i n (DrvFun _ (AD BasicAD Fwd)) (Tuple [_, ds])
  = Just (pSel i n ds)

optSelCall i n (DrvFun _ (AD TupleAD Fwd)) (Tuple [s, ds])
  = Just (Tuple [pSel i n s, pSel i n ds])

optSelCall i n (DrvFun _ (AD BasicAD Rev)) (Tuple [s, dt])
  = Just (mkTuple (map mk_dr [1..n]))    -- Returns (0,0,dt,0)
  where
    mk_dr j | i==j      = dt
            | otherwise = mkTangentZero (pSel j n s)

optSelCall _ _ _ _  = Nothing
  -- ToDo: what is supposed to happen for (AD TupleAD Rev)?


------------- $inline ------------

optInline :: Fun -> OptEnv -> TExpr -> Maybe TExpr
-- $inline needs to look up the global symtab
optInline fun env arg
  | Fun {} <- fun   -- Can't do grad versions of $inline
  , Call (TFun _ fun) inner_arg <- arg
  , Just fun_def <- lookupGblST (fun, typeof inner_arg) (optGblST env)
  , Def { def_pat = pat, def_rhs = UserRhs body } <- fun_def
  = Just (inlineCall (optEnvInScope env) pat body inner_arg)
  | otherwise
  = Nothing

------------- $trace ------------

optTrace :: Fun -> TExpr -> Maybe TExpr

optTrace (GradFun _ BasicAD) arg
  = Just (lmOne (typeof arg))

optTrace _ _ = Nothing

------------------ ts_add ---------------

optTsAdd :: Fun -> TExpr -> TExpr -> Maybe TExpr

-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optTsAdd (GradFun _ BasicAD) e1 e2
  = Just (lmHCat [lmOne (typeof e1), lmOne (typeof e2)])

optTsAdd (Fun {}) e1 e2
  -- RULE: (e1 : ()) + (e2 : ()) = ()
  -- The type () contains only one value (),
  -- which is a zero of the type.  We use () as the tangent
  -- type for non-differentiatable types
  | TypeTuple [] <- typeof e1
  , TypeTuple [] <- typeof e2
  = Just (Tuple [])

  -- RULE: (a1,a2) + (b1,b2) = (a1+a2, b1+b2)
  | Tuple es1 <- e1
  , Tuple es2 <- e2
  , length es1 == length es2
  = Just (Tuple (zipWith pTsAdd es1 es2))

  | Konst (KFloat k1) <- e1
  , Konst (KFloat k2) <- e2
  = Just (Konst (KFloat (k1+k2)))

      -- RULE: x+0 = 0+x = x
  | isKZero e1 = Just e2
  | isKZero e2 = Just e1

  | otherwise = Nothing

optTsAdd _ _ _ = Nothing

------------------ ts_scale ---------------

optTsScale :: Fun -> TExpr -> TExpr -> Maybe TExpr

optTsScale (Fun {}) x y
  -- RULE: scale 0 y = 0
  | isKZero x || isKZero y
  -- We use the type of y because the typing rule
  -- for scale in Prim.hs is
  --     scale: (Float, t) -> t
  = Just $ mkZero y

optTsScale _ _ _ = Nothing

------------------ tupTail ---------------

optTupTail :: Fun -> TExpr -> Maybe TExpr
optTupTail (Fun {}) arg
  | Tuple [_,e2]  <- arg = Just e2
  | Tuple (_:es2) <- arg = Just (Tuple es2)

optTupTail _ _ = Nothing

------------------ size ---------------

optSize :: Fun -> TExpr -> Maybe TExpr

-- RULE: size (build (n, _)) = n
optSize (Fun {}) (Call build (Tuple [n,_]))
  | build `isThePrimFun` "build"
  = Just n

optSize (GradFun _ BasicAD) arg
  = Just $ lmZero arg zeroInt

optSize _ _ = Nothing


------------------ index ---------------

optIndex :: Fun -> TExpr -> TExpr -> Maybe TExpr
-- RULE: index j (build n f) = f j
optIndex (Fun {}) ei arr
  | Just (_, i, e) <- isBuild_maybe arr
  = Just (Let i ei e)


optIndex (GradFun _ BasicAD) i v
  = Just (lmHCat [ lmZero i vi
                 , lmBuildT (pSize v) (Lam ii (lmDelta vi (Var ii) i)) ])
  where
    ii = TVar TypeInteger $ Simple "primDindex$i"
    vi = pIndex i v

optIndex _ _ _ = Nothing


------------------ deltaVec ---------------

optDeltaVec :: Fun -> TExpr -> TExpr -> TExpr -> Maybe TExpr

-- RULE: deltaVec n i 0 = zero (build n (\i . 0))
optDeltaVec (Fun {}) n _i val
  | isKZero val
  = Just $ pConstVec n val
optDeltaVec _ _ _ _ = Nothing


------------------ sum -------------

optSum :: Fun -> TExpr -> Maybe TExpr

-- ToDo: Where is this?
-- RULE: sum (build n (\i. (e1,e2,...)))
--       = (sum (build n (\i.e1)), sum (build n (\i.e2)), ...)

-- ToDo: Where is this?
-- RULE: sum (build n (\i. if (i==ej) then v else 0)
--  = let i = ej in v

optSum (Fun {}) arg
  -- RULE: sum (build n (\i. e)) = (sumbuild n (\i. e))
  | Just (n, i, body) <- isBuild_maybe arg
  = Just $ pSumBuild n (Lam i body)

  -- RULE: sum (diag sz f)  =  build sz f
  | Call diag (Tuple [sz, f]) <- arg
  , diag `isThePrimFun` "diag"
  = Just $ pBuild sz f

  -- RULE: sum (deltaVec sz i e) = e
  | Call deltaVec (Tuple [_, _, e]) <- arg
  , deltaVec `isThePrimFun` "deltaVec"
  = Just e

optSum (GradFun _ BasicAD) arg
  | TypeVec t <- typeof arg
  = Just (lmBuildT (pSize arg) (Lam (TVar TypeSize $ Simple "sum$i")
                               (lmOne t)))

optSum _ _ = Nothing

------------------ build -------------

optBuild :: Fun -> TExpr -> TVar -> TExpr -> Maybe TExpr

-- RULE: build sz (\i. <zero>)  =  <zero>
{-
optBuild _ _ e
  | isKZero e
  = Just $ pZero (.. the build)
-}

-- RULE: build sz (\i. delta i ex eb)  =  let i = ex in
--                                        deltaVec sz i eb
--       (if i is not free in ex)
-- NB: however, i might be free in eb
optBuild (Fun {}) sz i e
  | Call delta (Tuple [e1,e2,eb]) <- e
  , delta `isThePrimFun` "delta"
  , Just ex <- ok_eq e1 e2
  , i `notFreeIn` ex
  = Just $ Let i ex $ pDeltaVec sz (Var i) eb
  where
    -- We want this to work for both (\i. delta i j e)
    --                           and (\j. delta i j e)
    ok_eq :: TExpr -> TExpr -> Maybe TExpr
    ok_eq (Var v) e2 | v == i = Just e2
    ok_eq e1 (Var v) | v == i = Just e1
    ok_eq _ _ = Nothing

-- RULE: build sz (\i. deltaVec sz i e)   = diag sz (\i. e)
optBuild (Fun {}) sz i build_e
  | Call deltaVec (Tuple [sz2, Var i2, e]) <- build_e
  , deltaVec `isThePrimFun` "deltaVec"
  , i  == i2
  = Just $ pDiag sz sz2 (Lam i e)

{-
-- RULE: build sz (\i. f e1 ... eN ...)  =
--         let tN = eN in
--         build sz (\i. f e1 .. tN ... )
-- { if i is not free in eN }
optBuild (Fun {}) sz i e
  | Call tf@(TFun _ (Fun (UserFun _))) (Tuple [e1,e2]) <- e
  , is_expensive e2 -- try to apply only to "expensive" operations
  , i `notFreeIn` e2
  = Just $ Let tmp e2 $ pBuild sz (Lam i (Call tf (Tuple [e1, Var tmp])))
  where
    tmp = newVarNotInT (typeof e2)
                       (pBuild sz (Lam i e))
          -- Slightly inefficient to reassemble outer expr here
    is_expensive (Var _) = False
    is_expensive (Konst _) = False
    is_expensive _ = False
-}

-- build sz (\i. e1 * e2)  = (build sz (\i.e1)) * e2
-- { if i is not free in e2 }
{-  TODO: once decided on amount of polymorphism in *
optBuild (Fun {}) sz i e
  | Call f (Tuple [e1,e2]) <- e
  , f `isThePrimFun` "mul"
  , i `notFreeIn` e2
  , is_expensive e2
  = Just $ pMul (pBuild sz (Lam i e1)) e2
  where
      is_expensive (Call _ _) = True
      is_expensive _ = False
--}

optBuild _ _ _ _ = Nothing

------------------ sumBuild ---------------

optSumBuild :: Fun -> TExpr -> TVar -> TExpr -> Maybe TExpr

-- RULE: sumbuild n (\i. (e1,e2))  =  (sumbuild n (\i.e1), sumbuild n (\i.e2))
optSumBuild (Fun {}) n i (Tuple es)
  = Just $ Tuple (map (pSumBuild n . Lam i) es)

-- RULE: sumbuild n (\i. e)  =  n * e, when i not free in e
optSumBuild (Fun {}) sz i e
  | TVar TypeInteger _ <- i
  , i `notFreeIn` e
  = Just $ sz' sz e
    where sz' = case typeof e of
                  TypeInteger -> pMulii
                  _ -> pTsScale . pToFloat

-- RULE: sumbuild n (\i. delta i ej e)    where i is not free in ej
--       = let i = ej in e
optSumBuild (Fun {}) _ i (Call delta (Tuple [Var i1, ej, e]))
  | delta `isThePrimFun` "delta"
  , i == i1
  , i `notFreeIn` ej
  = Just (Let i ej e)

-- RULE: sumbuild n (\i. deltaVec n i e)
--       = build n (\i. e)
--
-- We prove this rule by showing that LHS = RHS at each index.
--
--   index j (sumbuild n (\i. deltaVec n i e))
-- = sumbuild n (\i. index j (deltaVec n i e))
-- = sumbuild n (\i. if j == i then e else 0)
-- = e[i->j]
-- = index j (build n (\i. e))
optSumBuild (Fun {}) n i (Call deltaVec (Tuple [_n1, Var i1, e]))
  | deltaVec `isThePrimFun` "deltaVec"
  , i == i1
  -- TODO n == sz
  = Just $ pBuild n (Lam i e)

optSumBuild _ _ _ _ = Nothing

------------------ lmVCat ---------------

optLMVCat :: Fun -> TExpr -> Maybe TExpr

optLMVCat (Fun {}) (Tuple es)
  | Just prs <- mapM isLMZero_maybe es
  , (s:_, ts) <- unzip prs
  = Just $ lmZero s (mkTuple ts)

optLMVCat _ _ = Nothing

------------------ lmAdd ---------------

optLMAdd :: Fun -> TExpr -> TExpr -> Maybe TExpr

optLMAdd (Fun {}) p q
  -- Add(0, x) = x = Add(x, 0)
  | isLMZero p = Just q
  | isLMZero q = Just p

  -- Add(Scale(x), Scale(y)) = Scale(Add(x,y))
  | Call scale1 (Tuple [t1, x]) <- p
  , Call scale2 (Tuple [t2, y]) <- q
  , scale1 `isThePrimFun` "lmScale"
  , scale2 `isThePrimFun` "lmScale"
  , typeof t1 == typeof t2
  = Just $ lmScale (typeof t1) (pTsAdd x y)

  -- Add(HCat(p1, p2, ...), HCat(q1, q2, ...)) = Hcat(Add(p1, q1), Add(p2, q2), ...)
  | Call hcat1 (Tuple ps) <- p
  , Call hcat2 (Tuple qs) <- q
  , hcat1 `isThePrimFun` "lmHCat"
  , hcat2 `isThePrimFun` "lmHCat"
  = Just (lmHCat (zipWith (\ pi qi -> lmAdds [pi, qi]) ps qs))

optLMAdd _ _ _ = Nothing

------------------ lmCompose ---------------

optLMCompose :: Fun -> TExpr -> TExpr -> Maybe TExpr
optLMCompose fun@(Fun {}) f g
  | isLMOne f  = Just g

  -- 1 o g = g
  | isLMOne g  = Just f

  -- f o 0 = 0
-- Can't do this without vector sizes :-(
-- optLMCompose f g
--  | isLMZero f = Just (lmZero s t)
--  | isLMZero g = Just (lmZero s t)

  -- Scale(T, x) . Scale(T, y) = Scale(T, xy )
  | Call scale1 (Tuple [t1, x]) <- f
  , Call scale2 (Tuple [t2, y]) <- g
  , scale1 `isThePrimFun` "lmScale"
  , scale2 `isThePrimFun` "lmScale"
  , typeof t1 == typeof t2
  = Just $ lmScale (typeof t1) (pMulff x y)

  -- (f . g) . h   =>   f . (g . h)
  | Call lmcomp (Tuple [p1,p2]) <- f
  , lmcomp `isThePrimFun` "lmCompose"
  = optLMCompose fun p1 (lmCompose p2 g)

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call hcat (Tuple qs) <- g
  , hcat `isThePrimFun` "lmHCat"
  = Just (lmHCat (map (lmCompose f) qs))

  -- (m1 `hcat` m2) . (m3 `vcat` m4)  =>  (m1 . m3) + (m2 . m4)
  | Call hcat (Tuple ps) <- f
  , Call vcat (Tuple qs) <- g
  , hcat `isThePrimFun` "lmHCat"
  , vcat `isThePrimFun` "lmVCat"
  = traceWhenUnequal "H o V" (length ps) (length qs) $
    Just (lmAdds (zipWith lmCompose ps qs))

optLMCompose _ _ _ = Nothing

---------------
-- Called for (lmApply lm dx)
optLMApply :: Fun -> InScopeSet -> ADMode -> TExpr -> TExpr -> Maybe TExpr

optLMApply (GradFun {}) _ _ _ _ = Nothing
optLMApply (DrvFun {})  _ _ _ _ = Nothing

optLMApply _ _ adm (Assert e1 e2) dx
  = Just (Assert e1 (lmApply_AD adm e2 dx))

optLMApply _ _ adm (Let v rhs body) dx
  = Just $ Let v rhs $ lmApply_AD adm body dx

optLMApply _ _ adm (If b et ef) dx
  = Just $ If b (lmApply_AD adm et dx) (lmApply_AD adm ef dx)

-- lmApplyT  (r, lm) dx ===> lmApply  lm dx
-- lmApplyTR dr (r, lm) ===> lmApplyR dr lm
optLMApply _ _ (AD TupleAD dir) (Tuple [_, lm]) dx
  = Just (lmApply_Dir dir lm dx)

-- Called for (lmApply (lm* es) dx)
-- In BasicAD only
optLMApply _ env (AD BasicAD dir) (Call (TFun _ (Fun (PrimFun (Builtin f)))) es) dx
  = optLMApplyCall env dir f es dx

-- Looking at:   D$f(e1, e2) `lmApply` dx
--   f :: S1 S2 -> T
--   D$f :: S1 S2 -> ((S1,S2) -o T)
--   fwd$f :: S1 S2 S1_t S2_t -> T_t
optLMApply _ _ (AD adp1 Fwd) (Call (TFun (TypeLM _ t) (GradFun f adp2)) es) dx
  | adp1 == adp2
  = Just (Call grad_fun es_dx)
  where
    grad_fun = TFun (tangentType t) (DrvFun f (AD adp1 Fwd))
    es_dx = Tuple [es, dx]

-- Looking at:   dr `lmApplyR` D$f(e1, e2)
--   f :: S1 S2 -> T
--   D$f :: S1 S2 -> ((S1,S2) -o T)
--   rev$f :: S1 S2 T_ -> (S1_t,S2_t)
optLMApply _ _ (AD adp1 Rev) (Call (TFun (TypeLM s _) (GradFun f adp2)) es) dx
  | adp1 == adp2
  = Just (Call grad_fun es_dx)
  where
    grad_fun = TFun (tangentType s) (DrvFun f (AD adp1 Rev))
    es_dx = Tuple [es, dx]

{-
optLMApply (Call (TFun (TypeLM _ t) (GradFun (PrimFun f) mode)) e) dx
  = -- trace ("Prim Grad->Der [" ++ f ++ "]")
    Just $ Call (TFun (tangentType t) (DrvFun (PrimFun f) mode)) (Tuple [e, dx])
-}

optLMApply _ _ _ _e _
  = --pprTrace "Apply not optimized:" (ppr e)
    Nothing

------------------
-- Optimise (lmApply (fun arg) dx)
-- Only for the BasicAD form
optLMApplyCall :: HasCallStack
               => InScopeSet -> ADDir
               -> BuiltinFunName -> TExpr -- f args :: s -o t
               -> TExpr                   -- :: T(s)
               -> Maybe TExpr             -- :: T(t)

-- (lmZero :: s -o t) `apply` (x :: T(s))  = 0 :: T(t)
optLMApplyCall _ dir "lmZero" (Tuple [s, t]) dx
  = traceWhenTypesUnequal "Apply lmZero" in_ty (typeof dx) $
    Just (case dir of
            Fwd -> mkTangentZero t
            Rev -> mkTangentZero s)
  where
    tangent_s = tangentType (typeof s)
    tangent_t = tangentType (typeof t)
    in_ty = case dir of
               Fwd -> tangent_s
               Rev -> tangent_t

optLMApplyCall _ _ "lmOne" t dx
  = traceWhenTypesUnequal "Apply lmOne"
             (tangentType (typeof t)) (typeof dx) $
    Just dx

optLMApplyCall _ dir "lmAdd" (Tuple [f,g]) dx
  = Just (pTsAdd (lmApply_Dir dir f dx) (lmApply_Dir dir g dx))

optLMApplyCall _ Fwd "lmCompose" (Tuple [f,g]) dx = Just (lmApply f (lmApply g dx))
optLMApplyCall _ Rev "lmCompose" (Tuple [f,g]) dx = Just (lmApplyR (lmApplyR dx f) g)

optLMApplyCall _ _ "lmScale" (Tuple [_ty, x]) dx
  = Just (pTsScale x dx)

optLMApplyCall _ Fwd "lmVCat" (Tuple es) dx = do_prod Fwd es dx
optLMApplyCall _ Rev "lmVCat" (Tuple es) dx = do_sum  Rev es dx
optLMApplyCall _ Fwd "lmHCat" (Tuple es) dx = do_sum  Fwd es dx
optLMApplyCall _ Rev "lmHCat" (Tuple es) dx = do_prod Rev es dx

optLMApplyCall env Fwd "lmVCatV" e dx = do_prod_v env Fwd e dx
optLMApplyCall env Rev "lmVCatV" e dx = do_sum_v  env Rev e dx
optLMApplyCall env Fwd "lmHCatV" e dx = do_sum_v  env Fwd e dx
optLMApplyCall env Rev "lmHCatV" e dx = do_prod_v env Rev e dx

optLMApplyCall _ dir "lmFold" (Tuple [sZero, Lam i m, Lam i' m', acc, v]) dx =
  do_fold dir sZero i m i' m' acc v dx

optLMApplyCall _ _ _ _ _
  = -- pprTrace ("No opt for LM apply of " ++ show fun)
    --         (ppr arg)
    Nothing

----------------------
do_prod :: ADDir -> [TExpr] -> TExpr -> Maybe TExpr
do_prod dir es dx = Just (Tuple [lmApply_Dir dir e dx | e <- es])

do_sum :: ADDir -> [TExpr] -> TExpr -> Maybe TExpr
do_sum dir es dx
  = Just $ foldr1 pTsAdd $ zipWith (lmApply_Dir dir) es dxs
  where
    n = length es

    dxs = case dx of
            Tuple dxs -> dxs
            _ -> [ pSel i n dx | i <- [1..n] ]

do_prod_v :: InScopeSet -> ADDir -> TExpr -> TExpr -> Maybe TExpr

-- (V( build n (\i.m) ) `lmApply` dx) = build n (\i. m `lmApply` dx)
--   This special case to avoids the hard-to-optimise
--       let m = build (\j. blah)
--       in ...(build n (\i. ...m[i]...))...
do_prod_v env dir e dx
  | Just (n, i, body) <- isBuild_maybe e
  , let (binds, [vdx]) = makeAtomic True (extendInScopeSet i env) [dx]
  = Just $ mkLets binds $
    pBuild n $ Lam i $
    lmApply_Dir dir body vdx

  -- (V(m) `lmApply` dx) = build n (\i. m[i] `lmApply` dx)
  | TypeVec {} <- typeof e
  = Just $ mkLets binds $
    pBuild (pSize ve) $ Lam indexTVar $
    lmApply_Dir dir (pIndex (Var indexTVar) ve) vdx

  | otherwise = Nothing
  where
    (binds, [ve, vdx]) = makeAtomic True (extendInScopeSet indexTVar env) [e,dx]

do_sum_v :: InScopeSet -> ADDir -> TExpr -> TExpr -> Maybe TExpr
do_sum_v env dir e dx
  | Just (n, i, body) <- isBuild_maybe e
  , let (binds, [vdx]) = makeAtomic True (extendInScopeSet i env) [dx]
  = Just $ mkLets binds $
    pSumBuild n $ Lam i $
    lmApply_Dir dir body (pIndex (Var i) vdx)

  -- (H(m) `lmApply` dx) = sumbuild n (\i. m[i] `lmApply` dx[i])
  | TypeVec {} <- typeof e
  = Just $
    mkLets binds $
    pSumBuild (pSize vm) $ Lam indexTVar $
    lmApply_Dir dir (pIndex (Var indexTVar) vm)
                    (pIndex (Var indexTVar) vdx)

  | otherwise = Nothing
  where
    (binds, [vm, vdx]) = makeAtomic True (extendInScopeSet indexTVar env) [e,dx]

do_fold :: ADDir
        -> TExpr
        -> TVar -> TExpr
        -> TVar -> TExpr
        -> TExpr -> TExpr -> TExpr
        -> Maybe TExpr
do_fold Rev sZero i m i' m' acc v dx = Just (pRFold (tangentType elt_ty) sZero f f' acc v dx)
  where acc_elt_ty = typeof i'
        TypeTuple [acc_ty, elt_ty] = acc_elt_ty
        dacc_ty = tangentType acc_ty
        f = Lam i m
        f' = Lam i'_dr
             $ mkLet i' (pFst (Var i'_dr))
             $ mkLet dr (pSnd (Var i'_dr))
             $ lmApplied
          where
            lmApplied = lmApply_Dir Rev m' (Var dr)
            dr        = newVarNotIn dacc_ty m'
            i'_dr     = newVarNotIn (TypeTuple [acc_elt_ty, dacc_ty]) lmApplied

do_fold Fwd _ i m i' m' acc v ds_acc_v = Just (pFFold f acc v df dacc dv)
  where f = Lam i m
        df = Lam i'_di'
             $ mkLet i' (pFst (Var i'_di'))
             $ mkLet di' (pSnd (Var i'_di'))
             $ lmApplied
          where
            lmApplied = lmApply_Dir Fwd m' (Tuple [ds, Var di'])
            di'       = newVarNotIn (tangentType (typeof i')) (Tuple [m', ds])
            i'_di'    =
              newVarNotIn (TypeTuple [ typeof i' , tangentType (typeof i')])
                          lmApplied

        ds   = pFst ds_acc_v
        dacc = pFst (pSnd ds_acc_v)
        dv   = pSnd (pSnd ds_acc_v)
