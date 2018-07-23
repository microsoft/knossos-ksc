module Opt where

import Lang
import Prim
import Text.PrettyPrint as PP
import qualified Data.Set as S

---------------
optD :: Def -> Def
optD (Def f as r) = Def f as (fst (dropDeadE (optE r)))

---------------
optE :: TExpr a -> TExpr a
optE (Tuple es)         = Tuple (map optE es)
optE (Var v)            = Var v
optE (Konst k)          = Konst k
optE (Let var rhs body) = Let var (optE rhs) (optE body)
optE (Call fun arg)     = case optCall fun opt_arg of
                            Nothing -> Call fun opt_arg
                            Just r  -> optE r
                        where
                          opt_arg = optE arg

--------------
-- 'optCall' performs one rewrite, returning (Just e) if
-- it succeeds in rewriting, and Nothing if not
--
-- The same goes for optFun, optGradFun, etc

optCall :: Fun
        -> Expr   -- Argument, already optimised
        -> Maybe Expr
optCall (LMFun lm)        arg = optLM lm arg
optCall (Fun f)           arg = optFun f arg
optCall (GradFun f False) arg = optGradFun f arg
optCall _ _ = Nothing

-----------------------
optFun :: String -> Expr -> Maybe Expr
optFun "fst" (Tuple [x,_]) = Just x
optFun "snd" (Tuple [_,y]) = Just y
optFun "+" (Tuple [x, Konst KZero]) = Just x
optFun "+" (Tuple [Konst KZero, y]) = Just y

optFun fun arg = Nothing


-----------------------
optGradFun :: String -> Expr -> Maybe Expr
optGradFun "*" (Tuple [x,y])
  = Just (lmFloat y `lmCross` lmFloat x)

-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optGradFun "+" _ = Just (lmOne `lmCross` lmOne)

-- fst :: (a,b) -> a
-- Dfst(x,y) :: (a,b) -o a
optGradFun "fst" _ = Just (lmOne  `lmCross` lmZero)
optGradFun "snd" _ = Just (lmZero `lmCross` lmOne)

optGradFun _ _ = Nothing


--------------
optLM :: String -> Expr -> Maybe Expr
optLM "lmApply" (Tuple [f,a]) = optApply f a

optLM "lmTranspose" m = optTrans m

optLM "lmCompose" (Tuple [f,g])
  | isLMOne f  = Just g
  | isLMOne g  = Just f
  | isLMZero f = Just lmZero
  | isLMZero g = Just lmZero

optLM "lmPair" (Tuple [f,g])
  | isLMOne  f, isLMOne  g = Just lmOne
  | isLMZero f, isLMZero g = Just lmZero

optLM fun arg = Nothing


---------------
optApply :: TExpr (LM a b) -> TExpr a -> Maybe (TExpr b)
optApply (Var (Grad n fr)) (Var (Delta _x))
  = -- Suspicious to ingore the 'x'
    Just $
    Var (Drv n fr)

optApply (Konst k) dx
  = error ("applyToDx " ++ show k)

optApply (Let (Simple n) rhs body) dx
  = Just $
    Let (Simple n) rhs $
    lmApply body dx

optApply (Let (Grad n fr) rhs body) dx
  = Just $
    Let (Grad n fr) rhs $
    Let (Drv n fr)  (lmApply rhs dx) $
    lmApply body dx

optApply (Call f es) dx
  = optApplyCall f es dx

optApply e dx
  = Nothing

------------------
optApplyCall :: Fun -> Expr      -- f args :: s -o t
             -> TExpr s          -- :: S
             -> Maybe (TExpr t)  -- :: T
-- Optimise (lmApply (fun arg) dx)
optApplyCall (LMFun "lmZero") _  dx = Just (Konst KZero)
optApplyCall (LMFun "lmOne")  _ dx = Just dx
optApplyCall (LMFun "lmCompose") (Tuple [f,g]) dx
  = Just (lmApply f (lmApply g dx))
optApplyCall (LMFun "lmPair") (Tuple es) dx
  = Just (Tuple [lmApply e dx | e <- es])
optApplyCall (LMFun "lmCross") (Tuple [a,b]) dx
  = Just (pAdd (lmApply a (pFst dx)) (lmApply b (pSnd dx)))
optApplyCall (LMFun "lmFloat") x dx
  = Just (pMul x dx)
optApplyCall fun arg dx
  = Nothing


----------------------
optTrans :: TExpr (LM a b) -> Maybe (TExpr (LM b a))
optTrans (Var (Grad n d)) = Just (Var (Grad n (not d)))
optTrans (Call f a)       = optTransCall f a
optTrans (Let (Grad n d) rhs body)
  = Just $ Let (Grad n (not d)) (lmTranspose rhs) $
    lmTranspose body
optTrans (Let var rhs body)
  = Just $ Let var rhs $
    lmTranspose body
optTrans e = error ("optTrans: " ++ PP.render (ppr e))

optTransCall :: Fun -> Expr -> Maybe Expr
optTransCall (LMFun "lmZero") _  = Just lmZero
optTransCall (LMFun "lmOne")  _  = Just lmOne
optTransCall (LMFun "lmFloat") e = Just (lmFloat e)

optTransCall (LMFun "lmTranspose") e = Just e
optTransCall (LMFun "lmCompose") (Tuple [f,g])
  = Just (lmCompose (lmTranspose g) (lmTranspose f))
optTransCall (LMFun "lmPair") (Tuple [a,b])
  = Just (lmCross (lmTranspose a) (lmTranspose b))
optTransCall (LMFun "lmCross") (Tuple [a,b])
  = Just (lmPair (lmTranspose a) (lmTranspose b))

optTransCall f a = Nothing

----------------------
-- Dead code elimination
----------------------

dropDeadD :: Def -> Def
dropDeadD (Def f as rhs) = Def f as (fst (dropDeadE rhs))

dropDeadE :: Expr -> (Expr, S.Set Var)
dropDeadE (Var v) = (Var v, S.singleton v)
dropDeadE (Konst k) = (Konst k, S.empty)
dropDeadE (Call f e) = (Call f e', vs)
                     where
                       (e',vs) = dropDeadE e
dropDeadE (Tuple es) = (Tuple es', S.unions vs)
                     where
                       (es', vs) = unzip (map dropDeadE es)
dropDeadE (Let var rhs body)
  | var `S.member` vsb
  = (Let var rhs' body', vs)
  | otherwise
  = (body', vsb)
  where
    (rhs',  vsr) = dropDeadE rhs
    (body', vsb) = dropDeadE body
    vs = (var `S.delete` vsb) `S.union` vsr
