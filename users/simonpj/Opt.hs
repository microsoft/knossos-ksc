module Opt where

import Lang
import Prim
import Text.PrettyPrint as PP
import qualified Data.Set as S

optD :: Def -> Def
optD (Def f as r) = Def f as (fst (dropDeadE (optE r)))

---------------
optE :: TExpr a -> TExpr a
optE (Tuple es) = Tuple (map optE es)
optE (Var v)    = Var v
optE (Konst k)  = Konst k
optE (Let var rhs body) = Let var (optE rhs) (optE body)
optE (Call fun args)    = optCall fun (optE args)

--------------
optCall :: Fun
        -> Expr   -- Argument, already optimised
        -> Expr
optCall (LMFun "lmApply") (Tuple [f,a])
  = optApply f a

optCall (LMFun "lmTranspose") m
  = optTrans m
  
optCall (GradFun "*" False) (Tuple [x,y])
  = lmFloat y `lmCross` lmFloat x
  
optCall (GradFun "+" False) (Tuple [_,_])
  = lmOne `lmCross` lmOne

optCall fun args
  = Call fun args
  
---------------
optApply :: TExpr (LM a b) -> TExpr a -> TExpr b
optApply (Var (Grad n fr)) (Var (Delta _x))
  = -- Suspicous to ingore the 'x'
    Var (Drv n fr)

optApply (Konst k) dx
  = error ("applyToDx " ++ show k)

optApply (Let (Simple n) rhs body) dx
  = Let (Simple n) (optE rhs) $
    optApply body dx

optApply (Let (Grad n fr) rhs body) dx
  = Let (Grad n fr) (optE rhs) $
    Let (Drv n fr)  (optApply rhs dx) $
    optApply body dx

optApply (Call f es) dx
  = optApplyCall f es dx

optApply e dx
  = lmApply e dx
  
------------------
optApplyCall :: Fun -> Expr -- f args :: s -o t
             -> TExpr s     -- :: S 
             -> TExpr t     -- :: T
-- Optimise (lmApply (fun arg) dx)
optApplyCall (LMFun "lmZero") _  dx = Konst KZero
optApplyCall (LMFun "lmOne")  _ dx = dx
optApplyCall (LMFun "lmCompose") (Tuple [f,g]) dx
  = optApply f (optApply g dx)
optApplyCall (LMFun "lmPair") (Tuple es) dx
  = Tuple [optApply e dx | e <- es]
optApplyCall (LMFun "lmCross") (Tuple [a,b]) (Tuple [dx1,dx2])
  = Call (Fun "+") (Tuple [optApply a dx1, optApply b dx2])
optApplyCall (LMFun "lmFloat") x dx
  = Call (Fun "*") (Tuple [x,dx])

optApplyCall fun arg dx
  = lmApply (Call fun arg) dx


----------------------
optTrans :: TExpr (LM a b) -> TExpr (LM b a)
optTrans (Var (Grad n d)) = Var (Grad n (not d))
optTrans (Call f a)     = optTransCall f a
optTrans (Let (Grad n d) rhs body)
  = Let (Grad n (not d)) (optTrans rhs) $
    optTrans body
optTrans (Let var rhs body)
  = Let var rhs $
    optTrans body
optTrans e = error ("optTrans: " ++ PP.render (ppr e))

optTransCall :: Fun -> Expr -> Expr
optTransCall (LMFun "lmZero") _  = lmZero
optTransCall (LMFun "lmOne")  _  = lmOne
optTransCall (LMFun "lmFloat") e = lmFloat e

optTransCall (LMFun "lmTranspose") e = e
optTransCall (LMFun "lmCompose") (Tuple [f,g])
  = lmCompose (optTrans g) (optTrans f)
optTransCall (LMFun "lmPair") (Tuple [a,b])
  = lmCross (optTrans a) (optTrans b)
optTransCall (LMFun "lmCross") (Tuple [a,b])
  = lmPair (optTrans a) (optTrans b)
  
optTransCall f a = lmTranspose (Call f a)

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
