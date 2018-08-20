module Opt where

import Lang
import Prim
import Text.PrettyPrint as PP
import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace

---------------
optD :: Def -> Def
optD (Def f as r) = Def f as (simplify r)

simplify :: Expr -> Expr
simplify = dropDeadE . substE . optE . substE
  -- Note the extra substE, which gets simple things,
  -- notably lmOne, to their use sites

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
optCall (LMFun lm)      arg = optLM lm arg
optCall (Fun f)         arg = optFun f arg
optCall (GradFun f Fwd) arg = optGradFun f arg
optCall _ _ = Nothing

-----------------------
optFun :: FunId -> Expr -> Maybe Expr
optFun (SelFun i _) (Tuple es)
  | i <= length es = Just (es !! (i-1))
optFun (SFun "+") (Tuple [x, Konst KZero]) = Just x
optFun (SFun "+") (Tuple [Konst KZero, y]) = Just y
optFun (SFun "*") (Tuple [x, Konst KZero]) = Just (Konst KZero)
optFun (SFun "*") (Tuple [Konst KZero, y]) = Just (Konst KZero)

optFun fun arg = Nothing


-----------------------
optGradFun :: FunId -> Expr -> Maybe Expr
optGradFun (SFun "*") (Tuple [x,y])
  = Just (lmCross [lmScalar y, lmScalar x])

optGradFun (SFun "/") (Tuple [x,y])
  = Just (lmCross [ lmScalar (pDiv (kInt 1) y)
                  , lmScalar (pNeg (pDiv x (pMul y y)))])

-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optGradFun (SFun "+") _ = Just (lmCross [lmOne, lmOne])

-- fst :: (a,b) -> a
-- Dfst(x,y) :: (a,b) -o a
optGradFun (SelFun i n) _ = Just (lmCross [ if i == j then lmOne else lmZero
                                          | j <- [1..n] ])

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

  -- Scalar(x) . Scalar(y) = Scalar( xy )
  | Call (LMFun "lmScalar") x <- f
  , Call (LMFun "lmScalar") y <- g
  = Just (lmScalar (pMul x y))

  -- (f . g) . h   =>   f . (g . h)
  | Call (LMFun "lmCompose") (Tuple [p1,p2]) <- f
  = optLM "lmCompose" (Tuple [p1, lmCompose p2 g])

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call (LMFun "lmCross") (Tuple qs) <- g
  = Just (lmCross (map (lmCompose f) qs))

  | Call (LMFun "lmCross") (Tuple ps) <- f
  , Call (LMFun "lmPair")  (Tuple qs) <- g
  , length ps == length qs
  = Just (lmAdds (zipWith lmCompose ps qs))

optLM "lmPair" (Tuple es)
  | all isLMZero es = Just lmZero

optLM "lmAdd" (Tuple [p,q])
  | isLMZero p = Just q
  | isLMZero q = Just p

  | Call (LMFun "lmScalar") x <- p
  , Call (LMFun "lmScalar") y <- p
  = Just (lmScalar (pAdd x y))

optLM fun arg = Nothing


---------------
optApply :: TExpr (LM a b) -> TExpr a -> Maybe (TExpr b)
optApply (Var (Grad n Fwd)) _
  = -- Suspicious to ignore the argument!!
    -- Correct only for forward
    Just $
    Var (Drv n Fwd)

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
optApplyCall (LMFun "lmAdd")  (Tuple [f,g]) dx
  = Just (pAdd (lmApply f dx) (lmApply g dx))
optApplyCall (LMFun "lmCompose") (Tuple [f,g]) dx
  = Just (lmApply f (lmApply g dx))

optApplyCall (LMFun "lmPair") (Tuple es) dx
  = Just (Tuple [lmApply e dx | e <- es])

optApplyCall (LMFun "lmCross") (Tuple es) dx
  = Just (foldr add (Konst KZero) (es `zip` [1..]))
  where
    n = length es

    add :: (Expr, Int) -> Expr -> Expr
    add (e,i) z = pAdd (lmApply e (pSel i n dx)) z

optApplyCall (LMFun "lmScalar") x dx
  = Just (pMul x dx)
optApplyCall fun arg dx
  = Nothing


----------------------
optTrans :: TExpr (LM a b) -> Maybe (TExpr (LM b a))
optTrans (Var (Grad n d)) = Just (Var (Grad n (flipMode d)))
optTrans (Call f a)       = optTransCall f a
optTrans (Let (Grad n d) rhs body)
  = Just $ Let (Grad n (flipMode d)) (lmTranspose rhs) $
    lmTranspose body
optTrans (Let var rhs body)
  = Just $ Let var rhs $
    lmTranspose body
optTrans e = error ("optTrans: " ++ PP.render (ppr e))

optTransCall :: Fun -> Expr -> Maybe Expr
optTransCall (LMFun "lmZero") _  = Just lmZero
optTransCall (LMFun "lmOne")  _  = Just lmOne
optTransCall (LMFun "lmScalar") e = Just (lmScalar e)

optTransCall (LMFun "lmTranspose") e = Just e
optTransCall (LMFun "lmCompose") (Tuple [f,g])
  = Just (lmCompose (lmTranspose g) (lmTranspose f))
optTransCall (LMFun "lmPair") (Tuple es)
  = Just (lmCross (map lmTranspose es))
optTransCall (LMFun "lmCross") (Tuple es)
  = Just (lmPair (map lmTranspose es))

optTransCall f a = Nothing

----------------------
-- Dead code elimination
----------------------

dropDeadD :: Def -> Def
dropDeadD (Def f as rhs) = Def f as (dropDeadE rhs)

dropDeadE :: Expr -> Expr
dropDeadE e = fst (dropDeadE' e)

dropDeadE' :: Expr -> (Expr, S.Set Var)
dropDeadE' (Var v) = (Var v, S.singleton v)
dropDeadE' (Konst k) = (Konst k, S.empty)
dropDeadE' (Call f e) = (Call f e', vs)
                      where
                        (e',vs) = dropDeadE' e
dropDeadE' (Tuple es) = (Tuple es', S.unions vs)
                      where
                        (es', vs) = unzip (map dropDeadE' es)
dropDeadE' (Let var rhs body)
  | var `S.member` vsb
  = (Let var rhs' body', vs)
  | otherwise
  = (body', vsb)
  where
    (rhs',  vsr) = dropDeadE' rhs
    (body', vsb) = dropDeadE' body
    vs = (var `S.delete` vsb) `S.union` vsr


-------------------------
-- Substitute trivials
-------------------------
substE :: Expr -> Expr
substE e = go M.empty e
  where
    go :: M.Map Var Expr -> Expr -> Expr
    go subst (Let v r b)
      | isTrivial r' = go (M.insert v r' subst) b
      | otherwise    = Let v r' (go subst b)
      where
        r' = go subst r
    go subst (Var v)
      = case M.lookup v subst of
          Just e  -> e
          Nothing -> Var v
    go subst (Konst k) = Konst k
    go subst (Call f e) = Call f (go subst e)
    go subst (Tuple es) = Tuple (map (go subst) es)

isTrivial :: Expr -> Bool
isTrivial (Tuple [])          = True
isTrivial (Var {})            = True
isTrivial (Konst {})          = True
isTrivial (Call _ (Tuple [])) = True
isTrivial e = False
