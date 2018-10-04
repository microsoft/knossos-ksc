module Opt where

import Lang
import Prim
import OptLet
import CSE
import Text.PrettyPrint as PP
import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace

---------------
optD :: Def -> Def
optD (Def f as r) = Def f as (simplify r)

simplify :: Expr -> Expr
simplify = optLets . optE . optLets
  -- Note the extra optLets, which gets simple things,
  -- notably lmOne, to their use sites

---------------
optE :: TExpr a -> TExpr a
optE (Tuple es)         = Tuple (map optE es)
optE (Var v)            = Var v
optE (Konst k)          = Konst k
optE (Lam v e)          = Lam v (optE e)
optE (App e1 e2)        = optApp (optE e1) (optE e2)
optE (Let var rhs body) = Let var (optE rhs) (optE body)
optE (If b t e)         = optIf (optE b) (optE t) (optE e)
optE (Call fun arg)     = case optCall fun opt_arg of
                            Nothing -> Call fun opt_arg
                            Just r  -> optE r
                        where
                          opt_arg = optE arg

--------------
optApp :: TExpr (a->b) -> TExpr a -> TExpr b
optApp (Lam v e) a = optE (substE (M.singleton v a) e)
                     -- Or, better? let v = a in e
optApp f a         = App f a

--------------
optIf :: TExpr Bool -> TExpr r -> TExpr r -> TExpr r
optIf (Konst (KBool True))  t e = t
optIf (Konst (KBool False)) t e = e
optIf b                     t e = If b t e

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

-- x+0 = 0+x = x
optFun (SFun "+") (Tuple [x, Konst KZero]) = Just x
optFun (SFun "+") (Tuple [Konst KZero, y]) = Just y

-- (a1,a2) + (b1,b2) = (a1+a2, b1+b2)
optFun (SFun "+") (Tuple [Tuple es1, Tuple es2])
  | length es1 == length es2 = Just (Tuple (zipWith pAdd es1 es2))

-- x*0 = 0*x = 0
optFun (SFun "*") (Tuple [x, Konst KZero]) = Just (Konst KZero)
optFun (SFun "*") (Tuple [Konst KZero, y]) = Just (Konst KZero)

-- size (build (n, _)) = n
optFun (SFun "size") (Call (Fun (SFun "build")) (Tuple [n,_]))
  = Just n

-- index j (build n f) = f j
optFun (SFun "index") (Tuple [ ei, Call (Fun (SFun "build")) (Tuple [_, f]) ])
  = Just (App f ei)

-- sum (build n (\i. if (i==e) then v else 0)
--  = v[i := e]
optFun (SFun "sum") (Call (Fun (SFun "build")) (Tuple [_,bb]))
  | Lam i (If (Call (Fun (SFun "==")) (Tuple [Var i2, ej]))
              v
              (Konst KZero)) <- bb
  , i == i2
  , i `notFreeIn` ej
  = Just (Let i ej v)

-- sum (build n (\i. (e1,e2)))
--  = (sum (build n (\i.e1)), sum (build n (\i.e2)))
optFun (SFun "sum") (Call (Fun (SFun "build")) (Tuple [n, Lam i (Tuple es)]))
   = Just (Tuple (map (\e -> pSum (pBuild n (Lam i e))) es))

optFun fun arg = Nothing


-----------------------
optGradFun :: FunId -> Expr -> Maybe Expr
-- Inline the definitions for grad(+), gran(*) etc

-- (+) :: (F,F) -> f
-- (D+)(x,y) :: (F,F) -o F
optGradFun (SFun "+") _ = Just (lmHCat [lmOne, lmOne])

optGradFun (SFun "*") (Tuple [x,y])
  = Just (lmHCat [lmScale y, lmScale x])

optGradFun (SFun "/") (Tuple [x,y])
  = Just (lmHCat [ lmScale (pDiv (kInt 1) y)
                 , lmScale (pNeg (pDiv x (pMul y y)))])
-- fst :: (a,b) -> a
-- Dfst(x,y) :: (a,b) -o a
optGradFun (SelFun i n) _ = Just (lmHCat [ if i == j then lmOne else lmZero
                                          | j <- [1..n] ])

optGradFun (SFun "sum") e
  = Just (lmBuildT (pSize e) (Lam (Simple "i") lmOne))

optGradFun (SFun "index") (Tuple [i,v])
  = Just (lmHCat [ lmZero
                 , lmBuildT (pSize v) (Lam (Simple "j") (lmDelta (Var (Simple "j")) i)) ])

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
  | Call (LMFun "lmScale") x <- f
  , Call (LMFun "lmScale") y <- g
  = Just (lmScale (pMul x y))

  -- (f . g) . h   =>   f . (g . h)
  | Call (LMFun "lmCompose") (Tuple [p1,p2]) <- f
  = optLM "lmCompose" (Tuple [p1, lmCompose p2 g])

  -- f . (g x h)   =>  (f . g) x (f . h)
  -- This duplicates f; we might want to take care
  | Call (LMFun "lmHCat") (Tuple qs) <- g
  = Just (lmHCat (map (lmCompose f) qs))

  | Call (LMFun "lmHCat") (Tuple ps) <- f
  , Call (LMFun "lmVCat")  (Tuple qs) <- g
  , length ps == length qs
  = Just (lmAdds (zipWith lmCompose ps qs))

optLM "lmVCat" (Tuple es)
  | all isLMZero es = Just lmZero

optLM "lmAdd" (Tuple [p,q])
  | isLMZero p = Just q
  | isLMZero q = Just p

  | Call (LMFun "lmScale") x <- p
  , Call (LMFun "lmScale") y <- p
  = Just (lmScale (pAdd x y))

optLM fun arg = Nothing


---------------
optApply :: TExpr (LM a b) -> TExpr a -> Maybe (TExpr b)
optApply (Call f es) dx
  = optApplyCall f es dx

optApply (Let v rhs body) dx
  = Just $
    Let v rhs $
    lmApply body dx

optApply (If b t e) dx
  = Just $ If b (lmApply t dx) (lmApply e dx)

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

optApplyCall (LMFun "lmVCat") (Tuple es) dx
  = Just (Tuple [lmApply e dx | e <- es])

optApplyCall (LMFun "lmHCat") (Tuple es) dx
  = Just (foldr add (Konst KZero) (es `zip` [1..]))
  where
    n = length es

    add :: (Expr, Int) -> Expr -> Expr
    add (e,i) z = pAdd (lmApply e (pSel i n dx)) z

optApplyCall (LMFun "lmScale") x dx
  = Just (pMul x dx)

optApplyCall (LMFun "lmBuild") (Tuple [n, Lam i m]) dx
  = Just (pBuild n (Lam i (lmApply m dx)))

optApplyCall (LMFun "lmBuildT") (Tuple [n, Lam i m]) dx
  = Just (pSum (pBuild n (Lam i (lmApply m (pIndex (Var i) dx)))))

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
optTrans (If b t e)
  = Just $ If b (lmTranspose t) (lmTranspose e)
optTrans e = error ("optTrans: " ++ PP.render (ppr e))

optTransCall :: Fun -> Expr -> Maybe Expr
optTransCall (LMFun "lmZero") _  = Just lmZero
optTransCall (LMFun "lmOne")  _  = Just lmOne
optTransCall (LMFun "lmScale") e = Just (lmScale e)

optTransCall (LMFun "lmTranspose") e = Just e
optTransCall (LMFun "lmCompose") (Tuple [f,g])
  = Just (lmCompose (lmTranspose g) (lmTranspose f))
optTransCall (LMFun "lmAdd") (Tuple [f,g])
  = Just (lmAdd (lmTranspose f) (lmTranspose g))
optTransCall (LMFun "lmVCat") (Tuple es)
  = Just (lmHCat (map lmTranspose es))
optTransCall (LMFun "lmHCat") (Tuple es)
  = Just (lmVCat (map lmTranspose es))
optTransCall (LMFun "lmBuild") (Tuple [n, Lam i b])
  = Just (lmBuildT n (Lam i (lmTranspose b)))
optTransCall (LMFun "lmBuildT") (Tuple [n, Lam i b])
  = Just (lmBuild n (Lam i (lmTranspose b)))

optTransCall f a = Nothing

