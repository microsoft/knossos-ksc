-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances, LambdaCase,
             PatternSynonyms, StandaloneDeriving, AllowAmbiguousTypes,
	     ScopedTypeVariables, TypeApplications #-}

module ANF where

import Lang
import Prim( isThePrimFun )
import KMonad
import Control.Monad( ap )

-- anfDefs :: (GenBndr p) => [DefX p] -> KM [DefX p]
anfDefs :: [TDef] -> KM [TDef]
anfDefs defs = runAnf $
               mapM anfD defs

-----------------------------------------------
-- anfD :: (GenBndr p) => DefX p -> AnfM p (DefX p)
anfD :: TDef -> AnfM Typed TDef
anfD def@(Def { def_rhs = rhs })
  | UserRhs expr <- rhs
  = do { expr' <- anfExpr expr
       ; return (def { def_rhs = UserRhs expr' }) }

  | otherwise   -- EDefRhs, StubRhs
  = return def

-- anfExpr :: (GenBndr p) => ExprX p -> AnfM p (ExprX p)
anfExpr :: TExpr -> AnfM Typed TExpr
anfExpr e = wrapLets (anfE e)

-- anfE :: (GenBndr p) => ExprX p -> AnfM p (ExprX p)
anfE :: TExpr -> AnfM Typed TExpr
anfE (Tuple es)     = Tuple <$> mapM anfE1 es
anfE (Konst k)      = return (Konst k)
anfE (Var v)        = return (Var v)
anfE (Call fun es)
 | fun `isThePrimFun` "build"   -- See Note [Do not ANF first arg of build]
 , [e1,e2] <- es
 = do { e2' <- anfE1 e2
      ; return (Call fun [e1, e2']) }
 | otherwise
 = Call fun <$> mapM anfE1 es
anfE (Let v r e)    = do { r' <- anfE r
                         ; emit v r'
                         ; anfE e }
anfE (If b t e)     = do { t' <- anfExpr t
                         ; e' <- anfExpr e
                         ; return (If b t' e') }
anfE (App e1 e2)    = do { f <- anfE e1
                         ; a <- anfE1 e2
                         ; return (App f a) }
anfE (Lam v e)      = do { e' <- anfExpr e
                         ; return (Lam v e') }
anfE (Assert e1 e2) = do { e1' <- anfE e1
                         ; e2' <- anfExpr e2
                         ; return (Assert e1' e2') }

-- anfE1 :: GenBndr p => ExprX p -> AnfM p (ExprX p)
anfE1 :: TExpr -> AnfM Typed TExpr
-- Returns an atomic expression
anfE1 e = do { e' <- anfE e
             ; atomise e' }

-- atomise :: GenBndr p => ExprX p -> AnfM p (ExprX p)
atomise :: TExpr -> AnfM Typed TExpr
atomise (Var v)   = return (Var v)
atomise (Konst k) = return (Konst k)
atomise (Lam x e) = return (Lam x e) -- Don't separate build from lambda
atomise e         = do { (b,v) <- newVar e
                       ; emit b e
                       ; return (Var v) }

{- Note [Do not ANF first arg of build]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not want to transform
   build (2*n) blah  ::: Vec (2*n) Float
into
   (let t1 = 2*n in build t1 blah) :: Vec t1 Float
because the type makes no sense.

An alternative would be to substitute for t1, in typeofExpr;
and similarly in the type checker.  In some ways that would
be nicer, but I have not tried it.
-}


data FloatDef p = FD (LetBndrX p) (ExprX p)

instance InPhase p => Pretty (FloatDef p) where
  pprPrec _ (FD b e) = pprLetBndr @p b <+> char '=' <+> ppr e

newtype AnfM p a = AnfM (KM ([FloatDef p], a))

runAnf :: InPhase p => AnfM p a -> KM a
runAnf m = do { (fs, r) <- run m
              ; assert (text "runANF" <+> ppr fs) (null fs) $
                return r }

run :: AnfM p a -> KM ([FloatDef p], a)
run (AnfM m) = m

instance Applicative (AnfM p) where
  pure  = return
  (<*>) = ap

instance Functor (AnfM p) where
  fmap f m = do { x <- m; return (f x) }

instance Monad (AnfM p) where
  return x = AnfM (return ([], x))
  m >>= k  = AnfM $ do { (fs1, x) <- run m
                       ; (fs2, r) <- run (k x)
                       ; return (fs1 ++ fs2, r) }

wrapLets :: AnfM p (ExprX p) -> AnfM p (ExprX p)
wrapLets (AnfM m) = AnfM $ do { (fs, e) <- m
                              ; return ([], wrap fs e) }

wrap :: [FloatDef p] -> ExprX p -> ExprX p
wrap fs e = foldr (\(FD v r) b -> Let v r b) e fs

emit :: LetBndrX p -> ExprX p -> AnfM p ()
emit v r = AnfM (return ([FD v r], ()))

---------------------------------
class GenBndr p where
  mkNewVar :: Uniq -> ExprX p -> (LetBndrX p, VarX p)

instance GenBndr Parsed where
  mkNewVar u _ = (v,v)
     where
        v = mkVar ("t" ++ show u)

instance GenBndr Typed where
  mkNewVar u e = (tv, tv)
    where
       tv = mkTVar (typeof e) ("t" ++ show u)

newVar :: GenBndr p => ExprX p -> AnfM p (LetBndrX p, VarX p)
newVar e = AnfM $ do { u <- getUniq; return ([], mkNewVar u e) }


