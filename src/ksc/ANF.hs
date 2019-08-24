-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances, LambdaCase,
             PatternSynonyms, StandaloneDeriving, AllowAmbiguousTypes,
	     ScopedTypeVariables, TypeApplications #-}

module ANF where

import Lang
import OptLet( Subst, mkEmptySubst, substBndr, substVar )
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
anfD def@(Def { def_rhs  = rhs
              , def_args = args })
  = case rhs of
      UserRhs expr ->
        do { expr' <- anfExpr (mkEmptySubst args) expr
           ; return (def { def_rhs = UserRhs expr' }) }

      EDefRhs{} -> return def
      StubRhs{} -> return def

-- anfExpr :: (GenBndr p) => ExprX p -> AnfM p (ExprX p)
anfExpr :: Subst -> TExpr -> AnfM Typed TExpr
anfExpr subst e = wrapLets (anfE subst e)

-- See Note [Cloning during ANF]
--
-- anfE :: (GenBndr p) => ExprX p -> AnfM p (ExprX p)
anfE :: Subst -> TExpr -> AnfM Typed TExpr
anfE subst (Tuple es)    = Tuple <$> mapM (anfE1 subst) es
anfE _ (Konst k)         = return (Konst k)
anfE subst (Var tv)      = return (substVar subst tv)
anfE subst (Call fun es)
 | fun `isThePrimFun` "build"   -- See Note [Do not ANF first arg of build]
 , [e1,e2] <- es
 = atomise =<<
   do { e2' <- anfE1 subst e2
      ; return (Call fun [e1, e2']) }
 | otherwise
 = atomise =<< Call fun <$> mapM (anfE1 subst) es
anfE subst (Let v r e)    = do { r' <- anfE subst r
                               ; let (v', subst') = substBndr v subst
                               ; emit v' r'
                               ; anfE subst' e }
anfE _ubst (Dup{})        = error "anfE Dup unimplemented"
anfE subst (If b t e)     = do { t' <- anfExpr subst t
                               ; e' <- anfExpr subst e
                               ; return (If b t' e') }
anfE subst (App e1 e2)    = do { f <- anfE subst e1
                               ; a <- anfE1 subst e2
                               ; return (App f a) }
anfE subst (Lam v e)      = do { e' <- anfExpr subst e
                               ; return (Lam v e') }
anfE subst (Assert e1 e2) = atomise =<<
                            do { e1' <- anfE subst e1
                               ; e2' <- anfExpr subst e2
                               ; return (Assert e1' e2') }

-- anfE1 :: GenBndr p => ExprX p -> AnfM p (ExprX p)
anfE1 :: Subst -> TExpr -> AnfM Typed TExpr
-- Returns an atomic expression
anfE1 subst e = do { e' <- anfE subst e
                   ; atomise e' }

-- atomise :: GenBndr p => ExprX p -> AnfM p (ExprX p)
atomise :: TExpr -> AnfM Typed TExpr
atomise (Var v)   = return (Var v)
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

{- Note [Cloning during ANF]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    let v = let r = <rhs1> in <body1>
    in <body2>

The ANF pass will float out that r-binding to
   let r = <rhs1> in
   let v = <body1> in
   <body2>

But that risks shadowing free occurrences of 'f' in <body2>.

Solution: clone any binder that is already in scope, so that the
result has no shadowing.  That is the (sole) reason that ANF carries a
substitution.
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


