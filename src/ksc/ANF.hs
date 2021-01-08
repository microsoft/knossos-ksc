-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances,
             PatternSynonyms,
	     ScopedTypeVariables, TypeApplications #-}

module ANF where

import Data.Void (Void, absurd)

import Ksc.Traversal( mapAccumLM, traverseState )
import Lang
import OptLet( Subst, mkEmptySubst, substBndr, substVar )
import KMonad
import Control.Monad( ap )

-- anfDefs :: (GenBndr p) => env -> [DefX p] -> KM (env, [DefX p])
anfDefs :: Monad m => env -> [TDef] -> KMT m (env, [TDef])
anfDefs = mapAccumLM anfDef

anfDef :: Monad m => env -> TDef -> KMT m (env, TDef)
anfDef env def = runAnf $ do { def' <- anfD def
                             ; return (env, def')
                             }

-----------------------------------------------
-- anfD :: (GenBndr p) => DefX p -> AnfM p (DefX p)
anfD :: Monad m => TDef -> AnfMTL l m TDef
anfD def@(Def { def_rhs = rhs
              , def_pat = pat })
  = case rhs of
      UserRhs expr ->
        do { expr' <- anfExpr (mkEmptySubst (patVars pat)) expr
           ; return (def { def_rhs = UserRhs expr' }) }

      EDefRhs{} -> return def
      GDefRhs{} -> return def
      StubRhs{} -> return def

-- anfExpr :: (GenBndr p) => ExprX p -> AnfM p (ExprX p)
anfExpr :: Monad m => Subst -> TExpr -> AnfMTL l m TExpr
anfExpr subst e = wrapLets (anfE subst e)

-- See Notes [Cloning during ANF] [ANF on tuples]
--
-- anfE :: (GenBndr p) => ExprX p -> AnfM p (ExprX p)
anfE :: Monad m => Subst -> TExpr -> AnfMT Typed m TExpr
anfE subst (Tuple es)    = Tuple <$> mapM (anfE1 subst) es
anfE _ e@(Konst _)       = return e
anfE _ e@(Dummy _)       = return e
anfE subst (Var tv)      = return (substVar subst tv)
anfE subst (Call fun es@(Tuple _))
                         = Call fun <$> anfE  subst es
anfE subst (Call fun es) = Call fun <$> anfE1 subst es
anfE subst (Let v r e)    = do { r' <- anfE subst r
                               ; let (v', subst') = traverseState substBndr v subst
                               ; emit v' r'
                               ; anfE subst' e }
anfE subst (If b t e)     = do { t' <- anfExpr subst t
                               ; e' <- anfExpr subst e
                               ; return (If b t' e') }
anfE subst (App e1 e2)    = do { f <- anfE subst e1
                               ; a <- anfE1 subst e2
                               ; return (App f a) }
anfE subst (Lam v e)      = do { e' <- anfExpr subst e
                               ; return (Lam v e') }
anfE subst (Assert e1 e2) = do { e1' <- anfE subst e1
                               ; e2' <- anfExpr subst e2
                               ; return (Assert e1' e2') }

-- anfE1 :: GenBndr p => ExprX p -> AnfM p (ExprX p)
anfE1 :: Monad m => Subst -> TExpr -> AnfMT Typed m TExpr
-- Returns an atomic expression
anfE1 subst e = do { e' <- anfE subst e
                   ; atomise e' }

-- atomise :: GenBndr p => ExprX p -> AnfM p (ExprX p)
atomise :: Monad m => TExpr -> AnfMT Typed m TExpr
atomise (Var v)   = return (Var v)
atomise (Konst k) = return (Konst k)
atomise (Lam x e) = return (Lam x e) -- Don't separate build from lambda
atomise e         = do { (b,v) <- newVar e
                       ; emit (VarPat b) e
                       ; return (Var v) }

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

{- Note [ANF on tuples]
~~~~~~~~~~~~~~~~~~~~~~~

We do not want turn to `f (x, y)` into `let a = (x, y) in f a`.  This
is particularly important when f is build, or some other function
which takes a lambda (inside a tuple).  We simply are not able (yet)
to differentiate the lambda away from its build.  But in general it
makes life harder for the optimiser if we move the tuple away from its
function.  Many of our optimisation rules match on the call of a
function on a literal tuple.
-}

data FloatDef p = FD (PatG (LetBndrX p)) (ExprX p)

instance InPhase p => Pretty (FloatDef p) where
  pprPrec _ (FD b e) = pprPatLetBndr @p b <+> char '=' <+> ppr e

newtype AnfMTL l m a = AnfM (KMT m ([l], a))

type AnfMT p = AnfMTL (FloatDef p)
type AnfM p = AnfMT p IO

runAnf :: Monad m  => AnfMTL Void m a -> KMT m a
runAnf m = do { (fs, r) <- run m
              ; case fs of
                  []  -> return r
                  x:_ -> absurd x
                 }

run :: AnfMTL l m a -> KMT m ([l], a)
run (AnfM m) = m

instance Monad m => Applicative (AnfMTL l m) where
  pure  = return
  (<*>) = ap

instance Monad m => Functor (AnfMTL l m) where
  fmap f m = do { x <- m; return (f x) }

instance Monad m => Monad (AnfMTL l m) where
  return x = AnfM (return ([], x))
  m >>= k  = AnfM $ do { (fs1, x) <- run m
                       ; (fs2, r) <- run (k x)
                       ; return (fs1 ++ fs2, r) }

wrapLets :: Monad m => AnfMT p m (ExprX p) -> AnfMTL l m (ExprX p)
wrapLets (AnfM m) = AnfM $ do { (fs, e) <- m
                              ; return ([], wrap fs e) }

wrap :: [FloatDef p] -> ExprX p -> ExprX p
wrap fs e = foldr (\(FD v r) b -> Let v r b) e fs

emit :: Monad m => PatG (LetBndrX p) -> ExprX p -> AnfMT p m ()
emit v r = AnfM (return ([FD v r], ()))

---------------------------------
{-
Note [Fresh names in ANF]
~~~~~~~~~~~~~~~~~~~~~~~~~

The ANF pass invents fresh names via a monad and a unique supply,
rather than by using OptLet.notInScope.  No particular reason for that
choice, but it comes with a danger: that the ANF pass might invent a
"fresh" name that is already in scope.

To avoid this nasty possiblity, we make the ANF names have the form
"t$23", "t$24" etc, and avoid using "t$" in any other names.  Simple,
if not very beautiful.
-}

class GenBndr p where
  mkNewVar :: Uniq -> ExprX p -> (LetBndrX p, VarX p)

instance GenBndr Parsed where
  mkNewVar u _ = (v,v)
     where
        -- See Note [Fresh names in ANF]
        v = mkVar ("t$" ++ show u)

instance GenBndr Typed where
  mkNewVar u e = (tv, tv)
    where
       -- See Note [Fresh names in ANF]
       tv = mkTVar (typeof e) ("t$" ++ show u)

newVar :: (Monad m, GenBndr p) => ExprX p -> AnfMT p m (LetBndrX p, VarX p)
newVar e = AnfM $ do { u <- getUniq; return ([], mkNewVar u e) }
