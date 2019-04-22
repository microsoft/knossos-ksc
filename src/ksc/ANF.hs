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
anfDefs defs
  = do { u <- getUniq
       ; let (u', defs') = runAnf u (mapM anfD defs)
       ; setUniq u'
       ; return defs' }

-----------------------------------------------
-- anfD :: (GenBndr p) => DefX p -> AnfM p (DefX p)
anfD :: TDef -> AnfM Typed TDef
anfD def@(Def { def_rhs = rhs })
  = do { rhs' <- anfExpr rhs
       ; return (def { def_rhs = rhs' }) }

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


type FloatDef p = (LetBndrX p, ExprX p)

runAnf :: Uniq -> AnfM p a -> (Uniq, a)
runAnf u (AnfM f1) = case f1 u of (u', _, r) -> (u', r)

newtype AnfM p a = AnfM (Uniq -> (Uniq, [FloatDef p], a))

instance Applicative (AnfM p) where
  pure  = return
  (<*>) = ap

instance Functor (AnfM p) where
  fmap f m = do { x <- m; return (f x) }

instance Monad (AnfM p) where
  return x = AnfM (\u -> (u, [], x))
  AnfM m1 >>= k  = AnfM $ \u ->
                   case m1 u  of { (u1, fs1, x) ->
                   case k x   of { AnfM m2 ->
                   case m2 u1 of { (u2, fs2, r) ->
                   (u2, fs1 ++ fs2, r) } } }

wrapLets :: AnfM p (ExprX p) -> AnfM p (ExprX p)
wrapLets (AnfM f1) = AnfM $ \u ->
                    case f1 u of
                       (u', fs, e) -> (u', [], wrap fs e)

wrap :: [FloatDef p] -> ExprX p -> ExprX p
wrap fs e = foldr (\(v,r) b -> Let v r b) e fs

emit :: LetBndrX p -> ExprX p -> AnfM p ()
emit v r = AnfM (\u -> (u, [(v,r)], ()))

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
newVar e = AnfM (\u -> (u+1, [], mkNewVar u e))


