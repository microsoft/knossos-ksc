{-# LANGUAGE FlexibleInstances #-}

module ANF where

import Lang
import KMonad
import Control.Monad( ap )

anfDefs :: (CmkVar b, Typeable b, TypeableFun f) => [DefX f b] -> KM [DefX f b]
anfDefs defs
  = do { u <- getUniq
       ; let (u', defs') = runAnf u (mapM anfD defs)
       ; setUniq u'
       ; return defs' }

-----------------------------------------------
anfD :: (CmkVar b, Typeable b, TypeableFun f) => DefX f b -> AnfM f b (DefX f b)
anfD (DefX fun args rhs) = DefX fun args <$> anfExpr rhs

anfExpr :: (CmkVar b, Typeable b, TypeableFun f) => ExprX f b -> AnfM f b (ExprX f b)
anfExpr e = wrapLets (anfE e)

anfE :: (CmkVar b, Typeable b, TypeableFun f) => ExprX f b -> AnfM f b (ExprX f b)
anfE (Tuple es)            = Tuple <$> mapM anfE1 es
anfE (Call fun (Tuple es)) {- FIXME anf build
  | Fun (SFun "build") <- fun
  , [n,e] <- es
  = -- Don't bother to ANF the first arr
    -- and leave the second arg in place
    do { e' <- anfE e
       ; return (Call fun (Tuple [n, e'])) }
  | otherwise -}
  = (Call fun . Tuple) <$> mapM anfE1 es
anfE (Konst k)      = return (Konst k)
anfE (Var v)        = return (Var v)
anfE (Call fun e)   = Call fun <$> anfE1 e
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

anfE1 :: (CmkVar b, Typeable b, TypeableFun f) => (ExprX f b) -> AnfM f b (ExprX f b)
-- Returns an atomic expression
anfE1 e = do { e' <- anfE e
             ; atomise e' }

atomise :: (CmkVar b, Typeable b, TypeableFun f) => (ExprX f b) -> AnfM f b (ExprX f b)
atomise (Var v)   = return (Var v)
atomise (Konst k) = return (Konst k)
atomise e         = do { v <- newVar (typeof e)
                       ; emit v e
                       ; return (Var v) }

type FloatDef f b = (b, ExprX f b)

runAnf :: Uniq -> AnfM f b a -> (Uniq, a)
runAnf u (AnfM f1) = case f1 u of (u', _, r) -> (u', r)

newtype AnfM f b a = AnfM (Uniq -> (Uniq, [FloatDef f b], a))

instance Applicative (AnfM f b) where
  pure  = return
  (<*>) = ap

instance Functor (AnfM f b) where
  fmap f m = do { x <- m; return (f x) }

instance Monad (AnfM f b) where
  return x = AnfM (\u -> (u, [], x))
  AnfM m1 >>= k  = AnfM $ \u ->
                   case m1 u  of { (u1, fs1, x) ->
                   case k x   of { AnfM m2 ->
                   case m2 u1 of { (u2, fs2, r) ->
                   (u2, fs1 ++ fs2, r) } } }

wrapLets :: AnfM f b (ExprX f b) -> AnfM f b (ExprX f b)
wrapLets (AnfM f1) = AnfM $ \u ->
                    case f1 u of
                       (u', fs, e) -> (u', [], wrap fs e)

wrap :: [FloatDef f b] -> ExprX f b -> ExprX f b
wrap fs e = foldr (\(v,r) b -> Let v r b) e fs

emit :: b -> ExprX f b -> AnfM f b ()
emit v r = AnfM (\u -> (u, [(v,r)], ()))

newVar :: CmkVar b => Type -> AnfM f b b
newVar ty = AnfM (\u -> (u+1, [], mkVar ty ('t' : show u)))
