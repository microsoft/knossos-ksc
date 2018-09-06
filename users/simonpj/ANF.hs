module ANF where

import Lang
import Control.Monad( ap )

anfD :: Uniq -> Def -> (Uniq, Def)
anfD u (Def fun args rhs)
  = runAnf u (Def fun args <$> wrapLets (anfE rhs))

anfE :: Expr -> AnfM Expr
anfE (Tuple es)            = Tuple <$> mapM anfE1 es
anfE (Call fun (Tuple es)) = (Call fun . Tuple) <$> mapM anfE1 es
anfE (Call fun e)          = Call fun <$> anfE1 e
anfE (Konst k)     = return (Konst k)
anfE (Var v)       = return (Var v)
anfE (Let v r e)   = do { r' <- anfE r
                        ; emit v r'
                        ; anfE e }

anfE1 :: Expr -> AnfM Expr
-- Returns an atomic expression
anfE1 e = do { e' <- anfE e
             ; atomise e' }

atomise :: Expr -> AnfM Expr
atomise (Var v)   = return (Var v)
atomise (Konst k) = return (Konst k)
atomise e         = do { v <- newVar
                       ; emit v e
                       ; return (Var v) }

type Uniq = Int

initialUniq :: Uniq
initialUniq = 1

type FloatDef = (Var, Expr)

runAnf :: Uniq -> AnfM a -> (Uniq, a)
runAnf u (AnfM f) = case f u of (u', _, r) -> (u', r)
                    
newtype AnfM a = AnfM (Uniq -> (Uniq, [FloatDef], a))

instance Applicative AnfM where
  pure  = return
  (<*>) = ap
  
instance Functor AnfM where
  fmap f m = do { x <- m; return (f x) }

instance Monad AnfM where
  return x = AnfM (\u -> (u, [], x))
  AnfM m1 >>= k  = AnfM $ \u ->
                   case m1 u  of { (u1, fs1, x) ->
                   case k x   of { AnfM m2 ->
                   case m2 u1 of { (u2, fs2, r) ->
                   (u2, fs1 ++ fs2, r) } } }
  
wrapLets :: AnfM Expr -> AnfM Expr
wrapLets (AnfM f) = AnfM $ \u ->
                    case f u of
                       (u', fs, e) -> (u', [], wrap fs e)

wrap :: [FloatDef] -> Expr -> Expr
wrap fs e = foldr (\(v,r) b -> Let v r b) e fs

emit :: Var -> Expr -> AnfM ()
emit v r = AnfM (\u -> (u, [(v,r)], ()))

newVar :: AnfM Var
newVar = AnfM (\u -> (u+1, [], Simple ('t' : show u)))
