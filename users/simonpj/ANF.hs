module ANF where

import Lang
import KMonad
import Control.Monad( ap )

anfDefs :: [Def] -> KM [Def]
anfDefs defs
  = do { u <- getUniq
       ; let (u', defs') = runAnf u (mapM anfD defs)
       ; setUniq u'
       ; return defs' }

-----------------------------------------------
anfD :: Def -> AnfM Def
anfD (Def fun args rhs) = Def fun args <$> anfExpr rhs

anfExpr :: Expr -> AnfM Expr
anfExpr e = wrapLets (anfE e)

anfE :: Expr -> AnfM Expr
anfE (Tuple es)            = Tuple <$> mapM anfE1 es
anfE (Call fun (Tuple es))
  | Fun (SFun "build") <- fun
  , [n,e] <- es
  = -- Don't bother to ANF the first arr
    -- and leave the second arg in place
    do { e' <- anfE e
       ; return (Call fun (Tuple [n, e'])) }
  | otherwise
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
