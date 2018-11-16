{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module TypedLang where

import           Data.Void
import qualified Bound                         as B
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Reader           ( lift )

-- Library

{-
data Mu t scope a = V a | Roll (t (Mu t) a)

instance Bound t => Functor (Mu t) where
    fmap f (V a)    = V (f a)
    fmap f (Roll g) = Roll (fmap2 f g)

fmap2 :: (Monad f, Bound t) => (a -> c) -> t f a -> t f c
fmap2 f x = x >>>= return . f

instance Bound t => Applicative (Mu t) where pure = V; (<*>) = ap

instance Bound t => Monad (Mu t) where
    return = V
    V a    >>= f = f a
    Roll g >>= f = Roll (g >>>= f)
-}

-- Client

{-
data Exp a
  = V a
  | Let (Scope Void Exp a) (Scope () Exp a)
  | Tuple [Scope Void Exp a]
  | Call Fun (Scope Void Exp a)
-}


data Fun = Fun

data ExprF scope = Let (scope Void) (scope ())
                 | Tuple [scope Void]
                 | Call Fun (scope Void)

data Flip scope bound exp free = Flip { unFlip :: scope free exp bound }

data Mu e scope a = V a
                  | Roll (e (Flip scope a (Mu e scope)))

instance Functor (Mu e scope) where
instance Applicative (Mu e scope) where
instance Monad (Mu e scope) where

class MonadScope scope where
  bound   :: b -> scope b e a -- "raise"

free = return
unScope = lift

tuple a b = Roll (Tuple [Flip (unScope a), Flip (unScope b)])

vx :: String
vx = "x"
vy = "y"

call f e = Roll (Call f `a` e)

pAdd a b = call Fun (tuple a b)
pMul a b = call Fun (tuple a b)

ex1 :: Mu ExprF B.Scope String
ex1 = let_ vy (pMul (pure vx) (pure vx)) (pAdd (pure vx) (pure vy))

let_
  :: Eq a => a -> Mu ExprF B.Scope a -> Mu ExprF B.Scope a -> Mu ExprF B.Scope a
let_ v e1 e2 = Roll (Let (Flip (unScope e1)) (Flip (B.abstract1 v e2)))

abstract f e = fmap k e
 where
  k y = case f y of
    Just z  -> bound z
    Nothing -> pure (return y)

--abstract1 a = abstract (\b -> if a == b then Just () else Nothing)



a b v = b (Flip (unScope v))
