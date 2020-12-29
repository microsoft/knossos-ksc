module Ksc.Traversal where

import qualified Control.Monad.State as S
import           Data.Tuple (swap)
import qualified Data.Traversable as T
import qualified Data.Functor.Identity as I

traverseState :: Traversable t
              => (a -> s -> (b, s)) -> t a -> s -> (t b, s)
traverseState = swap_ . T.mapAccumL . swap_
  where swap_ f x0 x1 = let (y0, y1) = f x1 x0 in (y1, y0)

mapAccumLM :: Monad m => (a -> b -> m(a, c)) -> a -> [b] -> m(a, [c])
mapAccumLM _ a [] = return (a, [])
mapAccumLM f a (x:xs) = do { (a', c) <- f a x
                           ; (a'', cs) <- mapAccumLM f a' xs
                           ; return (a'', c:cs) }

mapOf :: ((a -> I.Identity b) -> (s -> I.Identity t))
      -> (a -> b)
      -> s
      -> t
mapOf fmap' f = I.runIdentity . fmap' (I.Identity . f)

traverseOf :: Applicative f
           => ((a -> f b) -> (s -> f t))
           -> (a -> f b)
           -> s
           -> f t
traverseOf = id

-- From
-- https://www.stackage.org/haddock/lts-16.27/lens-4.18.1/Control-Lens-Combinators.html#v:mapAccumLOf,
-- but I don't want to pick up an entire lens dependency
mapAccumLOf :: ((a -> S.State acc b) -> (s -> S.State acc t))
            -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
mapAccumLOf l f acc0 s = swap (S.runState (l g s) acc0) where
     g a = S.state $ \acc -> swap (f acc a)
