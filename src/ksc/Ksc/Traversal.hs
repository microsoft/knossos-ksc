module Ksc.Traversal where

import qualified Data.Traversable as T
import qualified Data.Functor.Identity as I

traverseState :: Traversable t
              => (a -> s -> (b, s)) -> t a -> s -> (t b, s)
traverseState = swap_ . T.mapAccumL . swap_
  where swap_ f x0 x1 = let (y0, y1) = f x1 x0 in (y1, y0)

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
