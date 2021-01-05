module Ksc.Traversal where

import qualified Data.Traversable as T

traverseState :: Traversable t
              => (a -> s -> (b, s)) -> t a -> s -> (t b, s)
traverseState = swap_ . T.mapAccumL . swap_
  where swap_ f x0 x1 = let (y0, y1) = f x1 x0 in (y1, y0)

mapAccumLM :: Monad m => (a -> b -> m(a, c)) -> a -> [b] -> m(a, [c])
mapAccumLM _ a [] = return (a, [])
mapAccumLM f a (x:xs) = do { (a', c) <- f a x
                           ; (a'', cs) <- mapAccumLM f a' xs
                           ; return (a'', c:cs) }
