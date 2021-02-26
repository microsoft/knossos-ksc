{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Ksc.Traversal where

import           Data.Bifunctor (Bifunctor, bimap)
import qualified Data.Traversable as T

import Optics

traverseState :: Traversable t
              => (a -> s -> (b, s)) -> t a -> s -> (t b, s)
traverseState = swap_ . T.mapAccumL . swap_
  where swap_ f x0 x1 = let (y0, y1) = f x1 x0 in (y1, y0)

mapAccumLM :: Monad m => (a -> b -> m(a, c)) -> a -> [b] -> m(a, [c])
mapAccumLM _ a [] = return (a, [])
mapAccumLM f a (x:xs) = do { (a', c) <- f a x
                           ; (a'', cs) <- mapAccumLM f a' xs
                           ; return (a'', c:cs) }

mapPrism :: (forall a b. Lens (f a) (f b) a b)
         -> Prism s t a b
         -> Prism (f s) (f t) (f a) (f b)
mapPrism lens_ prism_ = prism (over lens_ (withPrism prism_ const))
                              ((bidistribute lens_ . matching) prism_)

data Both f a = Both { unBoth :: f a a }

instance Bifunctor f => Functor (Both f) where
  fmap f (Both faa) = Both (bimap f f faa)

bidistribute :: Bifunctor f
             => (forall a b. Lens (t a) (t b) a b)
             -> (a -> f b b') -> t a -> f (t b) (t b')
bidistribute l1 f = r
  where r = bimap (over l1 unLeft) (over l1 unRight)
            . unBoth
            . toLensVL l1 (Both . bimap Left Right . f)
        unLeft = \case
          Left l -> l
          Right{} -> error "Unexpected Right"

        unRight = \case
          Right r -> r
          Left{} -> error "Unexpected Left"

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is l = not . isn't l
