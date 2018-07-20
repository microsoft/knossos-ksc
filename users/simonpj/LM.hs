{-# LANGUAGE GADTs #-}

module LM where

import Data.Array

----------- Matrices and vectors ---------------
type Matrix a = Array (Int,Int) a
type Vector a = Array Int       a

mvMultiply :: Matrix Float -> Vector Float -> Vector Float
mvMultiply = error "Not yet"

mTranspose :: Matrix Float -> Matrix Float
mTranspose = error "not yet"

----------- Linear maps ---------------
data LM a b where
  Id     :: LM a a
  Leaf   :: Float -> LM Float Float

  Matrix :: Matrix Float  -- (R x C)
         -> LM (Vector Float)  -- R
               (Vector Float)  -- C

  Unpair :: Addable r => LM a r -> LM b r -> LM (a,b) r
  Pair :: Addable a => LM a b -> LM a c -> LM a (b,c)

class Addable a where
  add  :: a -> a -> a
  zero :: a
  
lmApply :: LM p q -> p -> q
lmApply Id           x     = x
lmApply (Leaf f)     x     = f * x
lmApply (Matrix a)   v     = a `mvMultiply` v
lmApply (Unpair ar br) (a,b) = lmApply ar a `add` lmApply br b
lmApply (Pair ab ac) a    = (lmApply ab a, lmApply ac a)
  -- ab :: LM a b
  -- ac :: LM a c
  -- 

lmTranspose :: LM a b -> LM b a
lmTranspose Id             = Id
lmTranspose (Leaf f)       = Leaf (1/f)
lmTranspose (Matrix a)     = Matrix (mTranspose a)
lmTranspose (Unpair ar br) = Pair (lmTranspose ar) (lmTranspose br)
lmTranspose (Pair ab ac)   = Unpair  (lmTranspose ab) (lmTranspose ac)

lmCompose :: LM b c -> LM a b -> LM a c
lmCompose Id        lm             = lm
lmCompose lm        Id             = lm
lmCompose (Leaf f1) (Leaf f2)      = Leaf (f2*f2)
lmCompose (Leaf f)  (Unpair ar br) = Unpair (lmCompose (Leaf f) ar) (lmCompose (Leaf f) br)
                                  -- Not quite sure
-- Etc


