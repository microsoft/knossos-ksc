-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.

{- Note [Shape functions]
~~~~~~~~~~~~~~~~~~~~~~~~~

For every function
   f :: S -> T
   f(x) = rhs
we can generate a companion function
   shape$f :: S -> Sh(T)
   shape$f(x) = shape(rhs)
which we hope to optimize by pushing the shape call down using optimization
rules.

One potential use of shape$f is to allow a caller of f to precalculate
the amount of memory required to hold the result of f. However, the function
shape$f may itself involve allocating memory in the heap.
-}

{-# LANGUAGE DataKinds #-}

module Shapes where

import Lang
import Prim
import GHC.Stack
import Data.Maybe(mapMaybe)

shapeDefs :: HasCallStack => [TDef] -> [TDef]
shapeDefs = mapMaybe shapeDef

shapeDef :: HasCallStack => TDef -> Maybe TDef

shapeDef (Def { def_fun = Fun ds f
              , def_pat = params
              , def_rhs = UserRhs def_rhs
              , def_res_ty = res_ty })
  | hasShapeType res_ty
  = Just $
    Def { def_fun    = Fun (ShapeFun ds) f
        , def_pat    = params
        , def_res_ty = shapeType res_ty
        , def_rhs    = UserRhs (pShape def_rhs) }

shapeDef _ = Nothing
