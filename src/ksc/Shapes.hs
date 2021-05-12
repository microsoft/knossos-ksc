-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.

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
  = Just $
    Def { def_fun    = Fun (ShapeFun ds) f
        , def_pat    = params
        , def_res_ty = shapeType res_ty
        , def_rhs    = UserRhs (pShape def_rhs) }

shapeDef _ = Nothing

-- Given a Type t, determines whether (shapeType t) is a unit type;
-- if so then the unit value is returned.
shapeIsUnit_maybe :: Type -> Maybe TExpr
shapeIsUnit_maybe TypeInteger = Just (Tuple [])
shapeIsUnit_maybe TypeBool = Just (Tuple [])
shapeIsUnit_maybe TypeFloat = Just (Tuple [])
shapeIsUnit_maybe TypeString = Just (Tuple [])
shapeIsUnit_maybe (TypeTuple ts) = fmap Tuple (traverse shapeIsUnit_maybe ts)
shapeIsUnit_maybe _ = Nothing
