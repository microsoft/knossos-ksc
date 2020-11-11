-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.

module Shapes where

import Lang
import LangUtils
import Prim
import GHC.Stack
import Data.Maybe(mapMaybe)

shapeDefs :: HasCallStack => [TDef] -> [TDef]
shapeDefs = mapMaybe (shapeDef . oneArgifyDef)

shapeDef :: HasCallStack => TDef -> Maybe TDef

shapeDef (Def { def_fun = ShapeFun _ }) 
  = Nothing

shapeDef (Def { def_fun = f
              , def_pat = VarPat params
              , def_rhs = UserRhs _
              , def_res_ty = res_ty })
  = Just $
    Def { def_fun    = ShapeFun f
        , def_pat    = VarPat params
        , def_res_ty = shapeType res_ty
        , def_rhs    = UserRhs (pShape (Call (TFun res_ty f) (Var params))) }

shapeDef (Def { def_pat = TupPat {} })
  -- TupPat should not appear.  See Note [Replacing TupPat with nested Let] in LangUtils
  = error "shapeDef: TupPat encountered.\nThis should not occur."

shapeDef _ = Nothing


