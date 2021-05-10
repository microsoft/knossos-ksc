module Ksc.Opt.Shape where

import Lang
import Prim

optShape :: TExpr -> Maybe TExpr

optShape (Call (TFun ty (Fun ds f)) e) = Just $ Call (TFun (shapeType ty) (Fun (ShapeFun ds) f)) e
optShape e = Just $ Prim.shape (typeof e) e
