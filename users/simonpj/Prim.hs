module Prim where

import Lang

lmZero :: Expr
lmZero = Call (Simple "lmZero") []    -- :: LM a b

lmOne :: Expr
lmOne = Call (Simple "lmOne") []      -- :: LM a a

lmCompose :: Expr -> Expr -> Expr
lmCompose f g = Call (Simple "lmCompose") [f,g]
