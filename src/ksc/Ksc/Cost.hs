{-# LANGUAGE LambdaCase #-}

module Ksc.Cost where

import qualified Lang
import           Prim (isThePrimFun)

cost :: Lang.TExpr -> Either String Float
cost = \case
  Lang.Konst{}  -> pure 0
  Lang.Var{}    -> pure 0
  Lang.Call f e -> do
    ce <- cost e
    cf <- costFun f (Lang.typeof e)
    pure (apply_cost + ce + cf)
  Lang.Tuple es -> fmap sum (traverse cost es)
  Lang.Let _ rhs body -> do
    r <- cost rhs
    b <- cost body
    pure (let_cost + r + b)
  Lang.If cond t f -> do
    cc <- cost cond
    ct <- cost t
    cf <- cost f
    pure (cc + max ct cf)
  Lang.Assert{} -> notImpl "Assert"
  Lang.Lam{}    -> notImpl "Lam"
  Lang.App{}    -> notImpl "App"
  Lang.Dummy{}  -> notImpl "Dummy"

  where apply_cost = 2
        let_cost = 0.1
        notImpl s = Left ("cost not implemented for " ++ s)

size :: Lang.Type -> Either String Int
size = \case
  Lang.TypeInteger -> pure 1
  Lang.TypeBool    -> pure 1
  Lang.TypeFloat   -> pure 1
  Lang.TypeTuple t -> fmap sum (mapM size t)
  Lang.TypeVec t   -> fmap (100 *) (size t)
  Lang.TypeString  -> notImpl "String"
  Lang.TypeLam{}   -> notImpl "Lam"
  Lang.TypeLM{}    -> notImpl "LM"
  Lang.TypeUnknown -> notImpl "Unknown"
  where notImpl s = Left ("size not implemented for " ++ s)

costFun :: Lang.TFun -> Lang.Type -> Either String Float
costFun f t
  | f `isThePrimFun` "add"    = fmap fromIntegral (size t)
  | f `isThePrimFun` "and"    = fmap fromIntegral (size t)
  | f `isThePrimFun` "or"     = fmap fromIntegral (size t)
  | f `isTheUserFun` "mul@ff" = fmap fromIntegral (size t)
  | f `isTheUserFun` "h"      = pure 10
  | otherwise = Left ("Unknown function: " ++ show f)

isTheUserFun :: Lang.TFun -> String -> Bool
isTheUserFun (Lang.TFun _ (Lang.Fun (Lang.UserFun f1))) f2 = f1 == f2
isTheUserFun _ _ = False
