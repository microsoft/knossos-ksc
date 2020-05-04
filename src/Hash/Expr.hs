{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Maybe (mapMaybe)
import Data.String (fromString)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Simple lambda expression type
data Expr a =
    Var a
  | Lam a (Expr a)
  | App (Expr a) (Expr a)
  deriving (Functor, Show, Eq)

exprSize :: Expr a -> Int
exprSize = \case
  Var _ -> 1
  Lam _ e -> 1 + exprSize e
  App f x -> 1 + exprSize f + exprSize x

showPath :: Path -> String
showPath = concatMap show . reverse

data Step = Apl | Apr | L deriving (Generic, Show, Eq, Ord)

instance Hashable Step where

type Path = [Step]

-- Example expressions from "Finding Identical Subexpressions"
--
-- The examples are not supposed to be well-typed.  The common
-- subexpression identification algorithms do their job regardless of
-- well-typedness.

showExpr :: Expr String -> String
showExpr = \case
  Var x   -> x
  Lam x e -> "(lam " ++ x ++ " " ++ showExpr e ++ ")"
  App f e -> "(" ++ showExpr f ++ " " ++ showExpr e ++ ")"

-- | Render the Expr to Html.  The subtrees that occur at the end of
-- any of the paths are colored.
highlightedExprColor :: String -> [Path] -> Expr String -> H.Html
highlightedExprColor color paths =
  if [] `elem` paths
  then highlight . H.toHtml . showExpr
  else \case
    Var x   -> H.toHtml x
    Lam x e -> do
      H.toHtml ("(lam " ++ x ++ " ")
      recurse (\case { L:xs -> Just xs; _ -> Nothing }) e
      H.toHtml ")"
    App f e -> do
      H.toHtml "("
      recurse (\case { Apl:xs -> Just xs; _ -> Nothing }) f
      H.toHtml " "
      recurse (\case { Apr:xs -> Just xs; _ -> Nothing }) e
      H.toHtml ")"

  where highlight = H.span H.! A.style
          (fromString ("background-color: " ++ color))

        recurse g = highlightedExprColor color (mapMaybe g paths)

highlightedExpr :: [Path] -> Expr String -> H.Html
highlightedExpr = highlightedExprColor "#bbbbff"

example1 :: Expr String
example1 = (Lam "x" (Var "x" .+ Var "x"))
           `App`
           (Lam "y" (Var "y" .+ Var "y"))

example2 :: Expr String
example2 = Lam "x" ((Var "x" .+ Var "x")
                    `App` (Lam "x" (Var "x" .+ Var "x"))
                    `App` (Var "x" .+ Var "x"))

example3 :: Expr String
example3 = (Lam "x" (Var "x" .+ (Var "y" .+ Var "3")))
           `App` (Var "y" .+ Var "3")
           `App` (Lam "y" (Var "y" .+ Var "3"))

example4 :: Expr String
example4 = Lam "x" ((Var "x" .+ Var "x")
                    .+ (Lam "y" ((Var "x" .+ Var "x") .+ Var "y")
                         `App` Var "3"))

example5 :: Expr String
example5 = ((Var "x" .+ Var "1") .* Var "y")
           ./
           (Lam "tmp" (Expr.atan2 (Var "tmp") (Var "tmp" .* Var "y"))
            `App`
           (Var "x" .+ Var "1"))


example6 :: Expr String
example6 = (Var "x" .+ Var "x") `App` (Var "x" .+ Var "x")

example7 :: Expr String
example7 = (Lam "x" (Var "x" .+ Var "x")) `App` (Lam "x" (Var "x" .+ Var "x"))

binOp :: String -> Expr String -> Expr String -> Expr String
binOp opName e1 e2 = Var opName `App` e1 `App` e2

(.+) :: Expr String -> Expr String -> Expr String
(.+) = binOp "add"

(.*) :: Expr String -> Expr String -> Expr String
(.*) = binOp "mul"

(./) :: Expr String -> Expr String -> Expr String
(./) = binOp "div"

atan2 :: Expr String -> Expr String -> Expr String
atan2 = binOp "atan2"
