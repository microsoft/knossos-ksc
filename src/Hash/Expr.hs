{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.Arrow (first)
import Data.Maybe (mapMaybe)
import Data.String (fromString)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Simple lambda expression type
data Expr h a =
    Var !h !a
  | Lam !h !a !(Expr h a)
  | App !h !(Expr h a) !(Expr h a)
  deriving (Functor, Show, Read, Eq)

exprSize :: Expr h a -> Int
exprSize = \case
  Var _ _ -> 1
  Lam _ _ e -> 1 + exprSize e
  App _ f x -> 1 + exprSize f + exprSize x

showPath :: Path -> String
showPath = concatMap show . reverse

data Step = Apl | Apr | L deriving (Generic, Show, Eq, Ord)

instance Hashable Step where

type Path = [Step]

mapAnnotation :: (h -> h') -> Expr h a -> Expr h' a
mapAnnotation f = \case
  Var h a -> Var (f h) a
  Lam h x e -> Lam (f h) x (mapAnnotation f e)
  App h e1 e2 -> App (f h) (mapAnnotation f e1) (mapAnnotation f e2)

-- I'm not convinced that fmapping the return value is better than
-- passing down an accumulating path, but it is marginally easier to
-- write.
allSubexprs :: Expr h a -> [(Path, Expr h a)]
allSubexprs e = ([], e) : case e of
  Var _ _ -> []
  Lam _ _ e' -> (map . first) (L:) (allSubexprs e')
  App _ e1 e2 -> (map . first) (Apr:) (allSubexprs e2)
                ++ (map . first) (Apl:) (allSubexprs e1)

annotation :: Expr h a -> h
annotation = \case
  Var h _   -> h
  Lam h _ _ -> h
  App h _ _ -> h

-- Example expressions from "Finding Identical Subexpressions"
--
-- The examples are not supposed to be well-typed.  The common
-- subexpression identification algorithms do their job regardless of
-- well-typedness.

showExpr :: Expr h String -> String
showExpr = \case
  Var _ x   -> x
  Lam _ x e -> "(lam " ++ x ++ " " ++ showExpr e ++ ")"
  App _ f e -> "(" ++ showExpr f ++ " " ++ showExpr e ++ ")"

-- | Render the Expr to Html.  The subtrees that occur at the end of
-- any of the paths are colored.
highlightedExprColor :: String -> [Path] -> Expr h String -> H.Html
highlightedExprColor color paths =
  if [] `elem` paths
  then highlight . H.toHtml . showExpr
  else \case
    Var _ x   -> H.toHtml x
    Lam _ x e -> do
      H.toHtml ("(lam " ++ x ++ " ")
      recurse (\case { L:xs -> Just xs; _ -> Nothing }) e
      H.toHtml ")"
    App _ f e -> do
      H.toHtml "("
      recurse (\case { Apl:xs -> Just xs; _ -> Nothing }) f
      H.toHtml " "
      recurse (\case { Apr:xs -> Just xs; _ -> Nothing }) e
      H.toHtml ")"

  where highlight = H.span H.! A.style
          (fromString ("background-color: " ++ color))

        recurse g = highlightedExprColor color (mapMaybe g paths)

highlightedExpr :: [Path] -> Expr h String -> H.Html
highlightedExpr = highlightedExprColor "#bbbbff"

app_ :: Expr () a -> Expr () a -> Expr () a
app_ = App ()

example1 :: Expr () String
example1 = (Lam () "x" (Var () "x" .+ Var () "x"))
           `app_`
           (Lam () "y" (Var () "y" .+ Var () "y"))

example2 :: Expr () String
example2 = Lam () "x" ((Var () "x" .+ Var () "x")
                    `app_` (Lam () "x" (Var () "x" .+ Var () "x"))
                    `app_` (Var () "x" .+ Var () "x"))

example3 :: Expr () String
example3 = (Lam () "x" (Var () "x" .+ (Var () "y" .+ Var () "3")))
           `app_` (Var () "y" .+ Var () "3")
           `app_` (Lam () "y" (Var () "y" .+ Var () "3"))

example4 :: Expr () String
example4 = Lam () "x" (sin_ (Var () "x")
                    .+ (Lam () "y" (sin_ (Var () "x") .+ Var () "y")
                         `app_` Var () "3"))

example5 :: Expr () String
example5 = ((Var () "x" .+ Var () "1") .* Var () "y")
           ./
           (Lam () "tmp" (Expr.atan2 (Var () "tmp") (Var () "tmp" .* Var () "y"))
            `app_`
           (Var () "x" .+ Var () "1"))

example6 :: Expr () String
example6 = (Var () "x" .+ Var () "x") `app_` (Var () "x" .+ Var () "x")

example7 :: Expr () String
example7 = (Lam () "x" (Var () "x" .+ Var () "x")) `app_` (Lam () "x" (Var () "x" .+ Var () "x"))

binOp :: String -> Expr () String -> Expr () String -> Expr () String
binOp opName e1 e2 = Var () opName `app_` e1 `app_` e2

(.+) :: Expr () String -> Expr () String -> Expr () String
(.+) = binOp "add"

(.*) :: Expr () String -> Expr () String -> Expr () String
(.*) = binOp "mul"

(./) :: Expr () String -> Expr () String -> Expr () String
(./) = binOp "div"

atan2 :: Expr () String -> Expr () String -> Expr () String
atan2 = binOp "atan2"

sin_ :: Expr () String -> Expr () String
sin_ = (Var () "sin" `app_`)
