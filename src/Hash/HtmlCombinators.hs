{-# LANGUAGE RankNTypes #-}

module HtmlCombinators (module HtmlCombinators) where

import qualified Control.Monad.Trans.Writer as W
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String

td :: Html -> Html
td = H.td H.! A.style (fromString "border: 1px solid")

tdCenter :: Html -> Html
tdCenter = H.td H.! A.style (fromString "border: 1px solid; text-align: center")

th :: Html -> Html
th = H.th H.! A.style (fromString "border: 1px solid")

table :: Html -> Html
table = H.table H.! A.style
    (fromString "border: 1px solid; border-collapse: collapse")


-- Simple iterator functions which could, perhaps, be replaced by the
-- Stream type from streaming library.

type Iterator' a r = W.Writer [a] r
type Iterator a = Iterator' a ()

forEach :: Monad m => Iterator' a r -> (a -> m b) -> m ()
forEach s = flip mapM_ (W.execWriter s)

inList :: [a] -> Iterator a
inList = mapM_ yield

inFoldable :: Foldable t => t a-> Iterator a
inFoldable = mapM_ yield

annotating :: Monad m
           => (a -> m b)
           -> (forall m'. Monad m' => (a -> m' ()) -> m' r)
           -> m ()
annotating f s = forEach (s yield) f

yield :: a -> Iterator a
yield = W.tell . pure

listOfIterator :: Iterator a -> [a]
listOfIterator = W.execWriter

setFromIterator :: Ord a => Iterator a -> Set a
setFromIterator = Set.fromList . listOfIterator

writeFile' :: FilePath -> Iterator String -> IO ()
writeFile' filename = writeFile filename . concat . listOfIterator

writeFileHTML :: FilePath -> Html -> IO ()
writeFileHTML filename =
  writeFile filename . Text.Blaze.Html.Renderer.String.renderHtml
