module CompareBehaviour where

import Hash (castHash,castHashOptimized,combinedHash,deBruijnHash,
             structuralHashWithBinders, structuralHashWithBinders2,
             normalizedGroupedEquivalentSubexpressions,
             allHashResults)
import Expr (Expr, Path, showExpr,
             example1,example2,example3,example4,example5,example6,example7,
             highlightedExprColor, highlightedExpr, showPath)

import HtmlCombinators (table, th, td, writeFileHTML, forEach, inList,
                        annotating, tdCenter, inFoldable, setFromIterator)
import Lens.Micro
import Text.Blaze.Html5 hiding (table, th, td, map)

-- | This is the entry point to this module.  Provide a file path and
-- it will write an HTML file which shows the comparison of the
-- hashing functions.
writeAwfFormatExpressionsHTML :: FilePath -> IO ()
writeAwfFormatExpressionsHTML = flip writeFileHTML awfFormatExpressionsHTML

awfFormatExpressionsHTML :: Html
awfFormatExpressionsHTML =
  forEach (inList expressions) $ \(expressionName, expression) -> do
    p (toHtml expressionName)
    awfFormatExpressionHTML expression

  where expressions = [ ("Example 1", example1)
                      , ("Example 2", example2)
                      , ("Example 3", example3)
                      , ("Example 4", example4)
                      , ("Example 5", example5)
                      , ("Example 6", example6)
                      , ("Example 7", example7) ]

awfFormatExpressionHTML :: Expr () String -> Html
awfFormatExpressionHTML e =
  table $ annotating tr $ \tr_ -> do
     tr_ $ annotating th $ \th_ -> do
       th_ (code (toHtml (showExpr e)))
       forEach (inRow algorithms) $ \(algorithmName, _) ->
         th_ (toHtml algorithmName)
     forEach (inFoldable allGroups) $ \group -> do
         let shouldBePresent = group `elem` castGroups
             color = if shouldBePresent then green else red
         tr_ $ do
           td $ code $ highlightedExprColor color (map reverse group) e
           forEach (inRow groupsPerAlgorithm) $ \thisAlgorithm'sGroups -> do
             let actuallyPresent = group `elem` thisAlgorithm'sGroups
                 correct = shouldBePresent == actuallyPresent
             tdCenter $ case (correct, shouldBePresent) of
                  (False, _)     -> cross
                  (True,  True)  -> tick
                  (True,  False) -> mempty

  where algorithms = ( ("Compositional", castHash)
                     , ( ("Compositional-Optimized", castHashOptimized)
                       , ("Combined", combinedHash)
                       , ("DeBruijn", deBruijnHash)
                       , ("Structural with binders 1", structuralHashWithBinders)
                       , ("Structural with binders 2", structuralHashWithBinders2) ) )

        groupsOfAlgorithm algorithm =
          ((map . map) fst
           . normalizedGroupedEquivalentSubexpressions
           . algorithm) e

        (castGroups, _) = groupsPerAlgorithm

        groupsPerAlgorithm = mapRow (groupsOfAlgorithm . (allHashResults .) . snd) algorithms

        allGroups = setFromIterator $ forEach (inRow groupsPerAlgorithm) inList

        inRow (t, ts) = inList (t : toListOf each ts)
        mapRow f (t, ts) = (f t, over each f ts)

printGroups :: Ord hash => [(hash, Path, Expr h String)] -> IO ()
printGroups = mapM_ (\x -> putStrLn "" >> mapM_ putStrLn x)
              . (map . map) (\(path, z) -> showExpr z ++ " [" ++ showPath path ++ "]")
              . normalizedGroupedEquivalentSubexpressions

showMany :: Html
showMany =
  forEach (inList examples) $ \(expressionName, expression) -> do
    p (toHtml (expressionName ++ ": " ++ showExpr expression))
    forEach (inList algorithms) $ \(algorithmName, algorithm) -> do
        p (b (toHtml algorithmName))
        showGroupsHTML (allHashResults . algorithm) expression

  where examples   = [ ("Example 4", example4) ]
        algorithms = [ ("Compositional", castHash)
                     , ("Combined", combinedHash)
                     , ("DeBruijn", deBruijnHash)
                     ]

red :: String
red = "#ffbbbb"

green :: String
green = "#bbffbb"

tick :: Html
tick = preEscapedToHtml "&#10003;"

cross :: Html
cross = preEscapedToHtml "&#10060;"

showGroupsHTML :: Ord hash
               => (Expr h String -> [(hash, Path, b)])
               -> Expr h String
               -> Html
showGroupsHTML algorithm e =
  ((\lines_ ->
      table $ annotating tr $ \tr_ ->
       forEach (inList lines_) $ \line ->
         tr_ $ annotating td $ \td_ -> do
           td_ line
           td_ (tick >> cross))
   . map (flip highlightedExpr e . map (reverse :: Path -> Path))
   . (map . map) fst
   . normalizedGroupedEquivalentSubexpressions
   . algorithm) e
