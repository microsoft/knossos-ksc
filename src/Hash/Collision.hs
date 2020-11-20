{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Collision where

import qualified Hash

import qualified Benchmark
import Expr
import qualified KATHashOptimizedHash16

import Data.Hashable (Hashable)
import Data.List (intercalate)
import Control.Monad (when)
import System.IO.Temp (createTempDirectory, emptyTempFile)

genNotAlphaEquiv :: (Monad m, Hashable a, Ord a, Eq h)
                 => m (Expr h a)
                 -> m (Expr h a, Expr h a)
genNotAlphaEquiv gen = do
  expr1 <- gen
  expr2 <- gen

  if Hash.alphaEquivalentAccordingToUniquifyBinders expr1 expr2
    then genNotAlphaEquiv gen
    else pure (expr1, expr2)

countCollisions :: (Int -> IO (Expr () Int, Expr () Int)) -> Int -> Int -> IO Double
countCollisions generate numNodes numBigIters = do
  let numHashcodes :: Int
      numHashcodes = 2 ^ (16 :: Int)
      numTrials :: Int
      numTrials = numBigIters * numHashcodes

  (_, collisionCount) <- Benchmark.times numTrials (0 :: Int, 0 :: Int) $ \(!loopCount, !collisionCount) -> do
    when (loopCount `mod` 100 == 0) $ do
      let percentComplete :: Double
          percentComplete = fromIntegral loopCount / fromIntegral numTrials * 100

      putStrLn ("Iteration " ++ show loopCount ++ "/" ++ show numTrials
               ++ " (" ++ show percentComplete ++ "% complete)")

    (expr1 :: Expr () Int, expr2 :: Expr () Int) <- generate numNodes

    let h1 = annotation (KATHashOptimizedHash16.katHash expr1)
        h2 = annotation (KATHashOptimizedHash16.katHash expr2)
        collisionCount' =
          if h1 == h2
          then collisionCount + 1
          else collisionCount

    pure (loopCount + 1, collisionCount')

  return ((fromIntegral collisionCount) / (fromIntegral numTrials) * (fromIntegral numHashcodes))

collisions :: Int -> IO ()
collisions numBigIters =
  let numsNodes = [128, 256, 512, 1024, 2048, 4096]
      getNumCollisions generate = traverse (\numNodes -> countCollisions generate numNodes numBigIters) numsNodes
  in do
    resultsRandom  <- getNumCollisions (\numNodes -> genNotAlphaEquiv (Hash.genExprWithVarsSize numNodes 10))
    resultsHard    <- getNumCollisions Hash.genExprHardPair
    results        <- pure (zip numsNodes (zip resultsRandom resultsHard))

    let textOutput = flip concatMap results $ \(numNodes, (resultRandom, resultHard)) ->
          unwords [show numNodes, show resultRandom, show resultHard, "1", show (10 * numNodes), "\n"]

    resultsDir <- createTempDirectory "." "benchmarks"
    datFile    <- emptyTempFile resultsDir "collisions.dat"
    gnuplotFilename <- emptyTempFile resultsDir "collisions.gnuplot"
    gnuplotPdfFilename <- emptyTempFile resultsDir "collisions-pdf.gnuplot"
    outputPdf <- emptyTempFile resultsDir "collisions.pdf"

    writeFile datFile textOutput
    writeFile gnuplotFilename (gnuplotFileContents datFile)
    writeFile gnuplotPdfFilename (gnuplotFilePdf outputPdf datFile)

    putStrLn ("I put stuff in " ++ resultsDir)
    putStrLn "If you have an X server and you want a live graph view run:"
    putStrLn ("gnuplot --persist " ++ gnuplotFilename)
    putStrLn "If you want to generate a PDF run:"
    putStrLn ("gnuplot " ++ gnuplotPdfFilename)
    putStrLn ("You will find the output PDF in " ++ outputPdf)

gnuplotFileContents :: String -> String
gnuplotFileContents datFile =
  unlines [ "set key at graph 0.445, 0.985"
          , "set logscale xy 10"
          , "set xrange [120:4500]"
          , "set yrange [0.5:65536]"
          , "set xlabel \"Expression size\""
          , "set ylabel \"Number of collisions per 2^{16} trials\""
          , "plot "
            ++ intercalate ", " [ quote datFile ++ " using 1:2 title \"Empirical (random expr.)\" with linespoints linestyle 1"
                                , quote datFile ++ " using 1:3 title \"Empirical (adversarial expr.)\" with linespoints linestyle 2"
                                , quote datFile ++ " using 1:4 title \"Perfectly random hash\" with lines linestyle 4"
                                , quote datFile ++ " using 1:5 title \"Bound from Theorem 6.7\" with lines linestyle 6" ]

          ]
  where quote s = "\"" ++ s ++ "\""

gnuplotFilePdf :: String -> String -> String
gnuplotFilePdf outputPdf datFile = unlines
  [ "set terminal pdf font \"Helvetica,13\""
  , "set output \"" ++ outputPdf ++ "\""
  , gnuplotFileContents datFile
  ]
