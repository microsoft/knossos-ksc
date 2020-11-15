module Collision where

import qualified Hash

import qualified Benchmark
import Expr
import Hash (Hash)
import qualified KATHashFastOrigHash

import Data.Bits
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Control.Monad (when)
import System.IO.Temp (createTempDirectory, emptyTempFile)

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen


genNotAlphaEquiv :: (MonadGen m, Hashable a, Ord a, Eq h)
                 => m (Expr h a)
                 -> m (Expr h a, Expr h a)
genNotAlphaEquiv gen = do
  expr1 <- gen
  expr2 <- gen

  when (Hash.alphaEquivalentAccordingToUniquifyBinders expr1 expr2) Gen.discard

  pure (expr1, expr2)

restrictToBits :: Int -> Hash -> Hash
restrictToBits i h = h `shiftL` (bitsize - i)
  where bitsize = finiteBitSize (0 :: Hash)

collisions :: Int -> IO ()
collisions maxBits = do
  let count :: Int
      count = 2 ^ (maxBits + 2)
      emptyMap :: Map.Map Int Int
      emptyMap = Map.fromList (map (\i -> (i, 0)) [0..maxBits])

  (_, m) <- Benchmark.times count (0 :: Int, emptyMap) $ \(loopCount, m) -> do
    when (loopCount `mod` 100 == 0) $ do
      let percentComplete :: Double
          percentComplete = fromIntegral loopCount / fromIntegral count * 100

      putStrLn ("Iteration " ++ show loopCount ++ "/" ++ show count
               ++ " (" ++ show percentComplete ++ "% complete)")

    (expr1, expr2) <- Gen.sample (genNotAlphaEquiv (Hash.genExprNumVars 10))

    let h1 = annotation (KATHashFastOrigHash.katHash expr1)
        h2 = annotation (KATHashFastOrigHash.katHash expr2)

        m'' = Map.mapWithKey (\i v -> if ((==) `on` restrictToBits i) h1 h2
                                      then v+1
                                      else v)
                            m

    pure (loopCount + 1, m'')

  let log2 :: Float -> Float
      log2 x = log x / log 2

  let m''' = Map.mapWithKey (\_ n -> -log2 (proportion n)) m
        where proportion n = fromIntegral n / fromIntegral count

  flip mapM_ (Map.toList m''') $ \(width, logProb) ->
    putStrLn ("Bit width " ++ show width ++ ": "
              ++ "Proportion 1/2 ^ " ++ show logProb ++ " hashes collide."
             ++ "  Uniformly random would be 1/2 ^ " ++ show width)

  let textOutput = flip concatMap (Map.toList m''') $ \(bw, mlogp) ->
        unwords [show bw, show mlogp, show bw, "\n"]

  resultsDir <- createTempDirectory "." "benchmarks"
  datFile    <- emptyTempFile resultsDir "collisions.dat"
  gnuplotFilename <- emptyTempFile resultsDir "collisions.gnuplot"
  gnuplotPngFilename <- emptyTempFile resultsDir "collisions-png.gnuplot"
  outputPng <- emptyTempFile resultsDir "collisions.png"

  writeFile datFile textOutput
  writeFile gnuplotFilename (gnuplotFileContents datFile)
  writeFile gnuplotPngFilename (gnuplotFilePng outputPng datFile)

  putStrLn ("I put stuff in " ++ resultsDir)
  putStrLn "If you have an X server and you want a live graph view run:"
  putStrLn ("gnuplot --persist " ++ gnuplotFilename)
  putStrLn "If you want to generate a PNG run:"
  putStrLn ("gnuplot " ++ gnuplotPngFilename)
  putStrLn ("You will find the output PNG in " ++ outputPng)

gnuplotFileContents :: String -> String
gnuplotFileContents datFile =
  unlines [ "set key left top"
          , "set xlabel \"Hash width / bits\""
          , "set ylabel \"-log_2 expressions per collision (balanced expressions)"
          , "plot "
            ++ intercalate ", " [ quote datFile ++ " using 2 title \"KAT hash actual\""
                                , quote datFile ++ " using 3 title \"Perfectly random\"" ]

          ]
  where quote s = "\"" ++ s ++ "\""

gnuplotFilePng :: String -> String -> String
gnuplotFilePng outputPng datFile = unlines
  [ "set terminal pngcairo size 1024,768"
  , "set output \"" ++ outputPng ++ "\""
  , gnuplotFileContents datFile
  ]
