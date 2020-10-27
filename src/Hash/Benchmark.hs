{-# LANGUAGE BangPatterns #-}

module Benchmark where

import qualified Hedgehog.Gen as Gen
import Data.List (intercalate, foldl')
import qualified System.Clock as Clock
import Text.Printf (printf)
import System.IO.Temp (createTempDirectory)

import Expr (Expr, Path, exprSize)
import Hash (Hash, castHash, castHashOptimized, deBruijnHash, combinedHash, naiveHashNested,
             genExprNumVars {-, genExprLinearNumVars-})

-- | This is the entry point to the module.  When run it will
-- benchmark the algorithms on a random set of expressions.  The data
-- from the run will be written out to a directory whose name is
-- displayed at the end of the run.
benchmark :: IO ()
benchmark = do
  let -- totalExpressions     = 100
      -- samplesPerExpression = 500
      -- iterationsPerSample  = 20
      -- genExpr = Gen.resize 2 . genExprLinearNumVars

      totalExpressions     = 10
      samplesPerExpression = 10
      iterationsPerSample  = 10
      genExpr = Gen.resize 15 . genExprNumVars

      algorithms = [ ("Compositional", castHash,   "green")
                   , ("Compositional-Optimized", castHashOptimized,   "black")
                   , ("DeBruijn", deBruijnHash,    "red")
                   , ("Combined", combinedHash,    "blue")
                   , ("Naive",    naiveHashNested, "orange") ]

      varCounts = [ (10, "1") {-, (100, "4")-} ]

      allParams = (,) <$> algorithms <*> varCounts

      enumFrom1 :: [a] -> [(Int, a)]
      enumFrom1 = zip [1..]

  benchmarksDir <- createTempDirectory "." "benchmarks"

  results <- flip mapM (enumFrom1 allParams) $ \(i, (algorithm_, var_)) -> do
    let (varCount, _) = var_
        (algorithmName, algorithm, _) = algorithm_
    results <- times totalExpressions [] $ \rest -> do
      expression <- Gen.sample (genExpr varCount)

      r <- benchmarkOne samplesPerExpression
                        iterationsPerSample
                        (seqHashResult . algorithm)
                        expression

      putStrLn ("Parameter set "
                 ++ show i ++ "/" ++ show (length allParams)
                 ++ " (" ++ algorithmName ++ ")")
      putStrLn ("Generated " ++ show (length rest)
                ++ " out of " ++ show totalExpressions ++ " expressions")

      let (n, mean, tmin, variance, stddev) = stats r
          showFloat = printf "%.0f" :: Double -> String

      putStrLn ("Count: "    ++ show n)
      putStrLn ("Mean: "     ++ showFloat mean     ++ "us")
      putStrLn ("Min: "      ++ showFloat tmin     ++ "us")
      putStrLn ("Variance: " ++ showFloat variance ++ "us^2")
      putStrLn ("Std dev: "  ++ showFloat stddev   ++ "us")

      return ((exprSize expression, tmin):rest)

    let textOutput = flip concatMap results $ \(size, time) ->
          show size ++ " " ++  show time ++ "\n"
        filename = benchmarksDir ++ "/" ++ algorithmName ++ show varCount ++ ".dat"

    writeFile filename textOutput

    return (algorithm_, var_, filename)

  let gnuplotFilename    = benchmarksDir ++ "/benchmarks.gnuplot"
      gnuplotPngFilename = benchmarksDir ++ "/benchmarks-png.gnuplot"

      gnuplotFileContent = gnuplotFile results
      (outputPng, gnuplotPngFileContent) = gnuplotFilePng benchmarksDir results

  writeFile gnuplotFilename gnuplotFileContent
  writeFile gnuplotPngFilename gnuplotPngFileContent

  putStrLn ("I put stuff in " ++ benchmarksDir ++ ".")
  putStrLn "If you have an X server and you want a live graph view run:"
  putStrLn ("DISPLAY=:0 gnuplot --persist " ++ gnuplotFilename)
  putStrLn "If you want to generate a PNG run:"
  putStrLn ("gnuplot " ++ gnuplotPngFilename)
  putStrLn ("You will find the output PNG in " ++ outputPng)

type AggregateStatistics = (Int, Double, Double, Double)

stats :: AggregateStatistics -> (Int, Double, Double, Double, Double)
stats (n, tsum, tsquaredsum, tmin) = (n, mean, tmin, variance, stddev)
  where n' = fromIntegral n
        mean     = tsum / n'
        variance = tsquaredsum / n' - mean * mean
        stddev   = sqrt variance

-- Runs algorithm on expression and produces aggregate timing
-- statistics.
--
-- benchmarkOne will seq the result of `algorithm expression`.  It is
-- the caller's responsibility to ensure that this causes *all*
-- desired work to be performed.  If you're not sure on this point
-- please ask Tom Ellis (toelli).
benchmarkOne :: Int
             -> Int
             -> (e -> r)
             -> e
             -> IO AggregateStatistics
benchmarkOne samplesPerExpression iterationsPerSample algorithm expression =
  times samplesPerExpression (0 :: Int, 0, 0, infinity) $ \(n, !t, !tsquared, !minSoFar) -> do
        start <- Clock.getTime Clock.Monotonic
        times iterationsPerSample () $ \() ->
          evaluate algorithm expression
        stop <- Clock.getTime Clock.Monotonic

        let elapsed_micro = iterationsElapsed_micro / fromIntegral iterationsPerSample
              where iterationsElapsed = Clock.diffTimeSpec stop start
                    iterationsElapsed_nano = Clock.toNanoSecs iterationsElapsed
                    iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3

        return (n + 1,
                t + elapsed_micro,
                tsquared + elapsed_micro * elapsed_micro,
                min minSoFar elapsed_micro)

  where infinity = 1e60

gnuplotFilePng :: String
               -> [((String, b, String), (Int, String), String)]
               -> (String, String)
gnuplotFilePng benchmarksDir results = (outputPng, unlines [
    "set terminal pngcairo size 1024,768"
  , "set output \"" ++ outputPng ++ "\""
  , gnuplotFile results
  ])
  where outputPng = benchmarksDir ++ "/benchmark.png"


gnuplotFile :: [((String, b, String), (Int, String), String)] -> String
gnuplotFile results =
  unlines [ "set xlabel \"Number of nodes in expression\""
          , "set ylabel \"Time taken to hash all subexpressions / us"
          , "plot " ++ intercalate ", " (fmap plotComponent results)
          ]

  where quote s = "\"" ++ s ++ "\""

        plotComponent ((algorithmName, _, algorithmColor),
                       (varCount, varCountSymbol),
                       filename) =
          intercalate " " [ quote filename
                          , "title " ++ title
                          , "lt rgb " ++ quote algorithmColor
                          , "pt " ++ varCountSymbol ]

          where title = quote (algorithmName ++ " "
                               ++ show varCount ++ " variables")

-- We apply the argument to the function here.  If we do it at the
-- call site then GHC may float it outside of the timing loop!
-- Therefore it's important that this function not be inlined.
-- It seems it's also important for it to return IO so as not to be
-- floated outside the timing loop.
{-# NOINLINE evaluate #-}
evaluate :: (e -> a) -> e -> IO ()
evaluate a e = let !_ = a e
                     in return ()

seqHashResult :: [(Hash, Path, Expr a)] -> ()
seqHashResult = let f a (hash, _path, _expr) =
                      let !_ = hash in a
                in foldl' f ()

times :: Monad m => Int -> s -> (s -> m s) -> m s
times n s f = times_f 0 s
  where times_f m s_ =
          if m >= n
          then return s_
          else do
            s' <- f s_
            times_f (m + 1) s'
