{-# LANGUAGE BangPatterns #-}

module Benchmark where

import qualified Hedgehog.Gen as Gen
import Data.List (intercalate, foldl')
import qualified System.Clock as Clock
import Text.Printf (printf)
import System.IO.Temp (createTempDirectory)

import Expr (Expr, Path, exprSize)
import Hash (Hash, castHash, deBruijnHash, combinedHash, naiveHashNested,
             genExprNumVars)

-- We apply the argument to the function here.  If we do it at the
-- call site then GHC may float it outside of the timing loop!
-- Therefore it's important that this function not be inlined.
{-# NOINLINE evalHashResult #-}
evalHashResult :: (e -> [(Hash, Path, Expr a)]) -> e -> IO ()
evalHashResult a e = let !_ = seqHashResult (a e)
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

benchmark :: IO ()
benchmark = do
  let totalExpressions     = 1000
      samplesPerExpression = 10
      iterationsPerSample  = 100

      genExpr = Gen.scale (* (100 * 1000)) . genExprNumVars

      algorithms = [ ("Compositional", castHash,   "green")
                   , ("DeBruijn", deBruijnHash,    "red")
                   , ("Combined", combinedHash,    "blue")
                   , ("Naive",    naiveHashNested, "orange") ]

      varCounts = [ (10, "1"), (100, "4") ]

      allParams = (,) <$> algorithms <*> varCounts

      infinity = 1e60

      enumFrom1 :: [a] -> [(Int, a)]
      enumFrom1 = zip [1..]

  benchmarksDir <- createTempDirectory "." "benchmarks"

  results <- flip mapM (enumFrom1 allParams) $ \(i, (algorithm_, var_)) -> do
    let (varCount, _) = var_
        (algorithmName, algorithm, _) = algorithm_
    results <- times totalExpressions [] $ \rest -> do
      expression <- Gen.sample (genExpr varCount)

      (n, tsum, tsquaredsum, tmin) <- times samplesPerExpression (0 :: Int, 0, 0, infinity) $ \(n, !t, !tsquared, !minSoFar) -> do
        start <- Clock.getTime Clock.Monotonic
        times iterationsPerSample () $ \() ->
          evalHashResult algorithm expression
        stop <- Clock.getTime Clock.Monotonic

        let elapsed_micro = iterationsElapsed_micro / fromIntegral iterationsPerSample
              where iterationsElapsed = Clock.diffTimeSpec stop start
                    iterationsElapsed_nano = Clock.toNanoSecs iterationsElapsed
                    iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3

        return (n + 1,
                t + elapsed_micro,
                tsquared + elapsed_micro * elapsed_micro,
                min minSoFar elapsed_micro)

      putStrLn ("Parameter set "
                 ++ show i ++ "/" ++ show (length allParams))
      putStrLn ("Generated " ++ show (length rest)
                ++ " out of " ++ show totalExpressions ++ " expressions")

      let n' = fromIntegral n
          mean     = tsum / n'
          variance = tsquaredsum / n' - mean * mean
          stddev   = sqrt variance

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

  writeFile (benchmarksDir ++ "/benchmarks.gnuplot") (gnuplotFile results)

  putStrLn ("I put stuff in " ++ benchmarksDir ++ ".  Try running the following:")
  putStrLn ("DISPLAY=:0 gnuplot --persist "
            ++ benchmarksDir
            ++ "/benchmarks.gnuplot")

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
