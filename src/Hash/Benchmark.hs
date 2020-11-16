{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

module Benchmark where

import qualified Data.Foldable
import Data.Hashable (Hashable)
import Data.List (intercalate)
import qualified System.Clock as Clock
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.IO.Temp (createTempDirectory, emptyTempFile)

import Expr (Expr, exprSize)
import Hash (deBruijnHash, structuralHashNested)
import qualified Hash
import qualified KATHashFastOrigHash
import qualified KATHashFasterOrigHash

data BenchmarkConfig = BenchmarkConfig
  { bcGenExpr :: Int -> Int -> IO (Expr () Int)
  , bcGenName :: String
  }

data Algorithms a = Algorithms
  { aLocallyNameless        :: a
  , aKATHashFromPaper       :: a
  , aKATHashFromPaperFaster :: a
  , aDeBrujinHash           :: a
  , aStructuralHashNested   :: a
  -- Hash.deBruijnNestedHash is slower than Hash.locallyNameless so
  -- we don't need it
  -- , aDeBruijnNested     :: a
  -- castHash is slower than castHashOptimized so we don't need it
  -- , aCastHash           :: a
  -- combinedHash is slow and broken. We don't want it
  -- , aCombinedHash       :: a
  }
  deriving (Functor, Foldable, Traversable)

algorithms_ :: (Hashable a, Ord a)
            => Algorithms (String, Expr h a -> Expr Hash.Hash a, String)
algorithms_ = Algorithms
  { aLocallyNameless    = ("Locally nameless", Hash.locallyNameless, baseline)
  , aKATHashFromPaper   = ("KATHash as in paper", KATHashFastOrigHash.katHash, paper)
  , aKATHashFromPaperFaster = ("KATHash optimized", KATHashFasterOrigHash.katHash, good)
  , aDeBrujinHash           = ("de Bruijn*", deBruijnHash, prettyBad)
  , aStructuralHashNested   = ("Structural*", structuralHashNested, veryBad)
  -- , aDeBruijnNested     = ("de Bruijn nested", Hash.deBruijnNestedHash, "magenta")
  -- , aCastHash           = ("Compositional", castHash,   "green")
  -- , aCombinedHash       = ("Combined", combinedHash,    "blue")
  }
  where
      veryBad   = "red"
      prettyBad = "orange"
      good      = "web-green"
      baseline  = "web-blue"
      paper     = "purple"

data BenchmarkParams = BenchmarkParams
  { runsToMinimizeOver :: Int
  , minimumMeasurableTime_secs :: Double
  , maximumTime_micro :: Double
  , sizeScale :: Double
  }

fast :: BenchmarkParams
fast = BenchmarkParams
  { runsToMinimizeOver = 3
  , minimumMeasurableTime_secs = 0.01
  , maximumTime_micro = 1000
  , sizeScale = 1.4
  }

full :: BenchmarkParams
full = BenchmarkParams
  { runsToMinimizeOver = 10
  , minimumMeasurableTime_secs = 0.1
  , maximumTime_micro = 1000 * 1000
  , sizeScale = 1.1
  }

-- | This is the entry point to the module.  When run it will
-- benchmark the algorithms on a random set of expressions.  The data
-- from the run will be written out to a directory whose name is
-- displayed at the end of the run.
benchmark :: BenchmarkParams -> IO ()
benchmark bps = do
  let bcs = [ BenchmarkConfig
              { bcGenExpr = \n size -> Hash.genExprWithVarsUnbalancedSize size n
              , bcGenName = "unbalanced expressions"
              }
            , BenchmarkConfig
              { bcGenExpr = \n size -> Hash.genExprWithVarsSize size n
              , bcGenName = "balanced expressions"
              }
            ]

      algorithms = Data.Foldable.toList algorithms_

  benchmarksDir <- createTempDirectory "." "benchmarks"
  results_genNames <- flip mapM (enumFrom1 bcs) $ \(i, bc) -> do
    results <- benchmarkThis bps
                             (show i ++ "/" ++ show (length bcs)
                              ++ " (" ++ bcGenName bc ++ ")")
                             benchmarksDir algorithms bc
    pure (results, bcGenName bc)
  flip mapM_ results_genNames $ \(results, genName) ->
    makeGnuplot benchmarksDir genName results

benchmarkThis :: BenchmarkParams
              -> String
              -> FilePath
              -> [(String, Expr () Int -> Expr hash string, String)]
              -> BenchmarkConfig
              -> IO [PlotDataset]
benchmarkThis bps
              expressionSet
              benchmarksDir algorithms bc = do
  let allParams = algorithms

  results <- flip mapM (enumFrom1 allParams) $ \(i, algorithm_) -> do
    let (algorithmName, algorithm, algorithmColor) = algorithm_
    results <- loop (64, []) $ \(size, rest) -> do
      -- We force the expression after generating it.  The Expr type
      -- is strict, that is forcing it forces everything it contains,
      -- therefore no time is wasted forcing it in the hashing
      -- algorithm itself.  On the other hand adding this bang pattern
      -- made absolutely no difference to the benchmarks.  Presumably
      -- the expression is generated already in forced state.  But
      -- it's nice to keep this here for clarity.
      putStrLn ("Expression set " ++ expressionSet)
      putStrLn ("Parameter set "
                 ++ show i ++ "/" ++ show (length allParams)
                 ++ " (" ++ algorithmName ++ ")")
      putStrLn ("Generated " ++ show (length rest) ++ " expressions")
      putStrLn ("Generating expression of approximate size " ++ show size ++ "...")
      !expression <- bcGenExpr bc 10 size
      let !exprSize' = exprSize expression
      putStrLn ("done.  Size was " ++ show exprSize')

      let minimumMeasureableTime_micro = minimumMeasurableTime_secs bps * 1000 * 1000

      (repeats, firstStats) <- benchmarkUntil minimumMeasureableTime_micro
                                              1
                                              (seqHashResult . algorithm)
                                              expression

      r <- benchmarkMore firstStats
                         (runsToMinimizeOver bps - 1)
                         repeats
                         (seqHashResult . algorithm)
                         expression

      let (n, mean_micro, tmin_micro, _, stddev_micro) = stats r
          showFloat = printf "%.0f" :: Double -> String

      putStrLn ("Count: "    ++ show n)
      putStrLn ("Mean: "     ++ showFloat mean_micro ++ "us")
      putStrLn ("Min: "      ++ showFloat tmin_micro ++ "us")
      putStrLn ("Std dev: "  ++ showFloat stddev_micro ++ "us")

      let done = tmin_micro > maximumTime_micro bps
          size' = floor (fromIntegral size * sizeScale bps) + 1
          tmin_secs = tmin_micro / (1000 * 1000)
          rest' = (exprSize', tmin_secs):rest

      pure $ if done
             then Right rest'
             else Left (size', rest')

    let textOutput = flip concatMap results $ \(size, time) ->
          show size ++ " " ++  show time ++ "\n"

    filename <- emptyTempFile benchmarksDir (algorithmName ++ ".dat")

    writeFile filename textOutput

    return PlotDataset
      { pdFile  = filename
      , pdTitle = algorithmName
      , pdColor = algorithmColor
      , pdStyle = "7"
      , pdSize  = "0.5"
      }

  pure results

makeGnuplot :: FilePath -> String -> [PlotDataset] -> IO ()
makeGnuplot benchmarksDir xlabel results = do
  gnuplotFilename <- emptyTempFile benchmarksDir "benchmarks.gnuplot"
  gnuplotPdfFilename <- emptyTempFile benchmarksDir "benchmarks-pdf.gnuplot"

  let gnuplotFileContent = gnuplotFile xlabel results
      (outputPdf, gnuplotPdfFileContent) = gnuplotFilePdf benchmarksDir xlabel results

  writeFile gnuplotFilename gnuplotFileContent
  writeFile gnuplotPdfFilename gnuplotPdfFileContent

  putStrLn ("I put stuff in " ++ benchmarksDir ++ ".")
  putStrLn "If you have an X server and you want a live graph view run:"
  putStrLn ("gnuplot --persist " ++ gnuplotFilename)
  putStrLn "If you want to generate a PDF run:"
  putStrLn ("gnuplot " ++ gnuplotPdfFilename)
  putStrLn ("You will find the output PDF in " ++ outputPdf)

type AggregateStatistics = (Int, Double, Double, Double)

stats :: AggregateStatistics -> (Int, Double, Double, Double, Double)
stats (n, tsum, tsquaredsum, tmin) = (n, mean, tmin, variance, stddev)
  where n' = fromIntegral n
        mean     = tsum / n'
        variance = tsquaredsum / n' - mean * mean
        stddev   = sqrt variance

-- This is probably the entry point you want to use to benchmark an
-- algorithm on a list of expressions each read from a FilePath.
--
-- Runs algorithm on expression and produces aggregate timing
-- statistics.
--
-- benchmarkOne will seq the result of `algorithm expression`.  It is
-- the caller's responsibility to ensure that this causes *all*
-- desired work to be performed.  If you're not sure on this point
-- please ask the author.
benchmarkOne :: Int
             -> Integer
             -> (e -> r)
             -> e
             -> IO AggregateStatistics
benchmarkOne = benchmarkMore (0, 0, 0, infinity)
  where infinity = 1e60

benchmarkMore :: AggregateStatistics
              -> Int
              -> Integer
              -> (e -> r)
              -> e
              -> IO AggregateStatistics
benchmarkMore already samplesPerExpression iterationsPerSample algorithm expression =
  times samplesPerExpression already $ \(n, !t, !tsquared, !minSoFar) -> do
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

benchmarkUntil :: Double
               -> Integer
               -> (e -> r)
               -> e
               -> IO (Integer, AggregateStatistics)
benchmarkUntil minimumMeasurableTime_micro repeats f x = do
  start <- Clock.getTime Clock.Monotonic
  times repeats () $ \() ->
    evaluate f x
  stop <- Clock.getTime Clock.Monotonic

  let iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3
        where iterationsElapsed = Clock.diffTimeSpec stop start
              iterationsElapsed_nano = Clock.toNanoSecs iterationsElapsed

      elapsed_micro = iterationsElapsed_micro / fromIntegral repeats

  if iterationsElapsed_micro < minimumMeasurableTime_micro
  then benchmarkUntil minimumMeasurableTime_micro (2 * repeats) f x
  else pure (repeats,
             (1, elapsed_micro, elapsed_micro * elapsed_micro, elapsed_micro))

readExpr :: FilePath -> IO (Expr () String)
readExpr = readExprG

readExprG :: Read e => FilePath -> IO e
readExprG filepath = do
  filecontents <- readFile filepath

  case readMaybe filecontents of
    Nothing   -> error ("Couldn't read the expression in " ++ filepath)
    Just expr -> pure expr

benchmarkOneReadFile :: Read e
                     => FilePath
                     -> Int
                     -> Integer
                     -> (e -> r)
                     -> IO AggregateStatistics
benchmarkOneReadFile filepath samplesPerExpression iterationsElapsed algorithm = do
  expr <- readExprG filepath

  benchmarkOne samplesPerExpression iterationsElapsed algorithm expr

gnuplotFilePdf :: String
               -> String
               -> [PlotDataset]
               -> (String, String)
gnuplotFilePdf benchmarksDir xlabel results = (outputPdf, unlines [
    "set terminal pdf font \"Helvetica,13\""
  , "set output \"" ++ outputPdf ++ "\""
  , gnuplotFile xlabel results
  ])
  where outputPdf = benchmarksDir ++ "/benchmark.pdf"

gnuplotFile :: String -> [PlotDataset] -> String
gnuplotFile xlabel results =
  unlines [ "set xlabel \"Number of nodes in expression (" ++ xlabel ++ ")\""
          , "set ylabel \"Time taken to hash all subexpressions (s)"
          , "set format y '%.0se%S'"
          , "set format x '%.0se%S'"
          , "set size 1,1"
          , "set logscale xy 10"
          , "set key right bottom"
          , "set yrange [1e-6:1]"
          , "plot " ++ intercalate ", " (fmap plotDataset results)
          ]

data PlotDataset = PlotDataset
  { pdFile  :: String
  , pdTitle :: String
  , pdColor :: String
  , pdStyle :: String
  , pdSize  :: String
  }

plotDataset :: PlotDataset -> String
plotDataset pd = intercalate " " [ quote (pdFile pd)
                                 , "title " ++ quote (pdTitle pd)
                                 , "lt rgb " ++ quote (pdColor pd)
                                 , "pt " ++ pdStyle pd
                                 , "ps " ++ pdSize pd ]
  where quote s = "\"" ++ s ++ "\""

-- We apply the argument to the function here.  If we do it at the
-- call site then GHC may float it outside of the timing loop!
-- Therefore it's important that this function not be inlined.
-- It seems it's also important for it to return IO so as not to be
-- floated outside the timing loop.
{-# NOINLINE evaluate #-}
evaluate :: (e -> a) -> e -> IO ()
evaluate a e = let !_ = a e
                     in return ()

seqHashResult :: Expr h a -> ()
seqHashResult = flip seq ()

times :: (Ord a, Num a, Monad m) => a -> s -> (s -> m s) -> m s
times n s f = times_f 0 s
  where times_f m s_ =
          if m >= n
          then return s_
          else do
            s' <- f s_
            times_f (m + 1) s'

enumFrom1 :: [a] -> [(Int, a)]
enumFrom1 = zip [1..]

loop :: Monad m => a -> (a -> m (Either a b)) -> m b
loop a f = do
  e <- f a
  case e of
    Left a' -> loop a' f
    Right b -> pure b
