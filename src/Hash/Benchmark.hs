{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

module Benchmark where

import qualified Hedgehog.Gen as Gen
import qualified Data.Foldable
import Data.Hashable (Hashable)
import Data.List (intercalate)
import qualified System.Clock as Clock
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.IO.Temp (createTempDirectory, emptyTempFile)

import Expr (Expr, exprSize)
import Hash (castHashOptimized, deBruijnHash,  structuralHashNested)
import qualified Hash
import qualified KATHashFastOrigHash
import qualified KATHashFasterOrigHash

data BenchmarkConfig = BenchmarkConfig
  { bcGenExpr              :: Int -> IO (Expr () Int)
  , bcGenName              :: String
  , bcTotalExpressions     :: Int
  , bcSamplesPerExpression :: Int
  , bcIterationsPerSample  :: Int
  }

data Algorithms a = Algorithms
  { aLocallyNameless        :: a
  , aCastHashOptimized      :: a
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
  , aCastHashOptimized  = ("CAST Optimized", castHashOptimized, "black")
  , aKATHashFromPaper   = ("KATHash as in paper", KATHashFastOrigHash.katHash, good)
  , aKATHashFromPaperFaster = ("KATHash optimized", KATHashFasterOrigHash.katHash, "yellow")
  , aDeBrujinHash           = ("de Bruijn*", deBruijnHash, prettyBad)
  , aStructuralHashNested   = ("Structural*", structuralHashNested, veryBad)
  -- , aDeBruijnNested     = ("de Bruijn nested", Hash.deBruijnNestedHash, "magenta")
  -- , aCastHash           = ("Compositional", castHash,   "green")
  -- , aCombinedHash       = ("Combined", combinedHash,    "blue")
  }
  where
      veryBad   = "red"
      prettyBad = "orange"
      good      = "green"
      baseline  = "blue"

-- | This is the entry point to the module.  When run it will
-- benchmark the algorithms on a random set of expressions.  The data
-- from the run will be written out to a directory whose name is
-- displayed at the end of the run.
benchmark :: IO ()
benchmark = do
  let bcs = [ BenchmarkConfig
              { bcGenExpr = Gen.sample . Gen.resize 10 . Hash.genExprLinearNumVars
              , bcGenName = "unbalanced expressions (old generator)"
              , bcTotalExpressions     = totalExpressions
              , bcSamplesPerExpression = 20
              , bcIterationsPerSample  = 20
              }
            , BenchmarkConfig
              { bcGenExpr = Gen.sample . Gen.resize 10 . Hash.genExprLinearNumVars'
              , bcGenName = "unbalanced expressions (new generator)"
              , bcTotalExpressions     = totalExpressions
              , bcSamplesPerExpression = 20
              , bcIterationsPerSample  = 20
              }
            , BenchmarkConfig
              { bcGenExpr = Gen.sample . Gen.resize 15 . Hash.genExprNumVars'
              , bcGenName = "balanced expressions (old generator)"
              , bcTotalExpressions     = totalExpressions
              , bcSamplesPerExpression = 20
              , bcIterationsPerSample  = 20
              }
            , BenchmarkConfig
              { bcGenExpr = Gen.sample . Gen.resize 15 . Hash.genExprNumVars
              , bcGenName = "balanced expressions (new generator)"
              , bcTotalExpressions     = totalExpressions
              , bcSamplesPerExpression = 20
              , bcIterationsPerSample  = 20
              }
            ]

      totalExpressions = 100

      algorithms = Data.Foldable.toList algorithms_
      varCounts = [ (10, "1") {-, (100, "4")-} ]

  benchmarksDir <- createTempDirectory "." "benchmarks"
  results_genNames <- flip mapM bcs $ \bc -> do
    results <- benchmarkThis benchmarksDir algorithms varCounts bc
    pure (results, bcGenName bc)
  flip mapM_ results_genNames $ \(results, genName) ->
    makeGnuplot benchmarksDir genName results

benchmarkThis :: FilePath
              -> [(String, Expr () Int -> Expr hash string, String)]
              -> [(Int, String)]
              -> BenchmarkConfig
              -> IO [PlotDataset]
benchmarkThis benchmarksDir algorithms varCounts bc = do
  let allParams = (,) <$> algorithms <*> varCounts

      enumFrom1 :: [a] -> [(Int, a)]
      enumFrom1 = zip [1..]

  results <- flip mapM (enumFrom1 allParams) $ \(i, (algorithm_, var_)) -> do
    let (varCount, varCountSymbol) = var_
        (algorithmName, algorithm, algorithmColor) = algorithm_
    results <- times (bcTotalExpressions bc) [] $ \rest -> do
      -- We force the expression after generating it.  The Expr type
      -- is strict, that is forcing it forces everything it contains,
      -- therefore no time is wasted forcing it in the hashing
      -- algorithm itself.  On the other hand adding this bang pattern
      -- made absolutely no difference to the benchmarks.  The
      -- expression must be generated already forced.  But it's nice
      -- to keep this hear for clarity.
      !expression <- bcGenExpr bc varCount

      r <- benchmarkOne (bcSamplesPerExpression bc)
                        (bcIterationsPerSample bc)
                        (seqHashResult . algorithm)
                        expression

      putStrLn ("Parameter set "
                 ++ show i ++ "/" ++ show (length allParams)
                 ++ " (" ++ algorithmName ++ ")")
      putStrLn ("Generated " ++ show (length rest)
                ++ " out of " ++ show (bcTotalExpressions bc) ++ " expressions")

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

    filename <- emptyTempFile benchmarksDir (algorithmName ++ show varCount ++ ".dat")

    writeFile filename textOutput

    return PlotDataset
      { pdFile  = filename
      , pdTitle = algorithmName ++ " " ++ show varCount ++ " variables"
      , pdColor = algorithmColor
      , pdStyle = varCountSymbol
      }

  pure results

makeGnuplot :: FilePath -> String -> [PlotDataset] -> IO ()
makeGnuplot benchmarksDir xlabel results = do
  gnuplotFilename <- emptyTempFile benchmarksDir "benchmarks.gnuplot"
  gnuplotPngFilename <- emptyTempFile benchmarksDir "benchmarks-png.gnuplot"

  let gnuplotFileContent = gnuplotFile xlabel results
      (outputPng, gnuplotPngFileContent) = gnuplotFilePng benchmarksDir xlabel results

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
             -> Int
             -> (e -> r)
             -> e
             -> IO AggregateStatistics
benchmarkOne = benchmarkMore (0, 0, 0, infinity)
  where infinity = 1e60

benchmarkMore :: AggregateStatistics
              -> Int
              -> Int
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
                     -> Int
                     -> (e -> r)
                     -> IO AggregateStatistics
benchmarkOneReadFile filepath samplesPerExpression iterationsElapsed algorithm = do
  expr <- readExprG filepath

  benchmarkOne samplesPerExpression iterationsElapsed algorithm expr

gnuplotFilePng :: String
               -> String
               -> [PlotDataset]
               -> (String, String)
gnuplotFilePng benchmarksDir xlabel results = (outputPng, unlines [
    "set terminal pngcairo size 1024,768"
  , "set output \"" ++ outputPng ++ "\""
  , gnuplotFile xlabel results
  ])
  where outputPng = benchmarksDir ++ "/benchmark.png"

gnuplotFile :: String -> [PlotDataset] -> String
gnuplotFile xlabel results =
  unlines [ "set xlabel \"Number of nodes in expression (" ++ xlabel ++ ")\""
          , "set ylabel \"Time taken to hash all subexpressions / us"
          , "set logscale xy 2"
          , "set key left top"
          , "set xrange [64:]"
          , "plot " ++ intercalate ", " (fmap plotDataset results)
          ]

data PlotDataset = PlotDataset
  { pdFile   :: String
  , pdTitle  :: String
  , pdColor  :: String
  , pdStyle  :: String
  }

plotDataset :: PlotDataset -> String
plotDataset pd = intercalate " " [ quote (pdFile pd)
                                 , "title " ++ quote (pdTitle pd)
                                 , "lt rgb " ++ quote (pdColor pd)
                                 , "pt " ++ pdStyle pd ]
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

times :: Monad m => Int -> s -> (s -> m s) -> m s
times n s f = times_f 0 s
  where times_f m s_ =
          if m >= n
          then return s_
          else do
            s' <- f s_
            times_f (m + 1) s'