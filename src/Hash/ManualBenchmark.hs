{-# LANGUAGE DeriveTraversable #-}

module ManualBenchmark where

import qualified Benchmark
import Expr (exprSize)

import qualified Data.Foldable

data Expressions a = Expressions
  { eMnistCNN :: a
  , eGMMRev   :: a
  , eGMM      :: a
  , eBERTs    :: [a]
  }
  deriving (Functor, Foldable, Traversable)

expressions :: Expressions String
expressions = Expressions
  { eMnistCNN = "mnistcnn"
  , eGMMRev   = "gmm-rev"
  , eGMM      = "gmm"
  , eBERTs    = map (\i -> "bert/bert" ++ show i) [1 :: Int .. 23]
  }

testcase_path :: String -> FilePath
testcase_path = \name -> "./exprs/" ++ name ++ ".expr"

process_stats :: Benchmark.AggregateStatistics -> (Int, Int)
process_stats aggregate_stats =
  let (_, mean, _, _, stddev) = Benchmark.stats aggregate_stats in (round mean, round stddev)

specific_benchmarks :: IO ()
specific_benchmarks = do
      loadedExpressions <- flip traverse expressions $ \name -> do
        expr <- Benchmark.readExpr (testcase_path name)
        pure (name, expr)

      putStrLn "Expressions:"
      flip Data.Foldable.traverse_ loadedExpressions $ \(name, expr) -> do
        putStrLn (name ++ " (size " ++ show (exprSize expr) ++ ")")
      putStrLn ""

      flip Data.Foldable.traverse_ Benchmark.algorithms_ $ \(algorithmName, algorithm, _) -> do
        putStrLn ("Algorithm " ++ algorithmName ++ ": ")

        flip Data.Foldable.traverse_ loadedExpressions $ \(name, expr) -> do
          putStr (name ++ ": ")
          stats_ <- Benchmark.benchmarkOne 50 50 (Benchmark.seqHashResult . algorithm) expr
          print (process_stats stats_)
        putStrLn ""
