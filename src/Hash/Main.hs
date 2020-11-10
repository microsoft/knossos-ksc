-- To build this, follow the "Compiling the ksc executable"
-- instructions from the ksc README, with the following changes
--
-- 1. Run it in the "Hash" directory
--
-- 2. The hash executable is called "hash" so if it still exists you
-- need to "rm hash" instead of "rm ksc".
--
--    https://github.com/microsoft/knossos-ksc#compiling-the-ksc-executable

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Benchmark
import Expr (exprSize, Expr)
import qualified Data.Foldable
import qualified Hash

import System.Environment (getArgs)

data Algorithms a = Algorithms
  { aNaiveNested    :: a
  , aDeBrujin       :: a
  , aDeBruijnNested :: a
  , aCastHashOpt    :: a
  }
  deriving (Functor, Foldable, Traversable)

data Expressions a = Expressions
  { eMnistCNN :: a
  , eGMMRev   :: a
  , eGMM      :: a
  }
  deriving (Functor, Foldable, Traversable)

algorithms :: Algorithms (Expr h String -> Expr Hash.Hash String)
algorithms = Algorithms
  { aNaiveNested    = Hash.naiveHashNested
  , aDeBrujin       = Hash.deBruijnHash
  , aDeBruijnNested = Hash.deBruijnNestedHash
  , aCastHashOpt    = Hash.castHashOptimized
  }

expressions :: Expressions String
expressions = Expressions
  { eMnistCNN = "mnistcnn"
  , eGMMRev   = "gmm-rev"
  , eGMM      = "gmm"
  }

testcase_names :: [String]
testcase_names = Data.Foldable.toList expressions

testcase_paths :: [FilePath]
testcase_paths =
  map (\name -> "./exprs/" ++ name ++ ".expr") testcase_names

process_stats :: Benchmark.AggregateStatistics -> (Int, Int)
process_stats aggregate_stats =
  let (_, mean, _, _, stddev) = Benchmark.stats aggregate_stats in (round mean, round stddev)

print_expr_sizes :: [FilePath] -> IO ()
print_expr_sizes paths = do
  exprs <- traverse readExpr paths
  print (map exprSize exprs)

print_stats_row :: (Expr () String -> Expr Hash.Hash String) -> IO ()
print_stats_row algorithm = do
  result <- flip traverse testcase_paths (\t -> Benchmark.benchmarkOneReadFile t 50 50 (seqHashResult . algorithm))
  print (map process_stats result)

main :: IO ()
main = do
  getArgs >>= \case
    ["manual"] -> do
      print testcase_names
      print_expr_sizes testcase_paths
      mapM_ print_stats_row algorithms
    ["random"] -> Benchmark.benchmark
    ["test"] -> Hash.testEverythingInFileStartingWith'prop_'
    _ -> putStrLn "Unsupported argument"
