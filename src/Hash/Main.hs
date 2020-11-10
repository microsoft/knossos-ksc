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

import qualified Benchmark
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

algorithms :: Algorithms (String, Expr h String -> Expr Hash.Hash String)
algorithms = Algorithms
  { aNaiveNested    = ("Naive nested", Hash.naiveHashNested)
  , aDeBrujin       = ("de Bruijn", Hash.deBruijnHash)
  , aDeBruijnNested = ("de Bruijn nested", Hash.deBruijnNestedHash)
  , aCastHashOpt    = ("CAST Hash Optimized", Hash.castHashOptimized)
  }

expressions :: Expressions String
expressions = Expressions
  { eMnistCNN = "mnistcnn"
  , eGMMRev   = "gmm-rev"
  , eGMM      = "gmm"
  }

testcase_path :: String -> FilePath
testcase_path = \name -> "./exprs/" ++ name ++ ".expr"

process_stats :: Benchmark.AggregateStatistics -> (Int, Int)
process_stats aggregate_stats =
  let (_, mean, _, _, stddev) = Benchmark.stats aggregate_stats in (round mean, round stddev)

main :: IO ()
main = do
  getArgs >>= \case
    ["manual"] -> do
      loadedExpressions <- flip traverse expressions $ \name -> do
        expr <- Benchmark.readExpr (testcase_path name)
        pure (name, expr)

      putStrLn "Expressions:"
      flip Data.Foldable.traverse_ loadedExpressions $ \(name, expr) -> do
        putStrLn (name ++ " (size " ++ show (exprSize expr) ++ ")")
      putStrLn ""

      flip Data.Foldable.traverse_ algorithms $ \(algorithmName, algorithm) -> do
        putStrLn ("Algorithm " ++ algorithmName ++ ": ")

        flip Data.Foldable.traverse_ loadedExpressions $ \(name, expr) -> do
          putStr (name ++ ": ")
          stats_ <- Benchmark.benchmarkOne 50 50 (Benchmark.seqHashResult . algorithm) expr
          print (process_stats stats_)
        putStrLn ""

    ["random"] -> Benchmark.benchmark
    ["test"] -> Hash.testEverythingInFileStartingWith'prop_'
    _ -> putStrLn "Unsupported argument"
