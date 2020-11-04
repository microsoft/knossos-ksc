-- To build this, follow the "Compiling the ksc executable"
-- instructions from the ksc README, with the following changes
--
-- 1. Run it in the "Hash" directory
--
-- 2. The hash executable is called "hash" so if it still exists you
-- need to "rm hash" instead of "rm ksc".
--
--    https://github.com/microsoft/knossos-ksc#compiling-the-ksc-executable

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Benchmark
import Expr (exprSize, Expr, Path)
import Hash (castHashOptimized, deBruijnHash, deBruijnNestedHash, naiveHashNested, Hash)


testcase_names :: [String]
testcase_names = ["mnistcnn", "gmm-rev", "gmm"]

testcase_paths :: [FilePath]
testcase_paths =
  map (\name -> "./exprs/" ++ name ++ ".expr") testcase_names

process_stats :: Benchmark.AggregateStatistics -> (Int, Int)
process_stats aggregate_stats =
  let (_, mean, _, _, stddev) = Benchmark.stats aggregate_stats in (round mean, round stddev)

print_expr_sizes :: [FilePath] -> IO ()
print_expr_sizes paths =
  let exprs :: IO [Expr String] = sequence (map readExpr paths) in
    fmap (map exprSize) exprs
    >>= print

print_stats_row :: (Expr String -> [(Hash, Path, Expr a)]) -> IO ()
print_stats_row algorithm =
  let testcases = map (\path -> (path, ())) testcase_paths
      result = Benchmark.benchmarkManyReadFile testcases 50 50 (seqHashResult . algorithm)
  in
    fmap (map (\(aggregate_stats, ()) -> process_stats aggregate_stats)) result
    >>= print


main :: IO ()
main =
  print testcase_names
  >> print_expr_sizes testcase_paths
  >> print_stats_row naiveHashNested
  >> print_stats_row deBruijnHash
  >> print_stats_row deBruijnNestedHash
  >> print_stats_row castHashOptimized
