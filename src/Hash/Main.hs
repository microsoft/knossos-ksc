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

import qualified Collision
import qualified Benchmark
import qualified ManualBenchmark
import qualified Hash

import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= \case
    ["manual"] -> ManualBenchmark.specific_benchmarks
    ["random", "fast"] -> Benchmark.benchmark Benchmark.fast
    ["random", "full"] -> Benchmark.benchmark Benchmark.full
    ["test"] -> Hash.testEverythingInFileStartingWith'prop_'
    ["collisions", "fast"] -> Collision.collisions 1
    ["collisions", "full"] -> Collision.collisions 10
    _ -> putStrLn "Unsupported argument"
