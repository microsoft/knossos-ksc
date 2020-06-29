-- To build this, follow the "Compiling the ksc executable"
-- instructions from the ksc README, with the following changes
--
-- 1. Run it in the "Hash" directory
--
-- 2. The hash executable is called "hash" so if it still exists you
-- need to "rm hash" instead of "rm ksc".
--
--    https://github.com/microsoft/knossos-ksc#compiling-the-ksc-executable

module Main where

import Benchmark

main :: IO ()
main = Benchmark.benchmark
