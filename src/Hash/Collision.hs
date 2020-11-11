module Collision where

import qualified Hash

import qualified Benchmark
import Expr
import qualified KATHashFastOrigHash

import Data.Bits
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Control.Monad (when)

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen


genNotAlphaEquiv :: (MonadGen m, Hashable a, Ord a, Eq h)
                 => m (Expr h a)
                 -> m (Expr h a, Expr h a)
genNotAlphaEquiv gen = do
  expr1 <- gen
  expr2 <- gen

  when (Hash.alphaEquivalentAccordingToUniquifyBinders expr1 expr2) Gen.discard

  pure (expr1, expr2)

collisions :: IO ()
collisions = do
  let count = 100

  (_, m) <- Benchmark.times count (0 :: Int, Map.fromList (map (\i -> (i :: Int, 0 :: Int)) [0..64])) $ \(loopCount, m) -> do
    when (loopCount `mod` 100 == 0) $
      putStrLn ("Iteration " ++ show loopCount ++ "/" ++ show count
               ++ " (" ++ show (fromIntegral loopCount / fromIntegral count * 100 :: Double)
               ++ "% complete)")

    (expr1, expr2) <- Gen.sample (genNotAlphaEquiv (Hash.genExprNumVars' 10))

    let h1 = annotation (KATHashFastOrigHash.katHash expr1)
        h2 = annotation (KATHashFastOrigHash.katHash expr2)

        bitsize = 64

        m'' = (\f -> foldl' f m [0..bitsize]) $ \m' i ->
                if h1 `shiftL` (bitsize - i) == h2 `shiftL` (bitsize - i)
                then Map.adjust (+1) i m'
                else m'

    pure (loopCount + 1, m'')

  let log2 :: Float -> Float
      log2 x = log x / log 2

  let m''' = Map.mapWithKey (\_ n -> -log2 (proportion n)) m
        where proportion n = fromIntegral n / fromIntegral count

  flip mapM_ (Map.toList m''') $ \(width, logProb) ->
    putStrLn ("Bit width " ++ show width ++ ": "
              ++ "Proportion 1/2 ^ " ++ show logProb ++ " hashes collide."
             ++ "  Uniformly random would be 1/2 ^ " ++ show width)
