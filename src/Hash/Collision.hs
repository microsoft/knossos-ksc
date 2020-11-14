module Collision where

import qualified Hash

import qualified Benchmark
import Expr
import Hash (Hash)
import qualified KATHashFastOrigHash

import Data.Bits
import Data.Function (on)
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
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

restrictToBits :: Int -> Hash -> Hash
restrictToBits i h = h `shiftL` (bitsize - i)
  where bitsize = finiteBitSize (0 :: Hash)

collisions :: IO ()
collisions = do
  let maxBits = 16
      count   = 2 ^ (maxBits + 2)
      emptyMap :: Map.Map Int Int
      emptyMap = Map.fromList (map (\i -> (i, 0)) [0..maxBits])

  (_, m) <- Benchmark.times count (0 :: Int, emptyMap) $ \(loopCount, m) -> do
    when (loopCount `mod` 100 == 0) $ do
      let percentComplete :: Double
          percentComplete = fromIntegral loopCount / fromIntegral count * 100

      putStrLn ("Iteration " ++ show loopCount ++ "/" ++ show count
               ++ " (" ++ show percentComplete ++ "% complete)")

    (expr1, expr2) <- Gen.sample (genNotAlphaEquiv (Hash.genExprNumVars' 10))

    let h1 = annotation (KATHashFastOrigHash.katHash expr1)
        h2 = annotation (KATHashFastOrigHash.katHash expr2)

        m'' = Map.mapWithKey (\i v -> if ((==) `on` restrictToBits i) h1 h2
                                      then v+1
                                      else v)
                            m

    pure (loopCount + 1, m'')

  let log2 :: Float -> Float
      log2 x = log x / log 2

  let m''' = Map.mapWithKey (\_ n -> -log2 (proportion n)) m
        where proportion n = fromIntegral n / fromIntegral count

  flip mapM_ (Map.toList m''') $ \(width, logProb) ->
    putStrLn ("Bit width " ++ show width ++ ": "
              ++ "Proportion 1/2 ^ " ++ show logProb ++ " hashes collide."
             ++ "  Uniformly random would be 1/2 ^ " ++ show width)
