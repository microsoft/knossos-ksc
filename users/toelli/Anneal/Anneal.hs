-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
module Anneal where

import qualified System.Random
import qualified Expression
import           Control.Monad (when)

-- https://en.wikipedia.org/wiki/Simulated_annealing#Pseudocode
step :: (s -> Double)
     -> (s -> [s])
     -> Double
     -> s
     -> IO s
step energy neighbours temperature state = do
  candidate <- randomChoice (neighbours state)
  x <- System.Random.randomRIO (0,1)

  return $ if p state candidate temperature >= x
           then candidate
           else state

  where p from to t = exp(-(energy to - energy from) / t)
  
randomChoice :: [a] -> IO a
randomChoice as = do
  i <- System.Random.randomRIO (0, length as - 1)
  return (as !! i)

exampleStep :: Double -> [Bool] -> IO [Bool]
exampleStep = step energy neighbours
  where neighbours [] = [[True], [False]]
        neighbours all_@(_:rest) = [rest, True:all_, False:all_]

energy :: [Bool] -> Double
energy s = -fromIntegral (length (filter id s) `min` 4) / (fromIntegral (length s) + 1)

examples :: Int -> Double -> [Bool] -> IO ()
examples remaining temperature state =
  if remaining == 0
  then return ()
  else do
    next <- exampleStep temperature state
    print (energy next)
    print next
    examples (remaining - 1) (temperature * 0.99) next

examplesE :: Double -> Int -> Double -> Expression.ExpressionZ -> IO ()
examplesE minSeen remaining temperature state =
  if remaining == 0
  then return ()
  else do
    next <- step Expression.energy Expression.neighbours temperature state
    let minSeen' = min minSeen (Expression.energy next)

    when ((remaining `mod` (100 * 1000)) == 0) $ do
      print minSeen'
      print (Expression.energy next)
      print next

    examplesE minSeen' (remaining - 1) temperature next
