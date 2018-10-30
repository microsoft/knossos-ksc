module MCTS where

import           Data.List  (maximumBy, (\\))
import           Data.Maybe (catMaybes)
import           Data.Ord   (comparing)
import qualified Lens.Micro as L
import qualified System.Random

data Node s = Node { numVisits         :: Int
                   , rewards           :: Double
                   , state             :: s
                   , unvisitedChildren :: [s]
                   , visitedChildren   :: [Node s]
                   }
              deriving Show

unvisitedChildrenL :: L.Lens' (Node s) [s]
unvisitedChildrenL = L.lens unvisitedChildren (\n a -> n { unvisitedChildren = a })

visitedChildrenL :: L.Lens' (Node s) [Node s]
visitedChildrenL = L.lens visitedChildren (\n a -> n { visitedChildren = a })

numVisitsL :: L.Lens' (Node s) Int
numVisitsL = L.lens numVisits (\n a -> n { numVisits = a })

rewardsL :: L.Lens' (Node s) Double
rewardsL = L.lens rewards (\n a -> n { rewards = a })

data Rose a = Rose a [Rose a]

showRose :: (a -> String) -> Rose a -> [String]
showRose show' (Rose a rs) =
  show' a : map ("    " ++ ) (concatMap (showRose show') rs)

roseOfNode :: Show s => Node s -> Rose String
roseOfNode node = Rose (show (state node)
                        ++ " " ++ show (numVisits node)
                        ++ " " ++ show (rewards node))
                       (map roseOfNode (visitedChildren node))

uct :: Double -> Int -> Node s -> Double
uct cp nParent node = rj / nj + cp * sqrt (ln n / nj)
  where n = fromIntegral nParent
        rj = rewards node
        nj = fromIntegral (numVisits node)
        ln = log

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

mcts :: (Monad m, Eq state)
     => Double
     -> (state -> [state] -> m state)
     -> (state -> m simulation)
     -> (simulation -> (Node state, Double))
     -> Node state
     -> m (Double, Node state)
mcts cp biasedDraw simulation expansion = step
  where step node = do
          (reward, updatedNode) <- case unvisitedChildren node of
                [] -> do
                  let n = numVisits node
                      t = visitedChildren node
                      (idxBest, best) = maximumBy (comparing (uct cp n . snd)) (enumerate t)
                  (reward, updatedBest) <- step best
                  let updatedNode = L.set (visitedChildrenL . L.ix idxBest) updatedBest node
                  return (reward, updatedNode)
                s@(_:_) -> do
                  s' <- biasedDraw (state node) s
                  sim <- simulation s'

                  let (node', reward) = expansion sim
                      updatedNode = (L.over unvisitedChildrenL (\\ [s'])
                                     . L.over visitedChildrenL (node' :))
                                    node

                  return (reward, updatedNode)

          let updated2Node = (L.over numVisitsL (+ 1)
                              . L.over rewardsL (+ reward))
                             updatedNode

          return (reward, updated2Node)

example :: Node [Bool] -> IO (Double, Node [Bool])
example = mcts cp biasedDraw simulation expansion
  where cp = 1
        biasedDraw _ [a, b] = do
          r <- System.Random.randomIO
          return (if r then a else b)

        biasedDraw _ [a] = return a

        simulation = return

        expansion s = (n, reward)
          where n = Node { numVisits         = 1
                         , rewards           = 0
                         , state             = s
                         , unvisitedChildren = [True:s, False:s]
                         , visitedChildren   = []
                         }

                reward = fromIntegral (length (filter id s) `min` 4) / (fromIntegral (length s) + 1)

startNode :: Node [Bool]
startNode = Node { numVisits         = 1
                 , rewards           = 0
                 , state             = []
                 , unvisitedChildren = [[True], [False]]
                 , visitedChildren   = []
                 }

checkAll :: Node a -> [String]
checkAll node = check node ++ concatMap checkAll (visitedChildren node)

check :: Node a -> [String]
check node = wrong
  where wrong = catMaybes [
          if numVisits node /= sum (map numVisits (visitedChildren node)) + 1
          then Just "Wrong number of visits"
          else Nothing,
          if rewards node ~< sum (map rewards (visitedChildren node))
          then Just "Wrong rewards"
          else Nothing
          ]
        x ~< y = (x - y) < (-0.000001)

test :: IO [String]
test = fmap checkAll (loop 1000 (fmap snd . example) startNode)
  where loop 0 _ a = return a
        loop n f a = loop (n - 1) f =<< f a
