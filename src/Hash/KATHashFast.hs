{-# LANGUAGE LambdaCase #-}

module KATHashFast where

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import Expr (Expr(Var, Lam, App))
import KATHash3 (Positions(..), Structure(..))

{- Positions and Structure reused from KATHash3

data Positions
  = HerePL
  | LeftOnlyPL Positions
  | RightOnlyPL Positions
  | BothPL Positions Positions
  deriving (Eq, Show)

data Structure
  = SVar
  | SLam (Maybe Positions) Structure
  | SApp Structure Structure
  deriving (Eq, Show)

-}

removeFromVM :: Ord v => v -> Map v p -> (Map v p, Maybe p)
removeFromVM v m = (Map.delete v m, Map.lookup v m)

unionVM :: Ord k
        => (Prefix, Map k UnprefixPositions)
        -> (Prefix, Map k UnprefixPositions)
        -> (Prefix, Map k UnprefixPositions)
unionVM ml@(_, map1) mr@(_, map2) = (new_prefix, new_map)
  where ((prefix_bigger, map_bigger), (prefix_smaller, map_smaller),
         next_prefix_bigger, bothPLBiggerFirst, smallerOnlyPL) =
          if Map.size map1 > Map.size map2
          then (ml, mr, DirL, BothPL, RightOnlyPL)
          else (mr, ml, DirR, flip BothPL, LeftOnlyPL)

        new_map = foldl' (flip add_smaller) map_bigger (Map.toList map_smaller)

        add_smaller (v, vv_smaller) = Map.alter (f vv_smaller) v
        f vv_smaller x = Just (lengthPrefix new_prefix, new_positions)
          where new_positions = case x of
                  Nothing -> smallerOnlyPL (applyPrefix prefix_smaller vv_smaller)
                  Just vv_bigger -> bothPLBiggerFirst
                                        (applyPrefix prefix_bigger vv_bigger)
                                        (applyPrefix prefix_smaller vv_smaller)

        new_prefix = next_prefix_bigger `consPrefix` prefix_bigger

data Dir = DirL | DirR
type Prefix = [Dir]

type UnprefixPositions = (Int, Positions)

consPrefix :: Dir -> Prefix -> Prefix
consPrefix = (:)

lengthPrefix :: Prefix -> Int
lengthPrefix = length

emptyPrefix :: Prefix
emptyPrefix = []

hereUnprefixPositions :: UnprefixPositions
hereUnprefixPositions = (0, HerePL)

-- `applyPrefix prefix (toIgnore, positions)` applies `prefix` to
-- `positions`, except it ignores the last `toIgnore` elements of
-- `prefix`.
applyPrefix :: Prefix -> UnprefixPositions -> Positions
applyPrefix prefix (toIgnore, positions) =
  applyAllPrefix (dropFromEnd toIgnore prefix)
  where dropFromEnd n xs = if toTake < 0
                           then error "toTake < 0"
                           else take toTake xs
          where toTake = length xs - n

        applyAllPrefix :: Prefix -> Positions
        applyAllPrefix = foldr (\case DirL -> LeftOnlyPL
                                      DirR -> RightOnlyPL)
                               positions

prop_applyPrefix :: TestLimit -> Property
prop_applyPrefix count = withTests count $ property $ do
  p <- forAll genPosition
  let prefix = [DirL, DirR, DirR, DirR]

  applyPrefix prefix (0, p) === LeftOnlyPL (RightOnlyPL (RightOnlyPL (RightOnlyPL p)))
  applyPrefix prefix (1, p) === LeftOnlyPL (RightOnlyPL (RightOnlyPL p))
  applyPrefix prefix (2, p) === LeftOnlyPL (RightOnlyPL p)
  applyPrefix prefix (3, p) === LeftOnlyPL p
  applyPrefix prefix (4, p) === p

-- This fast version has the desirable property that neither the
-- Positions nor Structure from the earlier version need to change.
--
-- Instead of changing Positions and Structure it adds more to the
-- return value.  The original version returned
--
--     (Structure, Map name Positions)
--
-- This one returns
--
--     (Structure, (Prefix, Map name PrefixPositions))
--
-- (Prefix, Map name PrefixPositions) encodes exactly the same
-- information as `Map name Positions` but does so in a way that is
-- amenable to merging by "accumulation into the smaller map".
--
-- If you want to recover a `Map name Positions` from a `(Prefix, Map
-- name PrefixPositions)` then you can use `prefixMapToMap`.
summariseExpr :: Ord name
              => Expr name
              -> (Structure, (Prefix, Map name UnprefixPositions))
summariseExpr = \case
  Var v   -> (SVar, (emptyPrefix, Map.singleton v hereUnprefixPositions))
  Lam x e ->
    let (str_body, (prefix, map_body)) = summariseExpr e
        (e_map, mskip_pos) = removeFromVM x map_body
    in (SLam (fmap (applyPrefix prefix) mskip_pos) str_body, (prefix, e_map))
  App e1 e2 ->
    let (str1, map1) = summariseExpr e1
        (str2, map2) = summariseExpr e2
    in (SApp str1 str2, unionVM map1 map2)

prefixMapToMap :: (Prefix, Map k UnprefixPositions)
               -> Map k Positions
prefixMapToMap (prefix, m) = Map.map (applyPrefix prefix) m

-- We don't implement rebuild here, we just show that the result of
-- KATHash3.summariseExpr can be recovered from the result of
-- KATHashFast.summariseExpr
fastTo3 :: (Structure, (Prefix, Map name UnprefixPositions))
        -> (Structure, Map name Positions)
fastTo3 (structure, prefixMap) = (structure, prefixMapToMap prefixMap)

genPosition :: Gen Positions
genPosition = Gen.recursive
  Gen.choice
  [ pure HerePL ]
  [ Gen.subterm genPosition LeftOnlyPL
  , Gen.subterm genPosition RightOnlyPL
  , Gen.subterm2 genPosition genPosition BothPL
  ]
