{-# LANGUAGE LambdaCase #-}

module KATHash where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import Expr (Expr(Var, Lam, App))
import Merge
import KATHash3 (Positions(..), Structure(..))

removeFromVM3 :: Ord v => v -> Map v p -> (Map v p, Maybe p)
removeFromVM3 v m = (Map.delete v m, Map.lookup v m)

unionVM3 :: Ord k
         => Map k Positions
         -> Map k Positions
         -> Map k Positions
unionVM3 = mergeMaps
            (\case
                LeftOnly l -> LeftOnlyPL l
                RightOnly r -> RightOnlyPL r
                Both l r -> BothPL l r
            )

findSingleton3 :: Map p Positions -> p
findSingleton3 m = case Map.toList m of
  [(v, HerePL)] -> v
  [(_, _)] -> error "Expected HerePL"
  [] -> error "Expected map to be non-empty"
  _:_:_ -> error "Expected map not to have multiple elements"

extendVM :: Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

data Dir = DirL | DirR
type Prefix = [Dir]
type UnprefixPositions = (Int, Positions)

consPrefix :: Dir -> Prefix -> Prefix
consPrefix = (:)

lengthPrefix :: Prefix -> Int
lengthPrefix = length

applyPrefix :: Prefix -> UnprefixPositions -> Positions
applyPrefix prefix (skip, positions) = applyPrefixNoSkip taken positions
  where n = length prefix
        toTake = n - skip
        taken = if toTake < 0
                then error "toTake < 0"
                else take toTake prefix

applyPrefixNoSkip :: Prefix -> Positions -> Positions
applyPrefixNoSkip prefix positions =
  foldr (\case DirL -> LeftOnlyPL
               DirR -> RightOnlyPL)
        positions
        prefix

unionVMFast :: Ord k
            => (Prefix, Map k UnprefixPositions)
            -> (Prefix, Map k UnprefixPositions)
            -> (Prefix, Map k UnprefixPositions)
unionVMFast ml@(_, map1) mr@(_, map2) = (new_prefix, new_map)
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

summariseExprFast :: Ord name
                  => Expr name
                  -> (Structure, (Prefix, Map name UnprefixPositions))
summariseExprFast = \case
  Var v   -> (SVar, ([], Map.singleton v (0, HerePL)))
  Lam x e ->
    let (str_body, (prefix, map_body)) = summariseExprFast e
        (e_map, mskip_pos) = removeFromVM3 x map_body
    in (SLam (fmap (applyPrefix prefix) mskip_pos) str_body, (prefix, e_map))
  App e1 e2 ->
    let (str1, map1) = summariseExprFast e1
        (str2, map2) = summariseExprFast e2
    in (SApp str1 str2, unionVMFast map1 map2)

fastTo3 :: (Structure, (Prefix, Map name UnprefixPositions))
        -> (Structure, Map name Positions)
fastTo3 (structure, (prefix, m)) = (structure, m')
  where m' = Map.map (applyPrefix prefix) m
