{-# LANGUAGE LambdaCase #-}

module KATHash where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Expr (Expr(Var, Lam, App))
import Merge

data Positions
  = EmptyPL
  | SinglePL
  | ShiftLeftPL Positions
  | ShiftRightPL Positions
  | UnionPL Positions Positions
  deriving (Eq, Show)

data Structure
  = SVar
  | SLam Positions Structure
  | SApp Structure Structure
  deriving (Eq, Show)

data Positions3
  = HerePL
  | LeftOnlyPL Positions3
  | RightOnlyPL Positions3
  | BothPL Positions3 Positions3
  deriving (Eq, Show)

data Structure3
  = SVar3
  | SLam3 (Maybe Positions3) Structure3
  | SApp3 Structure3 Structure3
  deriving (Eq, Show)

removeFromVM :: Ord v => v -> Map v Positions -> (Map v Positions, Positions)
removeFromVM v m = (Map.delete v m, fromMaybe EmptyPL (Map.lookup v m))

removeFromVMP :: Ord v
              => v
              -> Map v (Int, Positions)
              -> (Map v (Int, Positions), (Int, Positions))
removeFromVMP v m = (Map.delete v m, fromMaybe (0, EmptyPL) (Map.lookup v m))

removeFromVM3 :: Ord v => v -> Map v p -> (Map v p, Maybe p)
removeFromVM3 v m = (Map.delete v m, Map.lookup v m)

unionVM2 :: Ord k
         => Map k Positions
         -> Map k Positions
         -> Map k Positions
unionVM2 = mergeMaps
            (\case
                LeftOnly l -> ShiftLeftPL l
                RightOnly r -> ShiftRightPL r
                Both l r -> UnionPL l r
            )

unionVM3 :: Ord k
         => Map k Positions3
         -> Map k Positions3
         -> Map k Positions3
unionVM3 = mergeMaps
            (\case
                LeftOnly l -> LeftOnlyPL l
                RightOnly r -> RightOnlyPL r
                Both l r -> BothPL l r
            )

findSingleton2 :: Map p Positions -> p
findSingleton2 m = case (filter (isSinglePL2 . snd) . Map.toList) m of
  [(v, _)] -> v
  [] -> error "Expected map to be non-empty"
  _:_:_ -> error "Expected map not to have multiple elements"

findSingleton3 :: Map p Positions3 -> p
findSingleton3 m = case Map.toList m of
  [(v, HerePL)] -> v
  [(_, _)] -> error "Expected HerePL"
  [] -> error "Expected map to be non-empty"
  _:_:_ -> error "Expected map not to have multiple elements"

isSinglePL2 :: Positions -> Bool
isSinglePL2 = \case
  SinglePL -> True
  _ -> False

extendVM :: Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

pickL2 :: Positions -> Positions
pickL2 = \case
  ShiftLeftPL pl -> pl
  UnionPL pl _ -> pl
  _ -> EmptyPL

pickR2 :: Positions -> Positions
pickR2 = \case
  ShiftRightPL pr -> pr
  UnionPL _ pr -> pr
  _ -> EmptyPL

pickL3 :: Positions3 -> Maybe Positions3
pickL3 = \case
  LeftOnlyPL pl -> Just pl
  BothPL pl _ -> Just pl
  _ -> Nothing

pickR3 :: Positions3 -> Maybe Positions3
pickR3 = \case
  RightOnlyPL pr -> Just pr
  BothPL _ pr -> Just pr
  _ -> Nothing

summariseExprCorrectness2 :: Ord name
                          => Expr name
                          -> (Structure, Map name Positions)
summariseExprCorrectness2 = \case
  Var v   -> (SVar, Map.singleton v SinglePL)
  Lam x e ->
    let (str_body, map_body) = summariseExprCorrectness2 e
        (e_map, x_pos) = removeFromVM x map_body
    in (SLam x_pos str_body, e_map)
  App e1 e2 ->
    let (str1, map1) = summariseExprCorrectness2 e1
        (str2, map2) = summariseExprCorrectness2 e2
    in (SApp str1 str2, unionVM2 map1 map2)

rebuild2 :: Ord name
         => (name -> name)
         -> name
         -> (Structure, Map name Positions)
         -> Expr name
rebuild2 freshen fresh (structure, m) = case structure of
  SVar -> Var (findSingleton2 m)
  SLam p s -> Lam x (rebuild2 freshen fresher (s, extendVM m x p))
    where x = fresh
          fresher = freshen fresh
  SApp s1 s2 -> App (rebuild2 freshen fresh (s1, m1))
                    (rebuild2 freshen fresh (s2, m2))
    where m1 = Map.map pickL2 m
          m2 = Map.map pickR2 m

summariseExprCorrectness3 :: Ord name
                          => Expr name
                          -> (Structure3, Map name Positions3)
summariseExprCorrectness3 = \case
  Var v   -> (SVar3, Map.singleton v HerePL)
  Lam x e ->
    let (str_body, map_body) = summariseExprCorrectness3 e
        (e_map, x_pos) = removeFromVM3 x map_body
    in (SLam3 x_pos str_body, e_map)
  App e1 e2 ->
    let (str1, map1) = summariseExprCorrectness3 e1
        (str2, map2) = summariseExprCorrectness3 e2
    in (SApp3 str1 str2, unionVM3 map1 map2)

rebuild3 :: Ord name
         => (name -> name)
         -> name
         -> (Structure3, Map name Positions3)
         -> Expr name
rebuild3 freshen fresh (structure, m) = case structure of
  SVar3 -> Var (findSingleton3 m)
  SLam3 mp s -> Lam x (rebuild3 freshen fresher (s, m'))
    where x = fresh
          fresher = freshen fresh
          m' = case mp of Nothing -> m
                          Just p -> extendVM m x p
  SApp3 s1 s2 -> App (rebuild3 freshen fresh (s1, m1))
                     (rebuild3 freshen fresh (s2, m2))
    where m1 = Map.mapMaybe pickL3 m
          m2 = Map.mapMaybe pickR3 m

data Dir = DirL | DirR
type Prefix = [Dir]
type UnprefixPositions = (Int, Positions3)

consPrefix :: Dir -> Prefix -> Prefix
consPrefix = (:)

lengthPrefix :: Prefix -> Int
lengthPrefix = length

applyPrefix :: Prefix -> UnprefixPositions -> Positions3
applyPrefix prefix (skip, positions) = applyPrefixNoSkip taken positions
  where n = length prefix
        toTake = n - skip
        taken = if toTake < 0
                then error "toTake < 0"
                else take toTake prefix

applyPrefixNoSkip :: Prefix -> Positions3 -> Positions3
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
                  -> (Structure3, (Prefix, Map name UnprefixPositions))
summariseExprFast = \case
  Var v   -> (SVar3, ([], Map.singleton v (0, HerePL)))
  Lam x e ->
    let (str_body, (prefix, map_body)) = summariseExprFast e
        (e_map, mskip_pos) = removeFromVM3 x map_body
    in (SLam3 (fmap (applyPrefix prefix) mskip_pos) str_body, (prefix, e_map))
  App e1 e2 ->
    let (str1, map1) = summariseExprFast e1
        (str2, map2) = summariseExprFast e2
    in (SApp3 str1 str2, unionVMFast map1 map2)

fastTo3 :: (Structure3, (Prefix, Map name UnprefixPositions))
        -> (Structure3, Map name Positions3)
fastTo3 (structure, (prefix, m)) = (structure, m')
  where m' = Map.map (applyPrefix prefix) m
