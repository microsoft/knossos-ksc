{-# LANGUAGE LambdaCase #-}

module KATHash2 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

removeFromVM :: Ord v => v -> Map v Positions -> (Map v Positions, Positions)
removeFromVM v m = (Map.delete v m, fromMaybe EmptyPL (Map.lookup v m))

removeFromVMP :: Ord v
              => v
              -> Map v (Int, Positions)
              -> (Map v (Int, Positions), (Int, Positions))
removeFromVMP v m = (Map.delete v m, fromMaybe (0, EmptyPL) (Map.lookup v m))

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

findSingleton2 :: Map p Positions -> p
findSingleton2 m = case (filter (isSinglePL2 . snd) . Map.toList) m of
  [(v, _)] -> v
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

summariseExpr :: Ord name
              => Expr name
              -> (Structure, Map name Positions)
summariseExpr = \case
  Var v   -> (SVar, Map.singleton v SinglePL)
  Lam x e ->
    let (str_body, map_body) = summariseExpr e
        (e_map, x_pos) = removeFromVM x map_body
    in (SLam x_pos str_body, e_map)
  App e1 e2 ->
    let (str1, map1) = summariseExpr e1
        (str2, map2) = summariseExpr e2
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
