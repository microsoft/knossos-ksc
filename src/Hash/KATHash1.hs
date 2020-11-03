{-# LANGUAGE LambdaCase #-}

module KATHash1 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Expr (Expr(Var, Lam, App))

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

unionVM :: Ord k => Map k Positions -> Map k Positions -> Map k Positions
unionVM = Map.unionWith UnionPL

findSingleton :: Map p Positions -> p
findSingleton m = case (filter (isSinglePL . snd) . Map.toList) m of
  [(v, _)] -> v
  [] -> error "Expected map to be non-empty"
  _:_:_ -> error "Expected map not to have multiple elements"

-- This has terrible time complexity
isSinglePL :: Positions -> Bool
isSinglePL = \case
  SinglePL -> True
  UnionPL p1 p2 -> (isSinglePL p1 && isSinglePL p2)
                   || (isSinglePL p1 && isEmptyPL p2)
                   || (isEmptyPL p1 && isSinglePL p2)
  _ -> False

isEmptyPL :: Positions -> Bool
isEmptyPL = \case
  EmptyPL -> True
  UnionPL p1 p2 -> isEmptyPL p1 && isEmptyPL p2
  _ -> False

isSinglePL2 :: Positions -> Bool
isSinglePL2 = \case
  SinglePL -> True
  _ -> False

extendVM :: Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

pickL :: Positions -> Positions
pickL = \case
  ShiftLeftPL pl -> pl
  UnionPL pl1 pl2 -> UnionPL (pickL pl1) (pickL pl2)
  _ -> EmptyPL

pickR :: Positions -> Positions
pickR = \case
  ShiftRightPL pr -> pr
  UnionPL pl pr -> UnionPL (pickR pl) (pickR pr)
  _ -> EmptyPL

summariseExpr :: Ord name
              => Expr h name
              -> (Structure, Map name Positions)
summariseExpr = \case
  Var _ v   -> (SVar, Map.singleton v SinglePL)
  Lam _ x e ->
    let (str_body, map_body) = summariseExpr e
        (e_map, x_pos) = removeFromVM x map_body
    in (SLam x_pos str_body, e_map)
  App _ e1 e2 ->
    let (str1, map1) = summariseExpr e1
        (str2, map2) = summariseExpr e2
        map1_shift = Map.map ShiftLeftPL map1
        map2_shift = Map.map ShiftRightPL map2
    in (SApp str1 str2, unionVM map1_shift map2_shift)

rebuild :: Ord name
        => (name -> name)
        -> name
        -> (Structure, Map name Positions)
        -> Expr () name
rebuild freshen fresh (structure, m) = case structure of
  SVar -> Var () (findSingleton m)
  SLam p s -> Lam () x (rebuild freshen fresher (s, extendVM m x p))
    where x = fresh
          fresher = freshen fresh
  SApp s1 s2 -> App () (rebuild freshen fresh (s1, m1))
                       (rebuild freshen fresh (s2, m2))
    where m1 = Map.map pickL m
          m2 = Map.map pickR m
