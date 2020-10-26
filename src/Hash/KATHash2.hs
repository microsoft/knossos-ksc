{-# LANGUAGE LambdaCase #-}

module KATHash2 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Expr (Expr(Var, Lam, App))
import Merge

data Positions
  = EmptyPL
  | HerePL
  | LeftOnlyPL Positions
  | RightOnlyPL Positions
  | BothPL Positions Positions
  deriving (Eq, Show)

data Structure
  = SVar
  | SLam Positions Structure
  | SApp Structure Structure
  deriving (Eq, Show)

removeFromVM :: Ord v => v -> Map v Positions -> (Map v Positions, Positions)
removeFromVM v m = (Map.delete v m, fromMaybe EmptyPL (Map.lookup v m))

unionVM :: Ord k => Map k Positions -> Map k Positions -> Map k Positions
unionVM = Merge.mergeMaps
            (\case
                LeftOnly l -> LeftOnlyPL l
                RightOnly r -> RightOnlyPL r
                Both l r -> BothPL l r
            )

findSingleton :: Map p Positions -> p
findSingleton m = case (filter (isHerePL . snd) . Map.toList) m of
  [(v, _)] -> v
  [] -> error "Expected map to be non-empty"
  _:_:_ -> error "Expected map not to have multiple elements"

isHerePL :: Positions -> Bool
isHerePL = \case
  HerePL -> True
  _ -> False

extendVM :: Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

pickL :: Positions -> Positions
pickL = \case
  LeftOnlyPL pl -> pl
  BothPL pl _ -> pl
  _ -> EmptyPL

pickR :: Positions -> Positions
pickR = \case
  RightOnlyPL pr -> pr
  BothPL _ pr -> pr
  _ -> EmptyPL

summariseExpr :: Ord name
              => Expr name
              -> (Structure, Map name Positions)
summariseExpr = \case
  Var v   -> (SVar, Map.singleton v HerePL)
  Lam x e ->
    let (str_body, map_body) = summariseExpr e
        (e_map, x_pos) = removeFromVM x map_body
    in (SLam x_pos str_body, e_map)
  App e1 e2 ->
    let (str1, map1) = summariseExpr e1
        (str2, map2) = summariseExpr e2
    in (SApp str1 str2, unionVM map1 map2)

rebuild :: Ord name
        => (name -> name)
        -> name
        -> (Structure, Map name Positions)
        -> Expr name
rebuild freshen fresh (structure, m) = case structure of
  SVar -> Var (findSingleton m)
  SLam p s -> Lam x (rebuild freshen fresher (s, extendVM m x p))
    where x = fresh
          fresher = freshen fresh
  SApp s1 s2 -> App (rebuild freshen fresh (s1, m1))
                    (rebuild freshen fresh (s2, m2))
    where m1 = Map.map pickL m
          m2 = Map.map pickR m
