{-# LANGUAGE LambdaCase #-}

module KATHash3 where

import Hedgehog hiding (Var)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Expr (Expr(Var, Lam, App))
import Merge

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

removeFromVM :: Ord v => v -> Map v positions -> (Map v positions, Maybe positions)
removeFromVM v m = (Map.delete v m, Map.lookup v m)

unionVM :: Ord k => Map k Positions -> Map k Positions -> Map k Positions
unionVM = Merge.mergeMaps
            (\case
                LeftOnly l -> LeftOnlyPL l
                RightOnly r -> RightOnlyPL r
                Both l r -> BothPL l r
            )

findSingleton :: Map p Positions -> p
findSingleton m = case Map.toList m of
  [(v, HerePL)] -> v
  [(_, _)] -> error "Expected HerePL"
  [] -> error "Expected map to be non-empty"
  _:_:_ -> error "Expected map not to have multiple elements"

extendVM :: Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

pickL :: Positions -> Maybe Positions
pickL = \case
  LeftOnlyPL pl -> Just pl
  BothPL pl _ -> Just pl
  _ -> Nothing

pickR :: Positions -> Maybe Positions
pickR = \case
  RightOnlyPL pr -> Just pr
  BothPL _ pr -> Just pr
  _ -> Nothing

summariseExpr :: Ord name
              => Expr h name
              -> (Structure, Map name Positions)
summariseExpr = \case
  Var _ v   -> (SVar, Map.singleton v HerePL)
  Lam _ x e ->
    let (str_body, map_body) = summariseExpr e
        (e_map, x_pos) = removeFromVM x map_body
    in (SLam x_pos str_body, e_map)
  App _ e1 e2 ->
    let (str1, map1) = summariseExpr e1
        (str2, map2) = summariseExpr e2
    in (SApp str1 str2, unionVM map1 map2)

rebuild :: Ord name
        => (name -> name)
        -> name
        -> (Structure, Map name Positions)
        -> Expr () name
rebuild freshen fresh (structure, m) = case structure of
  SVar -> Var () (findSingleton m)
  SLam mp s -> Lam () x (rebuild freshen fresher (s, m'))
    where x = fresh
          fresher = freshen fresh
          m' = case mp of Nothing -> m
                          Just p -> extendVM m x p
  SApp s1 s2 -> App () (rebuild freshen fresh (s1, m1))
                       (rebuild freshen fresh (s2, m2))
    where m1 = Map.mapMaybe pickL m
          m2 = Map.mapMaybe pickR m

rebuildSApp :: Map k Positions -> (Map k Positions, Map k Positions)
rebuildSApp m = (Map.mapMaybe pickL m, Map.mapMaybe pickR m)

prop_rebuildSApp3_inverse :: Gen (Expr () Char) -> TestLimit -> Property
prop_rebuildSApp3_inverse gen count = withTests count $ property $ do
  e1 <- forAll gen
  e2 <- forAll gen

  let (_, m1) = summariseExpr e1
      (_, m2) = summariseExpr e2
      (_, m) = summariseExpr (App () e1 e2)

      (m1', m2') = rebuildSApp m

  m1 === m1'
  m2 === m2'
