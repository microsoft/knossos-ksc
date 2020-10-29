{-# LANGUAGE LambdaCase #-}

module KATHashFastOrig where

import Data.Map.Strict (Map, mapMaybe)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import Expr (Expr(Var, Lam, App))

data Positions
  = HerePL
  | JoinPL StructureTag (Maybe Positions) Positions
  deriving (Eq, Show)

data Structure
  = SVar
  | SLam StructureTag (Maybe Positions) Structure
  | SApp StructureTag Bool Structure Structure
  deriving (Eq, Show)

type StructureTag = Int

structureTag :: Structure -> StructureTag
structureTag = \case
  SVar -> 0
  SLam t _ _ -> t
  SApp t _ _ _ -> t

removeFromVM :: Ord v => v -> Map v positions -> (Map v positions, Maybe positions)
removeFromVM v m = (Map.delete v m, Map.lookup v m)

{-
unionVM :: Ord k => Map k Positions -> Map k Positions -> Map k Positions
unionVM = Merge.mergeMaps
            (\case
                LeftOnly l -> LeftOnlyPL l
                RightOnly r -> RightOnlyPL r
                Both l r -> BothPL l r
            )
-}

findSingleton :: Map p Positions -> p
findSingleton m = case Map.toList m of
  [(v, HerePL)] -> v
  [(_, _)] -> error "Expected HerePL"
  [] -> error "Expected map to be non-empty"
  _:_:_ -> error "Expected map not to have multiple elements"

extendVM :: Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

summariseExpr :: Ord name
              => Expr name
              -> (Structure, Map name Positions)
summariseExpr = \case
  Var v   -> (SVar, Map.singleton v HerePL)
  Lam x e ->
    let (str_body, map_body) = summariseExpr e
        (e_map, x_pos) = removeFromVM x map_body
    in (SLam (structureTag str_body) x_pos str_body, e_map)
  App e1 e2 ->
    let (str1, map1) = summariseExpr e1
        (str2, map2) = summariseExpr e2
        app_depth = max (structureTag str1) (structureTag str2) + 1
        tag = app_depth
        str = SApp tag left_bigger str1 str2
        vm = foldl' add_kv big_vm (Map.toList small_vm)
        left_bigger = Map.size map1 >= Map.size map2

        (big_vm, small_vm) = if left_bigger
                             then (map1, map2)
                             else (map2, map1)

        add_kv vm_ (v, p) = Map.alter (\mp -> Just (JoinPL tag mp p)) v vm_

    in (str, vm)

rebuild :: Ord name
        => (name -> name)
        -> name
        -> (Structure, Map name Positions)
        -> Expr name
rebuild freshen fresh (structure, m) = case structure of
  SVar -> Var (findSingleton m)
  SLam _ mp s -> Lam x (rebuild freshen fresher (s, m'))
    where x = fresh
          fresher = freshen fresh
          m' = case mp of Nothing -> m
                          Just p -> extendVM m x p
  SApp tag left_bigger s1 s2 -> App (rebuild freshen fresh (s1, m1))
                                    (rebuild freshen fresh (s2, m2))
    where small_m = mapMaybe upd_small m
          big_m   = mapMaybe upd_big m
          (m1, m2) = if left_bigger
                     then (big_m, small_m)
                     else (small_m, big_m)

          upd_small :: Positions -> Maybe Positions
          upd_small (JoinPL ptag _ pt) | ptag == tag = Just pt
          upd_small _ = Nothing

          upd_big :: Positions -> Maybe Positions
          upd_big (JoinPL ptag mpt _) | ptag == tag = mpt
          upd_big pt = Just pt
