{-# LANGUAGE LambdaCase #-}

module KATHashFastOrigHash where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Hashable (Hashable, hash)
import Data.List (foldl')

import Expr (Expr(Var, Lam, App))

type Hash = Int
type Structure = Hash
type Positions = Hash
type StructureTag = Structure

mkSVar :: Structure
mkSVar = hash "SVar"

mkHerePL :: Structure
mkHerePL = hash "HerePL"

mkJoinPL :: StructureTag -> Maybe Positions -> Positions -> Positions
mkJoinPL a b c = hash ("JoinPL", 0 :: Int,
                       (a, 0 :: Int, b, 0 :: Int, c, 0 :: Int), 0 :: Int)

mkSLam :: StructureTag -> Maybe Positions -> Structure -> Structure
mkSLam a b c = hash (("SLam", 0 :: Int, a, 0 :: Int, b, 0 :: Int), 0 :: Int, c, 0 :: Int)

mkSApp :: StructureTag -> Bool -> Structure -> Structure -> Structure
mkSApp a b c d = hash (("SApp", 0 :: Int, a, 0 :: Int, b, 0 :: Int), 0 :: Int, (c, 0 :: Int, d, 0 :: Int), 0 :: Int)

structureTag :: Structure -> StructureTag
structureTag = id

removeFromVM :: Ord v => v -> Map v positions -> (Map v positions, Maybe positions)
removeFromVM v m = (Map.delete v m, Map.lookup v m)

findSingleton :: Map p Positions -> p
findSingleton m = case Map.toList m of
  [(v, _HerePL)] -> v
  [] -> error "Expected map to be non-empty"
  _:_:_ -> error "Expected map not to have multiple elements"

extendVM :: Ord k => Map k a -> k -> a -> Map k a
extendVM m x p = Map.insert x p m

summariseExpr :: (Hashable name, Ord name)
              => Expr h name
              -> (Structure, Map name Positions, Expr Hash name)
summariseExpr = \case
  Var _ v   ->
    let structure   = mkSVar
        positionMap = Map.singleton v mkHerePL
    in (structure, positionMap, Var (hash (structure, 0 :: Int, Map.toList positionMap, 0 :: Int)) v)
  Lam _ x e ->
    let (str_body, map_body, e') = summariseExpr e
        (e_map, x_pos) = removeFromVM x map_body
        structure = mkSLam (structureTag str_body) x_pos str_body
        positionMap = e_map
    in (structure, positionMap, Lam (hash (structure, 0 :: Int, Map.toList positionMap, 0 :: Int)) x e')
  App _ e1 e2 ->
    let (str1, map1, e1') = summariseExpr e1
        (str2, map2, e2') = summariseExpr e2
        app_depth = --max (structureTag str1) (structureTag str2) + 1
          hash (structureTag str1, 0 :: Int, structureTag str2, 0 :: Int)
        tag = app_depth
        str = mkSApp tag left_bigger str1 str2
        vm = foldl' add_kv big_vm (Map.toList small_vm)
        left_bigger = Map.size map1 >= Map.size map2

        (big_vm, small_vm) = if left_bigger
                             then (map1, map2)
                             else (map2, map1)

        add_kv vm_ (v, p) = Map.alter (\mp -> Just (mkJoinPL tag mp p)) v vm_

    in (str, vm, App (hash (str, 0 :: Int, Map.toList vm, 0 :: Int)) e1' e2')

katHash :: (Ord name, Hashable name) => Expr h name -> Expr Hash name
katHash e = e'
  where (_, _, e') = summariseExpr e
