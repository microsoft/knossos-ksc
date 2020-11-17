{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module KATHashOptimizedHash where

import KATHashEfficientHash (thenHash, Hash, Positions, Structure, StructureTag)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Bits (xor)
import Data.Hashable (Hashable, hash)
import Data.List (foldl')

import Expr (Expr(Var, Lam, App))
import qualified Expr

data VarMap name = VM !(Map name Positions) !Hash

mkSVar :: Structure
mkSVar = hash "SVar"

mkHerePL :: Structure
mkHerePL = hash "HerePL"

mkJoinPL :: StructureTag -> Maybe Positions -> Positions -> Positions
mkJoinPL a b c = hash "JoinPL" `thenHash` a `thenHash` b `thenHash` c

mkSLam :: StructureTag -> Maybe Positions -> Structure -> Structure
mkSLam a b c = hash "SLam" `thenHash` a `thenHash` b `thenHash` c

mkSApp :: StructureTag -> Bool -> Structure -> Structure -> Structure
mkSApp a b c d = hash "SApp" `thenHash` a `thenHash` b `thenHash` c `thenHash` d

structureTag :: Structure -> StructureTag
structureTag = id

removeFromVM :: (Hashable v, Ord v) => v -> VarMap v -> (VarMap v, Maybe Positions)
removeFromVM key (VM entries existingHash)
  = munge (Map.updateLookupWithKey delete key entries)
  where munge (Nothing, m) = (VM m existingHash, Nothing)
        munge (Just pt, m) = (VM m (existingHash `xor` entryHash key pt), Just pt)
        delete _ _ = Nothing

entryHash :: Hashable name => name -> Positions -> Hash
entryHash key pos = hash key `thenHash` pos

singletonVM :: Hashable name => name -> Positions -> VarMap name
singletonVM key pos = VM (Map.singleton key pos) (entryHash key pos)

alterVM :: (Hashable name, Ord name)
        => (Maybe Positions -> Positions)
        -> name -> VarMap name -> VarMap name
alterVM f key (VM entries old_hash)
  = munge (Map.alterF g key entries)
  where munge (h, m) = VM m h
        g :: Maybe Positions -> (Hash, Maybe Positions)
        g = \case
          Nothing     -> (old_hash `xor` entryHash key new_pt, Just new_pt)
            where new_pt = f Nothing
          Just old_pt -> (old_hash `xor` entryHash key old_pt
                                   `xor` entryHash key new_pt, Just new_pt)
            where new_pt = f (Just old_pt)

hashVM :: VarMap name -> Hash
hashVM (VM _ h) = h

sizeVM :: VarMap name -> Int
sizeVM (VM m _) = Map.size m

toListVM :: VarMap name -> [(name, Positions)]
toListVM (VM m _) = Map.toList m

summariseExpr :: (Hashable name, Ord name)
              => Expr h name
              -> (Structure, VarMap name, Expr Hash name)
summariseExpr = \case
  Var _ v   ->
    let !structure   = mkSVar
        !positionMap = singletonVM v mkHerePL
    in (structure, positionMap, Var (hash structure `thenHash` hashVM positionMap) v)
  Lam _ x e ->
    let (str_body, map_body, e') = summariseExpr e
        (e_map, x_pos) = removeFromVM x map_body
        !structure = mkSLam (structureTag str_body) x_pos str_body
        !positionMap = e_map
    in (structure, positionMap, Lam (hash structure `thenHash` hashVM positionMap) x e')
  App _ e1 e2 ->
    let (str1, map1, e1') = summariseExpr e1
        (str2, map2, e2') = summariseExpr e2
        app_depth = hash (structureTag str1) `thenHash` structureTag str2
        tag = app_depth
        !str = mkSApp tag left_bigger str1 str2
        !vm = foldl' add_kv big_vm (toListVM small_vm)
        left_bigger = sizeVM map1 >= sizeVM map2

        (big_vm, small_vm) = if left_bigger
                             then (map1, map2)
                             else (map2, map1)

        add_kv vm_ (v, p) = alterVM (\mp -> mkJoinPL tag mp p) v vm_

    in (str, vm, App (hash str `thenHash` hashVM vm) e1' e2')

katHash :: (Ord name, Hashable name) => Expr h name -> Expr Hash name
katHash e = e'
  where (_, _, e') = summariseExpr e

katHashTop :: (Ord name, Hashable name) => Expr h name -> Hash
katHashTop = Expr.annotation . katHash
