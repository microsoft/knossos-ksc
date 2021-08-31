{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Ksc.Prune where

import Ksc.Lang hiding ((<>), empty)
import Data.Maybe (mapMaybe)
import Data.Set
import Data.Map (fromList, lookup, Map)

prune :: [TDef] -> Set (UserFun Typed) -> [TDef]
prune defs roots = mapMaybe keep defs
  where needed = prune' children Data.Set.empty roots
        keep def = if def_fun def `elem` needed
                   then Just def
                   else Nothing

        children :: UserFun Typed -> Set (UserFun Typed)
        children next = case Data.Map.lookup next defsMap of
          Nothing  -> Data.Set.empty
          Just def -> case def_rhs def of
            StubRhs   -> Data.Set.empty
            EDefRhs   -> Data.Set.empty
            UserRhs e -> userFunsCalled e

        defsMap :: Map (UserFun Typed) TDef
        defsMap = Data.Map.fromList (fmap (\def -> (def_fun def, def)) defs)

prune' :: Ord a
       => (a -> Set a)
       -- ^ Children of a node
       -> Set a
       -- ^ Descendants that have already been found
       -> Set a
       -- ^ Yet to be explored
       -> Set a
       -- ^ Transitive descendants
prune' children = recurse
  where recurse done needed =
          case pop needed of
            Nothing -> done
            Just (next, needed') ->
              if next `elem` done
              then recurse done needed'
              else recurse (insert next done) (needed' <> children next)

        pop = maxView

userFunsCalled :: TExpr -> Set (UserFun Typed)
userFunsCalled = \case
  Konst _  -> empty
  Var _    -> empty
  Call f e -> singletonIfUserFun f <> userFunsCalled e
  Tuple es -> foldMap userFunsCalled es
  Lam _ e  -> userFunsCalled e
  Dummy _  -> empty
  App e1 e2    -> foldMap userFunsCalled [e1, e2]
  Let _ e1 e2  -> foldMap userFunsCalled [e1, e2]
  If e e1 e2   -> foldMap userFunsCalled [e, e1, e2]
  Assert e1 e2 -> foldMap userFunsCalled [e1, e2]

singletonIfUserFun :: TFun t -> Set (UserFun t)
singletonIfUserFun (TFun _ f) = case maybeUserFun f of
  Nothing -> empty
  Just u  -> singleton u
