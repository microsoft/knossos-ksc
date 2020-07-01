-- | A proof-of-concept for a lambda expression hashing algorithm.  It
-- is explained below that the hashing function is compositional and
-- respects alpha-equivalence, what these concepts mean, and why they
-- are important.  There are test cases that demonstrate some
-- behaviour and check alpha-equivalence is indeed respected.  The
-- latter is done by comparing to the behaviour of an
-- alpha-equivalence check implemented in terms of an independent
-- function which gives each binder a unique name ('uniqifyBinders').
--
-- Being compositional implies that the hashing function can be used
-- during expression rewriting without recalculating the entire hash.
-- This allows us to avoid doing O(N) work at each node to recalculate
-- the subexpression hashes each time the expression changes. More is
-- explained below.
--
-- # How to run it
--
-- ## Install the dependencies
--
-- Make sure you have installed GHC and Cabal as documented in the
-- Knossos README
--
-- https://github.com/microsoft/knossos-ksc/#installing-dependencies
--
-- ## Run the REPL
--
-- In the directory containing `hash.cabal` issue the command
--
--     $ cabal v2-repl
--
-- If GHC and Cabal are not on your `PATH` you might need a more
-- explicit command line, like
--
-- ~/.ghcup/bin/cabal v2-repl --with-ghc ~/.ghcup/ghc/8.6.5/bin/ghc
--
-- ## Run some examples
--
-- For example, have a look at example1:
--
--     > import Expr
--     > showExpr example1
--
--     ((lam x ((add x) x)) (lam y ((add y) y)))
--
-- You can generate an HTML file which compares the behaviour of the
-- hashing algorithms
--
--     > import CompareBehaviour
--     > writeAwfFormatExpressionsHTML "filename.html"
--
-- The sub-expression hashing algorithms are
--
-- * castHash (Tom's structural and free-vars hash)
-- * deBruijnHash (known to be broken)
-- * combinedHash (deBruijnHash plus a free variable hash -- known to be broken)
--
-- The following example expressions have been transferred from
-- OneNote
--
-- * expression1
-- * expression2
-- * expression3
-- * expression4

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Hash where

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Merge.Strict as Merge
import Data.Hashable (Hashable, hash, hashWithSalt)
import GHC.Generics (Generic)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Maybe as Maybe

import Expr (Expr(Var, Lam, App), Path, Step(Apl, Apr, L),
             example1, example2, example3, example4)

-- | A helper type that is intended to make the hashing algorithm
-- clearer.  If it doesn't help I may just get rid of it.
data ExprO v expr =
    VarO
  | LamO v expr
  | AppO (Maybe expr) (Maybe expr)
  deriving Generic

instance (Hashable v, Hashable expr) => Hashable (ExprO v expr)

hashExprO :: ExprO (Maybe Hash) Hash -> Hash
hashExprO = hash

hashExprOWithSalt :: Hash -> ExprO (Maybe Hash) Hash -> Hash
hashExprOWithSalt = hashWithSalt

type Hash = Int

-- | A hashing function for expressions.  It respects
-- alpha-equivalence, that is, expressions should have the same value
-- under this function if and only if they are alpha-equivalent
-- (modulo hash collision, which we expect to be very rare).
--
-- Combines the variables hash and structure hash into a single hash
-- value.  NB it is the triple returned by 'castHashExplicit' that is
-- compositional, not this single hash value.
castHashTop :: (Ord a, Hashable a) => Expr a -> Hash
castHashTop e = hash (Map.toList m, h)
  where (m, h, _depth, _exprs) = castHashExplicit [] Map.empty e

castHash :: (Ord a, Hashable a)
         => Expr a -> [(Hash, Path, Expr a)]
castHash e = exprs
  where (_m, _h, _depth, exprs) = castHashExplicit [] Map.empty e

-- | The implementation of the alpha-equivalence-respecting hashing
-- function.
--
-- It assigns to each subtree a value which is a triple containing
--
--     (variables hash, structure hash, depth)
--
-- "depth" is the usual depth of a tree-like structure.  It seems to
-- be important to mix the depth into the hash when hashing recursive
-- data structures (in general, not just lambda-expression-like data
-- structures).  The other components will be explained below.
--
-- "castHash" means "compositional AST hash".
-- The important property that it satisfies is compositionality: the
-- value assigned to each subtree can be calculated from the values
-- assigned to its direct children and nothing else.  Compositionality
-- is desirable because it implies that when creating a new expression
-- that reuses parts of an old one we can avoid rerunning the parts of
-- the hash computation on parts of the expression that have not
-- changed.  (It requires decorating sub-expressions with their hash
-- value and is not implemented in this file but is a straightforward
-- standard technique)
--
-- The "variables hash" and "structure hash" are two values that are
-- computed for each subexpression.  Taken together they uniquely
-- identify that subexpression up to alpha-equivalence (modulo hash
-- collisions).  The "variables hash" contains information sufficient
-- to determine the placement of free variables in the expression, and
-- the "structure hash" determines everything else about the
-- expression (the "structure").  Treating the two values separately
-- is what allows @castHash@ to respect alpha-equivalence.
--
-- The "structure hash" is a hash that uniquely determines the
-- "structure" of the lambda expression up to the names of the
-- variables.  Included in "structure" are the positions of free
-- variables, but not their names, and the positions of bound
-- variables, and which binder they refer to, but not their names.
-- (This implies that bound variables with different binders are
-- distinguishable, but free variables are not distinguishable).  For
-- example, the expressions
--
--     (lam x. (x + x + Y + Z)) Y
--
-- and
--
--     (lam z. (z + z + U + U)) V
--
-- have the same structure, but
--
--     (lam z. (z + U + U + U)) V
--
-- Does not.
--
-- The "variables hash" is a map from the name of variables to the
-- hash of the collection of variable nodes in which that variable
-- name occur in the expression.  Important properties of the
-- variables hash are that
--
-- * each variable node in the expression is referenced by exactly one
-- entry in the variables hash.
--
-- * each entry in the variables hash only refers to variable nodes
-- that actually occur in the expression
--
-- Complexity: essentially constant(*) work is done at each node
-- *except* when merging the variables hash at App nodes.  That work
-- is linear in the number of free variables.  I haven't yet thought
-- much more about the implications of that on our use case.
--
-- (*) the map operations are actually logarithmic, not constant, but
-- we probably don't care about that.
castHashExplicit :: (Ord a, Hashable a)
                 => Path
                 -> Map a Path
                 -> Expr a
                 -> (Map a Hash, Hash, Int, [(Hash, Path, Expr a)])
castHashExplicit =
  let subExprHash_ variablesHash structureHash bvEnv =
        hash (Map.toList variablesHash,
              structureHash,
              filter ((`elem` Map.keys variablesHash) . fst) (Map.toList bvEnv))

  in \path bvEnv expr -> case expr of
  Var x   -> (variablesHash, structureHash, 0, hashes)
    where variablesHash = Map.singleton x (hashExprO VarO)
          structureHash = hashExprO VarO
          subExprHash   = subExprHash_ variablesHash structureHash bvEnv
          subExprHashes = []
          hashes        = (subExprHash, path, expr) : subExprHashes

  Lam x e -> (variablesHash, structureHash, depth + 1, hashes)
    where variablesHash = Map.delete x variablesHashE
          structureHash = hashExprOWithSalt depth (LamO hashX structureHashE)
          (variablesHashE, structureHashE, depth, subExprHashes) =
            castHashExplicit (L:path) (addLocn x path bvEnv) e
          subExprHash   = subExprHash_ variablesHash structureHash bvEnv
          hashes        = (subExprHash, path, expr) : subExprHashes

          hashX = Map.lookup x variablesHashE

  App f e -> (variablesHash, structureHash, max depthF depthE + 1, hashes)
    where variablesHash = mergeMaps (\case
                 LeftOnly  l   -> hashExprO (AppO (Just l) Nothing)
                 RightOnly r   -> hashExprO (AppO Nothing (Just r))
                 Both      l r -> hashExprO (AppO (Just l) (Just r)))
              variablesHashF variablesHashE

          structureHash =
            hashExprOWithSalt (hashWithSalt depthF depthE)
                             (AppO (Just structureHashF) (Just structureHashE))

          subExprHashes = subExprHashesF ++ subExprHashesE
          subExprHash   = subExprHash_ variablesHash structureHash bvEnv
          hashes        = (subExprHash, path, expr) : subExprHashes

          (variablesHashF, structureHashF, depthF, subExprHashesF) =
            castHashExplicit (Apl:path) bvEnv f
          (variablesHashE, structureHashE, depthE, subExprHashesE) =
            castHashExplicit (Apr:path) bvEnv e


-- | We have pairs of hashes in many places, so here's a type for that.
-- It would be cleared to name the components of this pair though.
type TwoHashes = (Hash, Hash)

-- | Describes a linear function `f(x) = (ax + b)` as a tuple (a, b, a^{-1})
-- Note the inverse is modulo 2^64, which exists assuming `a` is odd. By storing
-- `a^{-1}` we make it possible to reverse our function without having to compute
-- the modular inverse every time.
type HashMapping = (Hash, Hash, Hash)

-- | We actually want to apply `HashMapping`s on pairs of hashes, where
-- the second element just sits there unchanged. The following few functions
-- take care of applying a `HashMapping`, applying its inverse, and composing
-- two `HashMapping`s.

hashMappingApply :: HashMapping -> TwoHashes -> TwoHashes
hashMappingApply (a, b, _) (x_1, x_2) = (a * x_1 + b, x_2)

hashMappingApplyInverse :: HashMapping -> TwoHashes -> TwoHashes
hashMappingApplyInverse (_, b, aInv) (x_1, x_2) = ((x_1 - b) * aInv, x_2)

hashMappingCompose :: HashMapping -> HashMapping -> HashMapping
hashMappingCompose (a1, b1, aInv1) (a2, b2, aInv2) = (a1 * a2, a2 * b1 + b2, aInv1 * aInv2)

-- | Just your everyday 3^{-1} mod 2^64.
inverseOfThree :: Hash
inverseOfThree = -6148914691236517205

hashMappingIden :: HashMapping
hashMappingIden = (1, 0, 1)

-- | f(x) = 3 * x + 1
hashMappingL :: HashMapping
hashMappingL = (3, 1, inverseOfThree)

-- | f(x) = 3 * x + 2
hashMappingR :: HashMapping
hashMappingR = (3, 2, inverseOfThree)

-- | Now comes the type that does all the heavy-lifting. It's a map from keys to `TwoHashes`,
-- that, additionally to insert / lookup / delete, supports the following operations in O(1)
-- time:
--   - apply a `HashMapping` on all values
--   - compute `(sum hash (key_i, first_value_i), sum hash (key_i, second_value_i))`
--
-- The former is done by keeping a lazy mapping attached to our map (the first element of the tuple).
-- The latter is done by maintaining these sums as we go (the last two elements of the tuple).
type LazyMap a = (HashMapping, Map a TwoHashes, TwoHashes)

-- | Here we just lift some simple `Map` utils to `LazyMap`.

lazyMapKeys :: (Ord a, Hashable a) => LazyMap a -> [a]
lazyMapKeys (_, innerMap, _) =
  Map.keys innerMap

lazyMapSize :: (Ord a, Hashable a) => LazyMap a -> Int
lazyMapSize (_, innerMap, _) =
  Map.size innerMap

-- | A helper to hash a `LazyMap` entry.
computeEntryHash :: (Ord a, Hashable a) => a -> TwoHashes -> TwoHashes
computeEntryHash key (value_1, value_2) =
  (hash (key, value_1), hash (key, value_2))

-- | Now we implement some standard map functions, but need to take into account the lazy hash mapping.

lazyMapSingleton :: (Ord a, Hashable a) => a -> TwoHashes -> LazyMap a
lazyMapSingleton key value =
    (hashMappingIden, Map.singleton key value, computeEntryHash key value)

lazyMapLookup :: (Ord a, Hashable a) => a -> LazyMap a -> Maybe TwoHashes
lazyMapLookup key (hashMapping, innerMap, _) =
  fmap (hashMappingApply hashMapping) (Map.lookup key innerMap)

lazyMapInsert :: (Ord a, Hashable a) => a -> TwoHashes -> LazyMap a -> LazyMap a
lazyMapInsert key value (hashMapping, innerMap, (entriesHash_1, entriesHash_2)) =
  let value' = hashMappingApplyInverse hashMapping value in
    let (entryHash_1, entryHash_2) = computeEntryHash key value' in
      (hashMapping, Map.insert key value' innerMap, (entriesHash_1 + entryHash_1, entriesHash_2 + entryHash_2))

lazyMapInsertAll :: (Ord a, Hashable a) => LazyMap a -> LazyMap a -> LazyMap a
lazyMapInsertAll (hashMapping, innerMap, _) otherLazyMap =
  foldr (\(key, value) lazyMap -> lazyMapInsert key (hashMappingApply hashMapping value) lazyMap) otherLazyMap (Map.assocs innerMap)

lazyMapDelete :: (Ord a, Hashable a) => a -> LazyMap a -> LazyMap a
lazyMapDelete key (hashMapping, innerMap, entriesHash) =
  case (Map.lookup key innerMap) of
    Nothing -> (hashMapping, innerMap, entriesHash)
    Just value ->
      let (entriesHash_1, entriesHash_2) = entriesHash in
        let (entryHash_1, entryHash_2) = computeEntryHash key value in
          (hashMapping, Map.delete key innerMap, (entriesHash_1 - entryHash_1, entriesHash_2 - entryHash_2))

lazyMapApplyHashMapping :: (Ord a, Hashable a) => HashMapping -> LazyMap a -> LazyMap a
lazyMapApplyHashMapping otherMapping (hashMapping, innerMap, entriesHash) =
  (hashMappingCompose hashMapping otherMapping, innerMap, entriesHash)

-- | A helper that returns two hashes of a `LazyMap`: taking into account first and second values.
lazyMapHash :: (Ord a, Hashable a) => LazyMap a -> TwoHashes
lazyMapHash (hashMapping, _, (entriesHash_1, entriesHash_2)) =
  (hash (hashMapping, entriesHash_1), entriesHash_2)

-- | Helper for `splitMapsSmallAndLarge`.
updateMaps :: (Ord a, Hashable a) => a -> (LazyMap a, Map a (TwoHashes, TwoHashes), LazyMap a) -> (LazyMap a, Map a (TwoHashes, TwoHashes), LazyMap a)
updateMaps key (onlySmall, intersection, onlyLarge) =
  case (lazyMapLookup key onlyLarge) of
    Nothing -> (onlySmall, intersection, onlyLarge)
    Just valueLarge  -> (lazyMapDelete key onlySmall, Map.insert key ((Maybe.fromJust (lazyMapLookup key onlySmall)), valueLarge) intersection, lazyMapDelete key onlyLarge)

-- | Splits maps into "in small map only", "intersection" and "in large map only".
splitMapsSmallAndLarge :: (Ord a, Hashable a) => LazyMap a -> LazyMap a -> (LazyMap a, Map a (TwoHashes, TwoHashes), LazyMap a)
splitMapsSmallAndLarge mapSmall mapLarge =
  foldr updateMaps (mapSmall, Map.empty, mapLarge) (lazyMapKeys mapSmall)

-- | Helpers used to combine two hashes; `hashCombineRev` does the same but reversed the argument order.

hashCombine :: Hash -> Hash -> Hash
hashCombine l r = hashExprO (AppO (Just l) (Just r))

hashCombineRev :: Hash -> Hash -> Hash
hashCombineRev l r = hashExprO (AppO (Just r) (Just l))

sumHashEntries :: (Ord a, Hashable a) => Map a TwoHashes -> TwoHashes
sumHashEntries mp =
  let hashes = map (\(key, value) -> computeEntryHash key value) (Map.assocs mp) in
    (sum (map fst hashes), sum (map snd hashes))

-- | An ugly hack: we want to combine two pairs, where the function to combine first elements is
-- defined, while the second elements are assumed to be equal...
liftToPairs :: (Ord a) => (a -> a -> a) -> ((a, a) -> (a, a) -> (a, a))
liftToPairs f =
  \(x_1, x_2) (y_1, y_2) -> (case x_2 == y_2 of
    True -> (f x_1 y_1, x_2)
    False -> error "Something bad happened")

-- | Combines two lazy maps in time proportional to the smaller one.
lazyMapsCombineSmallToLarge :: (Ord a, Hashable a) => LazyMap a -> LazyMap a -> HashMapping -> (Hash -> Hash -> Hash) -> HashMapping -> LazyMap a
lazyMapsCombineSmallToLarge lazyMapSmall lazyMapLarge applySmall applyIntersection applyLarge =
  let (hashMappingSmall, _, _) = lazyMapSmall in
    let (hashMappingLarge, _, _) = lazyMapLarge in
      let applyIntersection' = (\(valueSmall, valueLarge) -> (liftToPairs applyIntersection) (hashMappingApply hashMappingSmall valueSmall) (hashMappingApply hashMappingLarge valueLarge)) in
        let (onlySmall, intersection, onlyLarge) = splitMapsSmallAndLarge lazyMapSmall lazyMapLarge in
          let intersectionCombined = Map.map applyIntersection' intersection in
            let lazyMapOnlySmall = lazyMapApplyHashMapping applySmall onlySmall in
              let lazyMapOnlyLarge = lazyMapApplyHashMapping applyLarge onlyLarge in
                let lazyMapIntersection = (hashMappingIden, intersectionCombined, sumHashEntries intersectionCombined) in
                  lazyMapInsertAll lazyMapOnlySmall (lazyMapInsertAll lazyMapIntersection lazyMapOnlyLarge)

lazyMapsCombine :: (Ord a, Hashable a) => LazyMap a -> LazyMap a -> LazyMap a
lazyMapsCombine lazyMapLeft lazyMapRight =
  case lazyMapSize lazyMapLeft < lazyMapSize lazyMapRight of
    True  -> lazyMapsCombineSmallToLarge lazyMapLeft lazyMapRight hashMappingL hashCombine hashMappingR
    False -> lazyMapsCombineSmallToLarge lazyMapRight lazyMapLeft hashMappingR hashCombineRev hashMappingL

castHashOptimized :: (Ord a, Hashable a)
         => Expr a -> [(Hash, Path, Expr a)]
castHashOptimized e = exprs
  where (_m, _b, _depth, exprs) = castHashOptimizedExplicit ([], 1) Map.empty e

castHashOptimizedExplicit :: (Ord a, Hashable a)
                 => (Path, Hash)
                 -> Map a Hash
                 -> Expr a
                 -> (LazyMap a, Hash, Int, [(Hash, Path, Expr a)])
castHashOptimizedExplicit =
  let subExprHash_ variablesHash structureHash =
        hash (lazyMapHash variablesHash, structureHash)

  in \(path, pathHash) bvEnv expr -> case expr of
  Var x   -> (variablesHash, structureHash, 0, hashes)
    where variablesHash = lazyMapSingleton x ((hashExprO VarO), hash (Map.lookup x bvEnv))
          structureHash = hashExprO VarO
          subExprHash   = subExprHash_ variablesHash structureHash
          subExprHashes = []
          hashes        = (subExprHash, path, expr) : subExprHashes

  Lam x e -> (variablesHash, structureHash, depth + 1, hashes)
    where variablesHash = lazyMapDelete x variablesHashE
          structureHash = hashExprOWithSalt depth (LamO hashX structureHashE)
          (variablesHashE, structureHashE, depth, subExprHashes) =
            castHashOptimizedExplicit ((L:path), (hash (pathHash, L))) (Map.insert x pathHash bvEnv) e
          subExprHash   = subExprHash_ variablesHash structureHash
          hashes        = (subExprHash, path, expr) : subExprHashes
          hashX = fmap fst (lazyMapLookup x variablesHashE)

  App f e -> (variablesHash, structureHash, max depthF depthE + 1, hashes)
    where variablesHash = lazyMapsCombine variablesHashF variablesHashE

          structureHash =
            hashExprOWithSalt (hashWithSalt depthF depthE)
                             (AppO (Just structureHashF) (Just structureHashE))

          subExprHashes = subExprHashesF ++ subExprHashesE
          subExprHash   = subExprHash_ variablesHash structureHash
          hashes        = (subExprHash, path, expr) : subExprHashes

          (variablesHashF, structureHashF, depthF, subExprHashesF) =
            castHashOptimizedExplicit ((Apl:path), (hash (pathHash, Apl))) bvEnv f
          (variablesHashE, structureHashE, depthE, subExprHashesE) =
            castHashOptimizedExplicit ((Apr:path), (hash (pathHash, Apr))) bvEnv e

-- | Whether two expressions are alpha-equivalent, implemented using
-- 'castHashTop'
alphaEquivalentAccordingToHashExpr :: (Ord a, Hashable a)
                                   => Expr a -> Expr a -> Bool
alphaEquivalentAccordingToHashExpr = (==) `on` castHashTop

-- | Makes binders unique whilst preserving alpha-equivalence.  The
-- binders are replaced with integers starting from zero and
-- increasing in left-to-right depth-first order.
--
-- In consequence, two expressions are alpha-equivalent if they are
-- equal under @uniqifyBinders@.
uniquifyBinders :: Ord a => Expr a -> Expr (Either a Int)
uniquifyBinders = fst . uniquifyBindersExplicit Map.empty 0

-- | The internals of 'uniquifyBinders'
uniquifyBindersExplicit :: Ord a
                        => Map.Map a Int
                        -> Int
                        -> Expr a
                        -> (Expr (Either a Int), Int)
uniquifyBindersExplicit m n = \case
  Var x -> case Map.lookup x m of
    Nothing -> (Var (Left x), n)
    Just i  -> (Var (Right i), n)
  Lam x e -> (Lam (Right n) e', n')
    where (e', n') = uniquifyBindersExplicit (Map.insert x n m) (n+1) e
  App f x -> (App f' x', n'')
    where (f', n')  = uniquifyBindersExplicit m n f
          (x', n'') = uniquifyBindersExplicit m n' x

-- | (Broken) DeBruijin Algorithm from "Finding Identical
-- Subexpressions"
deBruijnHash :: (Hashable a, Ord a) => Expr a -> [(Hash, Path, Expr a)]
deBruijnHash expr = es
  where (_, _, es) = deBruijnHashExplicit Map.empty [] expr

deBruijnHashExplicit :: (Hashable a, Ord a)
                     => Map.Map a Int
                     -> Path
                     -> Expr a
                     -> (Hash, Int, [(Hash, Path, Expr a)])
deBruijnHashExplicit = \env path expr -> case expr of
  Var x -> (hash', depth', l_ret)
    where hash' = case dbLookupVar x env of
            Nothing -> hash ("free", x, depth')
            Just i  -> hash ("bound", i, depth')
          depth' = 0 :: Int
          subExpressionHashes = []
          l_ret = (hash', path, expr) : subExpressionHashes
  Lam x e -> (hash', depth', l_ret)
    where (hashE, depthE, subExpressionHashesE) =
            deBruijnHashExplicit (dbAddVar x env) (L:path) e
          depth' = depthE + 1
          hash' = hash ("lam", hashE, depth')
          subExpressionHashes = subExpressionHashesE
          l_ret = (hash', path, expr) : subExpressionHashes
  App f e -> (hash', depth', l_ret)
    where (hashF, depthF, lF) = deBruijnHashExplicit env (Apl:path) f
          (hashE, depthE, lE) = deBruijnHashExplicit env (Apr:path) e
          depth' = max depthF depthE + 1
          hash'  = hash ("app", hashF, hashE, depth')
          subExpressionHashes = lF ++ lE
          l_ret = (hash', path, expr) : subExpressionHashes

dbAddVar :: Ord k => k -> Map k Int -> Map k Int
dbAddVar v env = Map.insert v (Map.size env) env

dbLookupVar :: Ord k => k -> Map k Int -> Maybe Int
dbLookupVar v env = fmap (Map.size env -) (Map.lookup v env)

combinedHash :: (Ord a, Hashable a) => Expr a -> [(Hash, Path, Expr a)]
combinedHash expr = es
  where (_, _, _, es) = combinedHashExplicit Map.empty Map.empty [] expr

-- | (Still broken) DeBruijn + free-var-hash algorithm from "Finding
-- Identical Subexpressions"
--
-- fvEnv isn't really the right name. It actually maps *bound*
-- variables (to their binding locations).
combinedHashExplicit :: (Hashable a, Ord a)
                     => Map.Map a Int
                     -> Map.Map a Path
                     -> Path
                     -> Expr a
                     -> (Hash, Set a, Int, [(Hash, Path, Expr a)])
combinedHashExplicit = \env fvEnv location expr ->
  let fvHash freeVars = map (flip Map.lookup fvEnv) (Set.toList freeVars)
  in case expr of
  Var x -> (dbHash', freeVars', depth', l_ret)
    where dbHash' = case dbLookupVar x env of
            Nothing -> hash ("free", x, depth')
            Just i  -> hash ("bound", i, depth')
          depth' = 0 :: Int
          freeVars' = Set.singleton x
          jointHash' = hash (dbHash', fvHash freeVars')
          subExpressionHashes = []
          l_ret = (jointHash', location, expr) : subExpressionHashes
  Lam x e -> (dbHash', freeVars', depth', l_ret)
    where (dbHashE, freeVarsE, depthE, subExpressionHashesE) =
            combinedHashExplicit (dbAddVar x env)
                                 (addLocn x location fvEnv)
                                 (L:location) e
          depth' = depthE + 1
          dbHash' = hash ("lam", dbHashE, depth')
          jointHash' = hash (dbHash', fvHash freeVars')
          freeVars' = Set.delete x freeVarsE
          subExpressionHashes = subExpressionHashesE
          l_ret = (jointHash', location, expr) : subExpressionHashes
  App f e -> (dbHash', freeVars', depth', l_ret)
    where (dbHashF, freeVarsF, depthF, subExpressionHashesF)  =
            combinedHashExplicit env fvEnv (Apl:location) f
          (dbHashE, freeVarsE, depthE, subExpressionHashesE) =
            combinedHashExplicit env fvEnv (Apr:location)  e
          depth'  = max depthF depthE + 1
          dbHash' = hash ("app", dbHashF, dbHashE, depth')
          jointHash' = hash (dbHash', fvHash freeVars')
          freeVars' = Set.union freeVarsF freeVarsE
          subExpressionHashes = subExpressionHashesF ++ subExpressionHashesE
          l_ret = (jointHash', location, expr) : subExpressionHashes

addLocn :: Ord k => k -> a -> Map k a -> Map k a
addLocn = Map.insert

-- | The most basic hash one could think of.  Is not intended to
-- respect any kind of equivalences.
naiveHash :: Hashable a => Expr a -> Hash
naiveHash = \case
  Var x   -> hash x
  Lam x e -> hash (x, naiveHash e)
  App f e -> hash (naiveHash f, naiveHash e)

naiveHashNested :: Hashable a => Expr a -> [(Hash, Path, Expr a)]
naiveHashNested = naiveHashNestedExplicit []

naiveHashNestedExplicit :: Hashable a
                        => Path
                        -> Expr a
                        -> [(Hash, Path, Expr a)]
naiveHashNestedExplicit path expr = (naiveHash expr, path, expr) : case expr of
  Var{}   -> []
  Lam _ e -> naiveHashNestedExplicit (L:path) e
  App f e -> naiveHashNestedExplicit (Apl:path) f
             ++ naiveHashNestedExplicit (Apr:path) e

naiveHashWithBinders :: (Ord a, Hashable a)
                     => Expr a -> [(Hash, Path, Expr a)]
naiveHashWithBinders e = exprs
  where (_h, _depth, exprs) = naiveHashWithBindersExplicit [] Map.empty e

naiveHashWithBindersExplicit :: (Ord a, Hashable a)
                             => Path
                             -> Map a Path
                             -> Expr a
                             -> (Hash, Int, [(Hash, Path, Expr a)])
naiveHashWithBindersExplicit location env expr = case expr of
  Var x -> (hash', depth', l_ret)
    where hash' = hash $ case Map.lookup x env of
            Nothing -> hash ("free", x, depth')
            Just p  -> hash ("bound", p, depth')
          depth' = 0
          subExpressionHashes = []
          l_ret = (hash', location, expr) : subExpressionHashes
  Lam x e -> (hash', depth', l_ret)
    where (hashE, depthE, subExpressionHashesE) =
            naiveHashWithBindersExplicit (L:location) (Map.insert x location env) e
          hash' = hash ("lam", hashE, depth')
          depth' = depthE + 1
          subExpressionHashes = subExpressionHashesE
          l_ret = (hash', location, expr) : subExpressionHashes
  App f e -> (hash', depth', l_ret)
    where (hashF, depthF, subExpressionHashesF) =
            naiveHashWithBindersExplicit (Apl:location) env f
          (hashE, depthE, subExpressionHashesE) =
            naiveHashWithBindersExplicit (Apr:location) env e
          hash' = hash ("app", hashF, hashE, depth')
          subExpressionHashes = subExpressionHashesF ++ subExpressionHashesE
          depth' = max depthF depthE + 1
          l_ret = (hash', location, expr) : subExpressionHashes

naiveHashWithBinders2 :: (Ord a, Hashable a)
                     => Expr a -> [(Hash, Path, Expr a)]
naiveHashWithBinders2 e = exprs
  where (_h, _depth, exprs) = naiveHashWithBinders2Explicit [] Map.empty e

naiveHashWithBinders2Explicit :: (Ord a, Hashable a)
                              => Path
                              -> Map a Path
                              -> Expr a
                              -> (Hash, Int, [(Hash, Path, Expr a)])
naiveHashWithBinders2Explicit location env expr = case expr of
  Var x -> (hash', depth', l_ret)
    where hash' = hash $ case Map.lookup x env of
            Nothing -> hash ("free", x, depth')
            Just p  -> hash ("bound", p, depth')
          depth' = 0
          subExpressionHashes = []
          l_ret = (hash', location, expr) : subExpressionHashes
  Lam x e -> (hash', depth', l_ret)
    where (hashE, depthE, subExpressionHashesE) =
            naiveHashWithBindersExplicit (L:location)
                                         (Map.insert x [] (Map.map (L:) env)) e
          hash' = hash ("lam", hashE, depth')
          depth' = depthE + 1
          subExpressionHashes = subExpressionHashesE
          l_ret = (hash', location, expr) : subExpressionHashes
  App f e -> (hash', depth', l_ret)
    where (hashF, depthF, subExpressionHashesF) =
            naiveHashWithBindersExplicit (Apl:location) (Map.map (Apl:) env) f
          (hashE, depthE, subExpressionHashesE) =
            naiveHashWithBindersExplicit (Apr:location) (Map.map (Apr:) env) e
          hash' = hash ("app", hashF, hashE, depth')
          subExpressionHashes = subExpressionHashesF ++ subExpressionHashesE
          depth' = max depthF depthE + 1
          l_ret = (hash', location, expr) : subExpressionHashes

normalizedGroupedEquivalentSubexpressions
  :: Ord hash => [(hash, Path, expr)] -> [[(Path, expr)]]
normalizedGroupedEquivalentSubexpressions =
  sortBy (comparing (map fst))
  . filter ((/= 1) . length)
  . (map . map) (\(_, path, z) -> (path, z))
  . groupBy ((==) `on` (\(x, _, _) -> x))
  . sortBy (comparing (\(x, _, _) -> x))

-- | Whether two expressions are alpha-equivalent, implemented using
-- 'uniquifyBinders'
alphaEquivalentAccordingToUniquifyBinders :: Ord a => Expr a -> Expr a -> Bool
alphaEquivalentAccordingToUniquifyBinders = (==) `on` uniquifyBinders

testEverythingInFileStartingWith'prop_' :: IO Bool
testEverythingInFileStartingWith'prop_' = checkParallel $$(discover)

-- | Some specific test cases that demonstrate how 'uniquifyBinders'
-- works.  Please suggest more examples if you have ones that would be
-- helpful.
prop_uniquifyBindersExamples :: Property
prop_uniquifyBindersExamples = withTests 1 $ property $ do
  let b = Right -- "bound"
      f = Left  -- "free"
      examples = [ (Lam "x"   (Var "x"),
                    Lam (b 0) (Var (b 0)))

                 , (Lam "x"   (Var "y"),
                    Lam (b 0) (Var (f "y")))

                 , (Lam "x"   (Lam "y"   (Var "x")),
                    Lam (b 0) (Lam (b 1) (Var (b 0))))

                 , (Lam "x"   (Lam "x"   (Var "x")),
                    Lam (b 0) (Lam (b 1) (Var (b 1))))

                 , (Lam "x"   (App (Var "x")   (Var "x")),
                    Lam (b 0) (App (Var (b 0)) (Var (b 0))))

                 , (App (Lam "x"   (Var "x"))   (Lam "x"   (Var "x")),
                    App (Lam (b 0) (Var (b 0))) (Lam (b 1) (Var (b 1))))
                 ]

  flip mapM_ examples $ \(expression, uniquified) ->
    uniquifyBinders expression === uniquified

-- | Shows when our hashing functions argree and disagree on our
-- examples
prop_compareSubExpressionHashes :: Property
prop_compareSubExpressionHashes = withTests 1 $ property $ do
  let n = normalizedGroupedEquivalentSubexpressions

  n (castHash example1) === n (combinedHash example1)
  n (castHash example2) === n (combinedHash example2)
  n (castHash example3) === n (combinedHash example3)

  n (castHash example4) /== n (combinedHash example4)

-- | Checks that the paths come out of the algorithms in the same
-- order (which just so happens to be depth first preorder).  This is
-- not an essential property of the algorithms, but it's nice that
-- they are thus normalised so that we can compare behaviour more
-- easily.
prop_stablePaths :: Property
prop_stablePaths = withTests numRandomTests $ property $ do
  let paths = map (\(_, path, _) -> path)

  expr <- forAll genExpr

  let h = castHash expr
      d = deBruijnHash expr
      c = combinedHash expr
      n1 = naiveHashWithBinders expr
      n2 = naiveHashWithBinders2 expr

  paths h === paths d
  paths h === paths c
  paths h === paths n1
  paths h === paths n2

numRandomTests :: TestLimit
numRandomTests = 100 * 1000

-- | A sanity check for uniquifyBinders: it should be idempotent
prop_uniquifyBindersIdempotent :: Property
prop_uniquifyBindersIdempotent = withTests numRandomTests $ property $ do
  expr <- forAll genExpr
  let uniquifyBinders_expr = uniquifyBinders expr
      -- For fairly boring type system reasons the types coming out of
      -- one iteration of uniquifyBinders are different from the types
      -- coming out of two iterations.
      --
      -- We just need to convert 'Left x' to 'Left (Left x)' so they
      -- types match.
      massageVariables = fmap (either (Left . Left) Right)

  massageVariables uniquifyBinders_expr === uniquifyBinders uniquifyBinders_expr

-- | A sanity check for both uniquifyBinders and castHashTop: uniquifying
-- binders should preserve alpha-equivalence and this equivalence
-- should be picked up by castHashTop.
prop_hashUniquifyBinders :: Property
prop_hashUniquifyBinders = withTests numRandomTests $ property $ do
  expr <- forAll genExpr
  let massageVariables = fmap Left
  assert (alphaEquivalentAccordingToHashExpr (uniquifyBinders expr)
                                             (massageVariables expr))

-- | A check for whether castHashTop respects alpha-equivalence (as
-- defined above) by checking it against alpha-equivalence in terms of
-- uniquifyBinders, which is presumably easier to get right.
prop_hashAlphaEquivalence :: Property
prop_hashAlphaEquivalence = withTests numRandomTests $ property $ do
  expr1 <- forAll genExpr
  expr2 <- forAll genExpr

  -- Or can use Hedgehog's "diff"
  alphaEquivalentAccordingToUniquifyBinders expr1 expr2
    === alphaEquivalentAccordingToHashExpr expr1 expr2

-- | Generates random expressions for testing
genExprWithVars :: MonadGen m => [v] -> m (Expr v)
genExprWithVars vars = genExprWithVars_vars
  -- Hedgehog has an example for exactly this use case!
  --
  -- http://hackage.haskell.org/package/hedgehog-1.0.2/docs/Hedgehog-Gen.html#v:recursive
  where genExprWithVars_vars = Gen.recursive
          Gen.choice
          [ Var <$> Gen.element vars ]
          [ Gen.subtermM genExprWithVars_vars (\e -> Lam <$> Gen.element vars <*> pure e)
          , Gen.subterm2 genExprWithVars_vars genExprWithVars_vars App
          ]

-- | Generate expressions that are completely unbalanced, for testing
-- the worst cases of some of our hashing algorithms.
genExprWithVarsLinear :: MonadGen m => [a] -> m (Expr a)
genExprWithVarsLinear vars =
  Gen.choice ([ Var <$> Gen.element vars ]
             ++ replicate 22 recurse)

  where e = genExprWithVarsLinear vars
        recurse = App <$> (Lam <$> Gen.element vars <*> e)
                      <*> (Var <$> Gen.element vars)

-- | Generates random expressions for testing
genExpr :: MonadGen m => m (Expr Char)
genExpr = genExprWithVars ['u'..'z']

genExprNumVars :: MonadGen m => Int -> m (Expr String)
genExprNumVars n = genExprWithVars (map show [1..n])

genExprLinearNumVars :: MonadGen m => Int -> m (Expr String)
genExprLinearNumVars n = genExprWithVarsLinear (map show [1..n])

-- A slightly nicer API for merging maps
data MergeMaps l r = LeftOnly l
                   | Both l r
                   | RightOnly r

mergeMaps :: Ord k => (MergeMaps l r -> a) -> Map k l -> Map k r -> Map k a
mergeMaps f = Merge.merge (Merge.mapMissing (\_ l -> f (LeftOnly l)))
                          (Merge.mapMissing (\_ r -> f (RightOnly r)))
                          (Merge.zipWithMatched (\_ l r -> f (Both l r)))

-- | Shows equivalence of castHash hash and castHashOptimized hash
prop_equivCastFast :: Property
prop_equivCastFast = withTests numRandomTests $ property $ do
  let n = normalizedGroupedEquivalentSubexpressions
  expr <- forAll genExpr
  n (castHash expr) === n (castHashOptimized expr)