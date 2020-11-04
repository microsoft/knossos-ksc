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
import qualified Hedgehog.Range as Range
import Data.Char (ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Hashable (Hashable, hash, hashWithSalt)
import GHC.Generics (Generic)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)

import Expr (Expr(Var, Lam, App), Path, Step(Apl, Apr, L),
             example1, example2, example3, example4,
             allSubexprs, annotation, mapAnnotation)
import qualified KATHash1
import qualified KATHash2
import qualified KATHash3
import qualified KATHashFast
import qualified KATHashFastOrig
import Merge

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

allHashResults :: Expr b a -> [(b, Path, Expr () a)]
allHashResults = f . allHashResults_
  where allHashResults_ = fmap (\(p, se) -> (annotation se, p, se)) . allSubexprs
        -- Warning: This mapAnnotation is slow, but it isn't involved
        -- in the benchmark and will likely disappear soon anyway.
        -- The reverse is also a pain.
        f = map (\(h__, p, es) -> (h__, reverse p, mapAnnotation (const ()) es))

-- | A hashing function for expressions.  It respects
-- alpha-equivalence, that is, expressions should have the same value
-- under this function if and only if they are alpha-equivalent
-- (modulo hash collision, which we expect to be very rare).
--
-- Combines the variables hash and structure hash into a single hash
-- value.  NB it is the triple returned by 'castHashExplicit' that is
-- compositional, not this single hash value.
castHashTop :: (Ord a, Hashable a) => Expr h a -> Hash
castHashTop e = hash (Map.toList m, h)
  where (m, h, _depth, _exprs) = castHashExplicit [] Map.empty e

castHash :: (Ord a, Hashable a)
         => Expr h a -> Expr Hash a
castHash e = e_
  where (_, _, _, e_) = castHashExplicit [] Map.empty e

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
                 -> Expr ignored a
                 -> (Map a Hash, Hash, Int, Expr Hash a)
castHashExplicit =
  let subExprHash_ variablesHash structureHash bvEnv =
        hash (Map.toList variablesHash,
              structureHash,
              filter ((`elem` Map.keys variablesHash) . fst) (Map.toList bvEnv))

  in \path bvEnv expr -> case expr of
  Var _ x   -> (variablesHash, structureHash, 0,
                Var subExprHash x)
    where variablesHash = Map.singleton x (hashExprO VarO)
          structureHash = hashExprO VarO
          subExprHash   = subExprHash_ variablesHash structureHash bvEnv

  Lam _ x e -> (variablesHash, structureHash, depth + 1,
                Lam subExprHash x subExpr)
    where variablesHash = Map.delete x variablesHashE
          structureHash = hashExprOWithSalt depth (LamO hashX structureHashE)
          (variablesHashE, structureHashE, depth, subExpr) =
            castHashExplicit (L:path) (addLocn x path bvEnv) e
          subExprHash   = subExprHash_ variablesHash structureHash bvEnv

          hashX = Map.lookup x variablesHashE

  App _ f e -> (variablesHash, structureHash, max depthF depthE + 1,
                App subExprHash subExprF subExprE)
    where variablesHash = mergeMaps (\case
                 LeftOnly  l   -> hashExprO (AppO (Just l) Nothing)
                 RightOnly r   -> hashExprO (AppO Nothing (Just r))
                 Both      l r -> hashExprO (AppO (Just l) (Just r)))
              variablesHashF variablesHashE

          structureHash =
            hashExprOWithSalt (hashWithSalt depthF depthE)
                             (AppO (Just structureHashF) (Just structureHashE))

          subExprHash   = subExprHash_ variablesHash structureHash bvEnv

          (variablesHashF, structureHashF, depthF, subExprF) =
            castHashExplicit (Apl:path) bvEnv f
          (variablesHashE, structureHashE, depthE, subExprE) =
            castHashExplicit (Apr:path) bvEnv e


-- | We have pairs of hashes in many places, so here's a type for that.
-- It would be cleared to name the components of this pair though.
data TwoHashes = TwoHashes !Hash !Hash

instance Hashable TwoHashes where
  hash (TwoHashes h1 h2) = hash (h1, h2)
  hashWithSalt s (TwoHashes h1 h2) = hashWithSalt s (h1, h2)

-- | Now comes the type that does all the heavy-lifting. It's a map
-- from keys to `TwoHashes`, that, additionally to insert / lookup /
-- delete, supports the following operations in O(1) time:
--   - compute `(sum hash (key_i, first_value_i), sum hash (key_i,
--     second_value_i))`
--
-- This is done by maintaining these sums as we go (the last two
-- elements of the tuple).
data LazyMap a = LazyMap !(Map a TwoHashes) !TwoHashes

-- | Here we just lift some simple `Map` utils to `LazyMap`.

lazyMapAssocs :: (Ord a, Hashable a) => LazyMap a -> [(a, TwoHashes)]
lazyMapAssocs (LazyMap m _) = Map.assocs m

lazyMapSize :: (Ord a, Hashable a) => LazyMap a -> Int
lazyMapSize (LazyMap m _) = Map.size m

lazyMapLookup :: (Ord a, Hashable a) => a -> LazyMap a -> Maybe TwoHashes
lazyMapLookup key (LazyMap innerMap _) = Map.lookup key innerMap

-- | A helper to hash a `LazyMap` entry.
computeEntryHash :: (Hashable a) => (a, TwoHashes) -> TwoHashes
computeEntryHash (key, TwoHashes value_1 value_2) =
  TwoHashes (hash (key, value_1)) (hash (key, value_2))

addEntryHash :: (Hashable a) => TwoHashes -> (a, TwoHashes) -> TwoHashes
addEntryHash (TwoHashes entriesHash_1 entriesHash_2) entry =
  let TwoHashes entryHash_1 entryHash_2 = computeEntryHash entry in
    TwoHashes (entriesHash_1 + entryHash_1) (entriesHash_2 + entryHash_2)

subtractEntryHash :: (Hashable a) => TwoHashes -> (a, TwoHashes) -> TwoHashes
subtractEntryHash (TwoHashes entriesHash_1 entriesHash_2) entry =
  let (TwoHashes entryHash_1 entryHash_2) = computeEntryHash entry in
    TwoHashes (entriesHash_1 - entryHash_1) (entriesHash_2 - entryHash_2)

lazyMapSingleton :: (Ord a, Hashable a) => a -> TwoHashes -> LazyMap a
lazyMapSingleton key value =
    LazyMap (Map.singleton key value) (computeEntryHash (key, value))

lazyMapInsert :: (Ord a, Hashable a) => a -> TwoHashes -> LazyMap a -> LazyMap a
lazyMapInsert key value (LazyMap innerMap entriesHash) =
  LazyMap (Map.insert key value innerMap) (addEntryHash entriesHash (key, value))

lazyMapDelete :: (Ord a, Hashable a) => a -> LazyMap a -> LazyMap a
lazyMapDelete key (LazyMap innerMap entriesHash) =
  let (ret, newInnerMap) = Map.updateLookupWithKey (\_ _ -> Nothing) key innerMap
  in case ret of
      Nothing -> LazyMap newInnerMap entriesHash
      Just value -> LazyMap newInnerMap (subtractEntryHash entriesHash (key, value))

lazyMapInsertWith :: (Ord a, Hashable a)
                  => (TwoHashes -> TwoHashes)
                  -> a
                  -> TwoHashes
                  -> LazyMap a
                  -> LazyMap a
lazyMapInsertWith f key value (LazyMap innerMap entriesHash) =
  let (ret, newInnerMap) =
        Map.insertLookupWithKey (\_ _ -> f) key value innerMap
  in case ret of
      Nothing -> LazyMap newInnerMap (addEntryHash entriesHash (key, value))
      Just oldValue ->
        LazyMap newInnerMap
         (addEntryHash (subtractEntryHash entriesHash (key, oldValue))
                       (key, f oldValue))

-- | Helper for `updateMapsSmallAndLarge`.
updateMaps :: (Ord a, Hashable a)
           => (TwoHashes -> TwoHashes)
           -> (TwoHashes -> TwoHashes -> TwoHashes)
           -> (a, TwoHashes)
           -> LazyMap a
           -> LazyMap a
updateMaps fOnlySmall fIntersection (key, value) =
  lazyMapInsertWith (fIntersection value) key (fOnlySmall value)

-- | Update the larger map by iterating through the entries of the smaller map.
updateMapsSmallAndLarge :: (Ord a, Hashable a)
                        => (TwoHashes -> TwoHashes)
                        -> (TwoHashes -> TwoHashes -> TwoHashes)
                        -> LazyMap a
                        -> LazyMap a
                        -> LazyMap a
updateMapsSmallAndLarge fOnlySmall fIntersection mapSmall mapLarge =
  foldr (updateMaps fOnlySmall fIntersection) mapLarge (lazyMapAssocs mapSmall)

-- | Helpers used to combine two hashes; `hashCombineRev` does the
-- same but reversed the argument order.

hashCombine :: Hash -> Hash -> Hash
hashCombine l r = hashExprO (AppO (Just l) (Just r))

hashCombineRev :: Hash -> Hash -> Hash
hashCombineRev l r = hashExprO (AppO (Just r) (Just l))

liftToPairs :: (Hash -> Hash -> Hash) -> TwoHashes -> TwoHashes -> TwoHashes
liftToPairs f = \(TwoHashes x_1 y) (TwoHashes x_2 _) -> TwoHashes (f x_1 x_2) y

liftToFirst :: (Hash -> Hash) -> TwoHashes -> TwoHashes
liftToFirst f = \(TwoHashes x y) -> TwoHashes (f x) y

-- | Combines two lazy maps in time proportional to the smaller one.
lazyMapsCombineSmallToLarge :: (Ord a, Hashable a)
                            => LazyMap a
                            -> LazyMap a
                            -> (Hash -> Hash)
                            -> (Hash -> Hash -> Hash)
                            -> LazyMap a
lazyMapsCombineSmallToLarge lazyMapSmall lazyMapLarge fOnlySmall fIntersection =
  updateMapsSmallAndLarge (liftToFirst fOnlySmall)
                          (liftToPairs fIntersection)
                          lazyMapSmall
                          lazyMapLarge

hashStepLeft :: Int -> Hash -> Hash
hashStepLeft subtreeSize h =
  hash (hashExprO (AppO (Just h) Nothing), subtreeSize)

hashStepRight :: Int -> Hash -> Hash
hashStepRight subtreeSize h =
  hash (hashExprO (AppO Nothing (Just h)), subtreeSize)

lazyMapsCombine :: (Ord a, Hashable a)
                => LazyMap a
                -> LazyMap a
                -> Int
                -> LazyMap a
lazyMapsCombine lazyMapLeft lazyMapRight subtreeSize =
  case lazyMapSize lazyMapLeft < lazyMapSize lazyMapRight of
    True  -> lazyMapsCombineSmallToLarge lazyMapLeft
                                         lazyMapRight
                                         (hashStepLeft subtreeSize)
                                         hashCombine

    False -> lazyMapsCombineSmallToLarge lazyMapRight
                                         lazyMapLeft
                                         (hashStepRight subtreeSize)
                                         hashCombineRev

castHashOptimized :: (Ord a, Hashable a)
                  => Expr h a -> Expr Hash a
castHashOptimized e = exprs
  where (_m, _b, _depth, _subtreeSize, exprs) =
          castHashOptimizedExplicit ([], 1) Map.empty e

castHashOptimizedExplicit :: (Ord a, Hashable a)
                          => (Path, Hash)
                          -> Map a Hash
                          -> Expr h a
                          -> (LazyMap a, Hash, Int, Int, Expr Hash a)
castHashOptimizedExplicit =
  let subExprHash_ (LazyMap _ sndVariablesHash) structureHash =
        hash (sndVariablesHash, structureHash)

  in \(path, pathHash) bvEnv expr -> case expr of
  Var _ x   -> (variablesHash, structureHash, 0, 1, Var subExprHash x)
    where variablesHash = lazyMapSingleton x (TwoHashes (hashExprO VarO)
                                                    (hash (Map.lookup x bvEnv)))
          structureHash = hashExprO VarO
          subExprHash   = subExprHash_ variablesHash structureHash

  Lam _ x e -> (variablesHash, structureHash, depth + 1, subtreeSize, Lam subExprHash x subExprHashes)
    where variablesHash = lazyMapDelete x variablesHashE
          structureHash = hashExprOWithSalt hashSalt (LamO hashX structureHashE)
          (!variablesHashE, !structureHashE, !depth, !subtreeSizeE, subExprHashes) =
            castHashOptimizedExplicit (L:path, hash (pathHash, L))
                                                  (Map.insert x pathHash bvEnv)
                                                  e
          subtreeSize   = subtreeSizeE + 1
          hashSalt      = hash (depth, lazyMapSize variablesHash)
          subExprHash   = subExprHash_ variablesHash structureHash
          hashX         = fmap fst_ (lazyMapLookup x variablesHashE)
            where fst_ (TwoHashes h _) = h

  App _ f e ->
    (variablesHash, structureHash, max depthF depthE + 1, subtreeSize, App subExprHash subExprHashesF subExprHashesE)
    where variablesHash = lazyMapsCombine variablesHashF variablesHashE subtreeSize

          structureHash =
            hashExprOWithSalt hashSalt
                              (AppO (Just structureHashF) (Just structureHashE))

          subtreeSize   = subtreeSizeF + subtreeSizeE + 1
          hashSalt      = hash (hashWithSalt depthF depthE,
                                lazyMapSize variablesHash)
          subExprHash   = subExprHash_ variablesHash structureHash

          (!variablesHashF, !structureHashF, !depthF, !subtreeSizeF, subExprHashesF)
            = castHashOptimizedExplicit (Apl:path, hash (pathHash, Apl)) bvEnv f
          (!variablesHashE, !structureHashE, !depthE, !subtreeSizeE, subExprHashesE)
            = castHashOptimizedExplicit (Apr:path, hash (pathHash, Apr)) bvEnv e

type HashCode = (Int, Hash)

spjLocallyNameless :: (Hashable a, Ord a) => Expr h a -> Expr Hash a
spjLocallyNameless = mapAnnotation hash . spjLocallyNamelessExplicit

spjLocallyNamelessExplicit :: (Ord a, Hashable a) => Expr h a -> Expr HashCode a
-- Decorates an expression with a
-- hash-code at every node
spjLocallyNamelessExplicit (Var _ n)     = Var (0, hash n) n
spjLocallyNamelessExplicit (App _ e1 e2) = App h he1 he2
  where
        he1 = spjLocallyNamelessExplicit e1
        he2 = spjLocallyNamelessExplicit e2
        h =   ((1 + (max `on` (fst . annotation)) he1 he2),
               hash ("App", annotation he1, annotation he2))

spjLocallyNamelessExplicit e_@(Lam _ n e) = Lam h n (spjLocallyNamelessExplicit e)
  where
    h = hashOnly emptyEnv e_
        -- Yikes!  A second full traversal of e

    -- Does not return a decorated expression, only a hash code
    -- All nested lambdas are dealt with via deBruijn
    hashOnly env (Var _ n')     = (0, case lookupEnv env n' of
      Just h' -> h'
      Nothing -> hash n')
    hashOnly env (App _ e1 e2) = (1 + (max `on` fst) h1 h2,
                              hash ("App", h1, h2))
      where h1 = hashOnly env e1
            h2 = hashOnly env e2
    hashOnly env (Lam _ n' e')   = (fst h' + 1, hash ("Lam", h'))
      where h' = hashOnly (extendEnv env n') e'

    lookupEnv (_, env) n' = Map.lookup n' env

    extendEnv (i, env) n'
      = (i+1, Map.insert n' (hash i) env)

    emptyEnv = (0 :: Int, Map.empty)

-- | Whether two expressions are alpha-equivalent, implemented using
-- 'castHashTop'
alphaEquivalentAccordingToHashExpr :: (Ord a, Hashable a)
                                   => Expr h a -> Expr h a -> Bool
alphaEquivalentAccordingToHashExpr = (==) `on` castHashTop

alphaEquivalentAccordingToSummariseExpr :: Ord name
                                        => Expr h name
                                        -> Expr h name
                                        -> Bool
alphaEquivalentAccordingToSummariseExpr = (==) `on` KATHash1.summariseExpr

-- | Makes binders unique whilst preserving alpha-equivalence.  The
-- binders are replaced with integers starting from zero and
-- increasing in left-to-right depth-first order.
--
-- In consequence, two expressions are alpha-equivalent if they are
-- equal under @uniqifyBinders@.
uniquifyBinders :: Ord a => Expr h a -> Expr h (Either a Int)
uniquifyBinders = fst . uniquifyBindersExplicit Map.empty 0

-- | The internals of 'uniquifyBinders'
uniquifyBindersExplicit :: Ord a
                        => Map.Map a Int
                        -> Int
                        -> Expr h a
                        -> (Expr h (Either a Int), Int)
uniquifyBindersExplicit m n = \case
  Var h x -> case Map.lookup x m of
    Nothing -> (Var h (Left x), n)
    Just i  -> (Var h (Right i), n)
  Lam h x e -> (Lam h (Right n) e', n')
    where (e', n') = uniquifyBindersExplicit (Map.insert x n m) (n+1) e
  App h f x -> (App h f' x', n'')
    where (f', n')  = uniquifyBindersExplicit m n f
          (x', n'') = uniquifyBindersExplicit m n' x

-- | (Broken) DeBruijin Algorithm from "Finding Identical
-- Subexpressions"
deBruijnHash :: (Hashable a, Ord a) => Expr h a -> Expr Hash a
deBruijnHash expr = es
  where (_, _, es) = deBruijnHashExplicit Map.empty [] expr

deBruijnHashExplicit :: (Hashable a, Ord a)
                     => Map.Map a Int
                     -> Path
                     -> Expr h a
                     -> (Hash, Int, Expr Hash a)
deBruijnHashExplicit = \env path expr -> case expr of
  Var _ x -> (hash', depth', Var hash' x)
    where hash' = case dbLookupVar x env of
            Nothing -> hash ("free", x, depth')
            Just i  -> hash ("bound", i, depth')
          depth' = 0 :: Int
  Lam _ x e -> (hash', depth', Lam hash' x subExpressionHashesE)
    where (!hashE, !depthE, subExpressionHashesE) =
            deBruijnHashExplicit (dbAddVar x env) (L:path) e
          depth' = depthE + 1
          hash' = hash ("lam", hashE, depth')
  App _ f e -> (hash', depth', App hash' lF lE)
    where (!hashF, !depthF, lF) = deBruijnHashExplicit env (Apl:path) f
          (!hashE, !depthE, lE) = deBruijnHashExplicit env (Apr:path) e
          depth' = max depthF depthE + 1
          hash'  = hash ("app", hashF, hashE, depth')

deBruijnNestedHash :: (Hashable a, Ord a) => Expr h a -> Expr Hash a
deBruijnNestedHash = deBruijnNestedHashExplicit []

deBruijnNestedHashExplicit :: (Hashable a, Ord a)
                           => Path
                           -> Expr h a
                           -> Expr Hash a
deBruijnNestedHashExplicit = \path expr ->
  let hash' = annotation (deBruijnHash expr)
      allHashes = case expr of
        Var _ x -> Var hash' x
        Lam _ x e -> Lam hash' x (deBruijnNestedHashExplicit (L:path) e)
        App _ f e ->
          App hash' (deBruijnNestedHashExplicit (Apl:path) f)
                    (deBruijnNestedHashExplicit (Apr:path) e)
  in allHashes

dbAddVar :: Ord k => k -> Map k Int -> Map k Int
dbAddVar v env = Map.insert v (Map.size env) env

dbLookupVar :: Ord k => k -> Map k Int -> Maybe Int
dbLookupVar v env = fmap (Map.size env -) (Map.lookup v env)

combinedHash :: (Ord a, Hashable a) => Expr h a -> Expr Hash a
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
                     -> Expr h a
                     -> (Hash, Set a, Int, Expr Hash a)
combinedHashExplicit = \env fvEnv location expr ->
  let fvHash freeVars = map (flip Map.lookup fvEnv) (Set.toList freeVars)
  in case expr of
  Var _ x -> (dbHash', freeVars', depth', Var jointHash' x)
    where dbHash' = case dbLookupVar x env of
            Nothing -> hash ("free", x, depth')
            Just i  -> hash ("bound", i, depth')
          depth' = 0 :: Int
          freeVars' = Set.singleton x
          jointHash' = hash (dbHash', fvHash freeVars')
  Lam _ x e -> (dbHash', freeVars', depth', Lam jointHash' x subExpressionHashesE)
    where (dbHashE, freeVarsE, depthE, subExpressionHashesE) =
            combinedHashExplicit (dbAddVar x env)
                                 (addLocn x location fvEnv)
                                 (L:location) e
          depth' = depthE + 1
          dbHash' = hash ("lam", dbHashE, depth')
          jointHash' = hash (dbHash', fvHash freeVars')
          freeVars' = Set.delete x freeVarsE
  App _ f e -> (dbHash', freeVars', depth', App jointHash' subExpressionHashesF subExpressionHashesE)
    where (dbHashF, freeVarsF, depthF, subExpressionHashesF)  =
            combinedHashExplicit env fvEnv (Apl:location) f
          (dbHashE, freeVarsE, depthE, subExpressionHashesE) =
            combinedHashExplicit env fvEnv (Apr:location)  e
          depth'  = max depthF depthE + 1
          dbHash' = hash ("app", dbHashF, dbHashE, depth')
          jointHash' = hash (dbHash', fvHash freeVars')
          freeVars' = Set.union freeVarsF freeVarsE

addLocn :: Ord k => k -> a -> Map k a -> Map k a
addLocn = Map.insert

-- | The most basic hash one could think of.  Is not intended to
-- respect any kind of equivalences.
naiveHash :: Hashable a => Expr h a -> Hash
naiveHash = \case
  Var _ x   -> hash x
  Lam _ x e -> hash (x, naiveHash e)
  App _ f e -> hash (naiveHash f, naiveHash e)

naiveHashNested :: Hashable a => Expr h a -> Expr Hash a
naiveHashNested e = es
  where (_, es) = naiveHashNestedExplicit [] e

naiveHashNestedExplicit :: Hashable a
                        => Path
                        -> Expr h a
                        -> (Hash, Expr Hash a)
naiveHashNestedExplicit path expr =
  case expr of
  Var _ x   -> (thisHash, Var thisHash x)
    where thisHash = hash x

  Lam _ x e -> (thisHash, Lam thisHash x subExpressionHashes)
    where (h, subExpressionHashes) = naiveHashNestedExplicit (L:path) e
          thisHash                 = hash (x, h)

  App _ f e -> (thisHash, App thisHash subExpressionHashesL subExpressionHashesR)
    where (hL, subExpressionHashesL) =
            naiveHashNestedExplicit (Apl:path) f
          (hR, subExpressionHashesR) =
            naiveHashNestedExplicit (Apr:path) e
          thisHash                   = hash (hL, hR)

naiveHashWithBinders :: (Ord a, Hashable a)
                     => Expr h a -> Expr Hash a
naiveHashWithBinders e = exprs
  where (_h, _depth, exprs) = naiveHashWithBindersExplicit [] Map.empty e

naiveHashWithBindersExplicit :: (Ord a, Hashable a)
                             => Path
                             -> Map a Path
                             -> Expr h a
                             -> (Hash, Int, Expr Hash a)
naiveHashWithBindersExplicit location env expr = case expr of
  Var _ x -> (hash', depth', Var hash' x)
    where hash' = hash $ case Map.lookup x env of
            Nothing -> hash ("free", x, depth')
            Just p  -> hash ("bound", p, depth')
          depth' = 0
  Lam _ x e -> (hash', depth', Lam hash' x subExpressionHashesE)
    where (hashE, depthE, subExpressionHashesE) =
            naiveHashWithBindersExplicit (L:location) (Map.insert x location env) e
          hash' = hash ("lam", hashE, depth')
          depth' = depthE + 1
  App _ f e -> (hash', depth', App hash' subExpressionHashesF subExpressionHashesE)
    where (hashF, depthF, subExpressionHashesF) =
            naiveHashWithBindersExplicit (Apl:location) env f
          (hashE, depthE, subExpressionHashesE) =
            naiveHashWithBindersExplicit (Apr:location) env e
          hash' = hash ("app", hashF, hashE, depth')
          depth' = max depthF depthE + 1

naiveHashWithBinders2 :: (Ord a, Hashable a)
                     => Expr h a -> Expr Hash a
naiveHashWithBinders2 e = exprs
  where (_h, _depth, exprs) = naiveHashWithBinders2Explicit [] Map.empty e

naiveHashWithBinders2Explicit :: (Ord a, Hashable a)
                              => Path
                              -> Map a Path
                              -> Expr h a
                              -> (Hash, Int, Expr Hash a)
naiveHashWithBinders2Explicit location env expr = case expr of
  Var _ x -> (hash', depth', Var hash' x)
    where hash' = hash $ case Map.lookup x env of
            Nothing -> hash ("free", x, depth')
            Just p  -> hash ("bound", p, depth')
          depth' = 0
  Lam _ x e -> (hash', depth', Lam hash' x subExpressionHashesE)
    where (hashE, depthE, subExpressionHashesE) =
            naiveHashWithBinders2Explicit (L:location)
                                         (Map.insert x [] (Map.map (L:) env)) e
          hash' = hash ("lam", hashE, depth')
          depth' = depthE + 1
  App _ f e -> (hash', depth', App hash' subExpressionHashesF subExpressionHashesE)
    where (hashF, depthF, subExpressionHashesF) =
            naiveHashWithBinders2Explicit (Apl:location) (Map.map (Apl:) env) f
          (hashE, depthE, subExpressionHashesE) =
            naiveHashWithBinders2Explicit (Apr:location) (Map.map (Apr:) env) e
          hash' = hash ("app", hashF, hashE, depth')
          depth' = max depthF depthE + 1



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
alphaEquivalentAccordingToUniquifyBinders :: Ord a => Expr () a -> Expr () a -> Bool
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
      examples = [ (Lam () "x"   (Var () "x"),
                    Lam () (b 0) (Var () (b 0)))

                 , (Lam () "x"   (Var () "y"),
                    Lam () (b 0) (Var () (f "y")))

                 , (Lam () "x"   (Lam () "y"   (Var () "x")),
                    Lam () (b 0) (Lam () (b 1) (Var () (b 0))))

                 , (Lam () "x"   (Lam () "x"   (Var () "x")),
                    Lam () (b 0) (Lam () (b 1) (Var () (b 1))))

                 , (Lam () "x"   (App () (Var () "x")   (Var () "x")),
                    Lam () (b 0) (App () (Var () (b 0)) (Var () (b 0))))

                 , (App () (Lam () "x"   (Var () "x"))   (Lam () "x"   (Var () "x")),
                    App () (Lam () (b 0) (Var () (b 0))) (Lam () (b 1) (Var () (b 1))))
                 ]

  flip mapM_ examples $ \(expression, uniquified) ->
    uniquifyBinders expression === uniquified

-- | Shows when our hashing functions argree and disagree on our
-- examples
prop_compareSubExpressionHashes :: Property
prop_compareSubExpressionHashes = withTests 1 $ property $ do
  let n = normalizedGroupedEquivalentSubexpressions . allHashResults

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
  let paths = map (\(_, path, _) -> path) . allHashResults

  expr <- forAll genExpr

  let h = castHash expr
      d = deBruijnHash expr
      c = combinedHash expr
      n = naiveHashNested expr
      n1 = naiveHashWithBinders expr
      n2 = naiveHashWithBinders2 expr

  paths h === paths d
  paths h === paths c
  paths h === paths n
  paths h === paths n1
  paths h === paths n2

numRandomTests :: TestLimit
numRandomTests = 100 * 100

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

prop_hashAlphaEquivalence2 :: Property
prop_hashAlphaEquivalence2 = withTests numRandomTests $ property $ do
  expr1 <- forAll genExpr
  expr2 <- forAll genExpr

  -- Or can use Hedgehog's "diff"
  alphaEquivalentAccordingToUniquifyBinders expr1 expr2
    === alphaEquivalentAccordingToSummariseExpr expr1 expr2

prop_rebuild :: Property
prop_rebuild = withTests numRandomTests $ property $ do
  expr1Char <- forAll genExpr
  let expr1 = fmap ord expr1Char
      esummary = KATHash1.summariseExpr expr1
      expr2 = KATHash1.rebuild (+1) (0 :: Int) esummary
  assert (alphaEquivalentAccordingToUniquifyBinders expr1 expr2)

prop_rebuild2 :: Property
prop_rebuild2 = withTests numRandomTests $ property $ do
  expr1Char <- forAll genExpr
  let expr1 = fmap ord expr1Char
      esummary = KATHash2.summariseExpr expr1
      expr2 = KATHash2.rebuild (+1) (0 :: Int) esummary
  assert (alphaEquivalentAccordingToUniquifyBinders expr1 expr2)

prop_rebuild3 :: Property
prop_rebuild3 = withTests numRandomTests $ property $ do
  expr1Char <- forAll genExpr
  let expr1 = fmap ord expr1Char
      esummary = KATHash3.summariseExpr expr1
      expr2 = KATHash3.rebuild (+1) (0 :: Int) esummary
  assert (alphaEquivalentAccordingToUniquifyBinders expr1 expr2)

prop_rebuildFastOrig :: Property
prop_rebuildFastOrig = withTests numRandomTests $ property $ do
  expr1Char <- forAll genExpr
  let expr1 = fmap ord expr1Char
      esummary = KATHashFastOrig.summariseExpr expr1
      expr2 = KATHashFastOrig.rebuild (+1) (0 :: Int) esummary
  assert (alphaEquivalentAccordingToUniquifyBinders expr1 expr2)

prop_fastMatches3 :: Property
prop_fastMatches3 = withTests numRandomTests $ property $ do
  expr1 <- forAll genExpr
  let summary1 = KATHash3.summariseExpr expr1
      summary2 = KATHashFast.fastTo3 (KATHashFast.summariseExpr expr1)
  summary1 === summary2

-- | Generates random expressions for testing
genExprWithVarsTest :: MonadGen m => [v] -> m (Expr () v)
genExprWithVarsTest vars = genExprWithVars_vars
-- Hedgehog has an example for exactly this use case!
--
-- http://hackage.haskell.org/package/hedgehog-1.0.2/docs/Hedgehog-Gen.html#v:recursive
  where genExprWithVars_vars = Gen.recursive
          Gen.choice
          [ Var () <$> Gen.element vars ]
          [ Gen.subtermM genExprWithVars_vars (\e -> Lam () <$> Gen.element vars <*> pure e)
          , Gen.subterm2 genExprWithVars_vars genExprWithVars_vars (App ())
          ]

-- | Generates random expressions for benchmarking
genExprWithVars :: MonadGen m => [v] -> m (Expr () v)
genExprWithVars vars = do
  size <- Gen.int (Range.linear 0 2000)
  genExprWithVarsSize size vars

genExprWithVarsSize :: MonadGen m => Int -> [v] -> m (Expr () v)
genExprWithVarsSize size vars =
  if size <= 0
  then Var () <$> Gen.element vars
  else Gen.choice
       [ do sizeL <- Gen.int (Range.constant 0 size)
            let sizeR = size - sizeL
            App () <$> genExprWithVarsSize sizeL vars
                   <*> genExprWithVarsSize sizeR vars
       , do Lam () <$> Gen.element vars <*> genExprWithVarsSize (size - 1) vars
       ]

-- | Generate expressions that are completely unbalanced, for
-- benchmarking the worst cases of some of our hashing algorithms.
genExprWithVarsLinear :: MonadGen m => [a] -> m (Expr () a)
genExprWithVarsLinear vars = do
  size <- Gen.int (Range.linear 0 2000)
  genExprWithVarsLinearSize size vars

genExprWithVarsLinearSize :: MonadGen m => Int -> [a] -> m (Expr () a)
genExprWithVarsLinearSize size vars =
  if size <= 0
  then Var () <$> Gen.element vars
  else App () <$> (Lam () <$> Gen.element vars <*> e)
              <*> (Var () <$> Gen.element vars)
  where e = genExprWithVarsLinearSize (size -1) vars

-- | Generates random expressions for testing
genExpr :: MonadGen m => m (Expr () Char)
genExpr = genExprWithVarsTest ['u'..'z']

genExprNumVars :: MonadGen m => Int -> m (Expr () String)
genExprNumVars n = genExprWithVars (map show [1..n])

genExprLinearNumVars :: MonadGen m => Int -> m (Expr () String)
genExprLinearNumVars n = genExprWithVarsLinear (map show [1..n])

-- | Shows equivalence of castHash hash and castHashOptimized hash
prop_equivCastFast :: Property
prop_equivCastFast = withTests numRandomTests $ property $ do
  let n = normalizedGroupedEquivalentSubexpressions . allHashResults
  expr <- forAll (fmap uniquifyBinders genExpr)
  let castHash_groups = n (castHash expr)
      castHashOptimized_groups = n (castHashOptimized expr)
      spjLocallyNameless_groups = n (spjLocallyNameless expr)

  castHash_groups === castHashOptimized_groups
  castHash_groups === spjLocallyNameless_groups

prop_applyPrefix :: Property
prop_applyPrefix = KATHashFast.prop_applyPrefix numRandomTests

prop_rebuildSApp3_inverse :: Property
prop_rebuildSApp3_inverse =
  KATHash3.prop_rebuildSApp3_inverse genExpr numRandomTests

prop_rebuildSApp_inverse :: Property
prop_rebuildSApp_inverse =
  KATHashFastOrig.prop_rebuildSApp_inverse genExpr numRandomTests
