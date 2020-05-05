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
--     > writeAWFFormatExpressionsHtml "filename.html"
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

-- | Whether two expressions are alpha-equivalent, implemented using
-- 'castHash'
alphaEquivalentAccordingToHashExpr :: (Ord a, Hashable a)
                                   => Expr a -> Expr a -> Bool
alphaEquivalentAccordingToHashExpr = (==) `on` castHash

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

-- | A sanity check for both uniquifyBinders and castHash: uniquifying
-- binders should preserve alpha-equivalence and this equivalence
-- should be picked up by castHash.
prop_hashUniquifyBinders :: Property
prop_hashUniquifyBinders = withTests numRandomTests $ property $ do
  expr <- forAll genExpr
  let massageVariables = fmap Left
  assert (alphaEquivalentAccordingToHashExpr (uniquifyBinders expr)
                                             (massageVariables expr))

-- | A check for whether castHash respects alpha-equivalence (as
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

genExprWithVarsLinear :: MonadGen m => [a] -> m (Expr a)
genExprWithVarsLinear vars =
  Gen.choice [ Var <$> Gen.element vars
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             , recurse
             ]
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
