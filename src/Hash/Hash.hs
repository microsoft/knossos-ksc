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
--     > writeFormatExpressionsHTML "filename.html"
--
-- The sub-expression hashing algorithms are (this list is out of date)
--
-- * castHash (structural and free-vars hash)
-- * deBruijnHash (known to be broken)
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
import Control.Monad.Trans.State
import Data.Char (ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Hashable (Hashable, hash)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import System.Random (Random, randomRIO, randomIO)

import Expr (Expr(Var, Lam, App), Path,
             allSubexprs, annotation, mapAnnotation)
import qualified KATHashInefficient
import qualified KATHashEfficient
import qualified KATHashEfficientHash
import qualified KATHashOptimizedHash
import KATHashEfficientHash (thenHash)

-- | A helper type that is intended to make the hashing algorithm
-- clearer.  If it doesn't help I may just get rid of it.
data ExprO v expr =
    VarO
  | LamO v expr
  | AppO (Maybe expr) (Maybe expr)

hashExprO :: ExprO (Maybe Hash) Hash -> Hash
hashExprO = \case
  VarO -> 0
  LamO v e   -> hash (1 :: Int) `thenHash` v `thenHash` hash e
  AppO e1 e2 -> hash (2 :: Int) `thenHash` e1 `thenHash` e2

type Hash = Int

allHashResults :: Expr b a -> [(b, Path, Expr () a)]
allHashResults = f . allHashResults_
  where allHashResults_ = fmap (\(p, se) -> (annotation se, p, se)) . allSubexprs
        -- Warning: This mapAnnotation is slow, but it isn't involved
        -- in the benchmark and will likely disappear soon anyway.
        -- The reverse is also a pain.
        f = map (\(h__, p, es) -> (h__, reverse p, mapAnnotation (const ()) es))

locallyNameless :: (Hashable a, Ord a) => Expr h a -> Expr Hash a
locallyNameless = mapAnnotation hash . locallyNamelessExplicit

locallyNamelessExplicit :: (Ord a, Hashable a) => Expr h a -> Expr Hash a
-- Decorates an expression with a
-- hash-code at every node
locallyNamelessExplicit (Var _ n)     = Var (hash n) n
locallyNamelessExplicit (App _ e1 e2) = App h he1 he2
  where
        he1 = locallyNamelessExplicit e1
        he2 = locallyNamelessExplicit e2
        h =   hashExprO (AppO (Just (annotation he1)) (Just (annotation he2)))

locallyNamelessExplicit e_@(Lam _ n e) = Lam h n (locallyNamelessExplicit e)
  where
    h = hashOnly emptyEnv e_
        -- Yikes!  A second full traversal of e

    -- Does not return a decorated expression, only a hash code
    -- All nested lambdas are dealt with via deBruijn
    hashOnly env (Var _ n')    = case lookupEnv env n' of
      Just h' -> hash (3 :: Int) `thenHash` h'
      Nothing -> hash (4 :: Int) `thenHash` hash n'
    hashOnly env (App _ e1 e2) = hashExprO (AppO (Just h1) (Just h2))
      where h1 = hashOnly env e1
            h2 = hashOnly env e2
    hashOnly env (Lam _ n' e') = hashExprO (LamO Nothing h')
      where !env' = (extendEnv env n')
            h' = hashOnly env' e'

    lookupEnv (_, env) n' = Map.lookup n' env

    extendEnv (i, env) n'
      = ((,) $! (i+1)) $! Map.insert n' (hash i) env

    emptyEnv = (0 :: Int, Map.empty)

alphaEquivalentAccordingToHashExpr :: (Ord a, Hashable a)
                                   => Expr h a -> Expr h a -> Bool
alphaEquivalentAccordingToHashExpr = (==) `on` KATHashOptimizedHash.katHashTop

alphaEquivalentAccordingToSummariseExpr :: Ord name
                                        => Expr h name
                                        -> Expr h name
                                        -> Bool
alphaEquivalentAccordingToSummariseExpr = (==) `on` KATHashInefficient.summariseExpr

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
  where (_, es) = deBruijnHashExplicit Map.empty expr

deBruijnHashExplicit :: (Hashable a, Ord a)
                     => Map.Map a Int
                     -> Expr h a
                     -> (Hash, Expr Hash a)
deBruijnHashExplicit = \env -> \case
  Var _ x -> (hash', Var hash' x)
    where !hash' = case dbLookupVar x env of
            Nothing -> hash "free" `thenHash` x
            Just i  -> hash "bound" `thenHash` i
  Lam _ x e -> (hash', Lam hash' x subExpressionHashesE)
    where (hashE, subExpressionHashesE) =
            deBruijnHashExplicit env' e
          !hash'  = hash "lam" `thenHash` hashE
          !env'   = dbAddVar x env
  App _ f e -> (hash', App hash' lF lE)
    where (hashF, lF) = deBruijnHashExplicit env f
          (hashE, lE) = deBruijnHashExplicit env e
          !hash'  = hash "app" `thenHash` hashF `thenHash` hashE

deBruijnNestedHash :: (Hashable a, Ord a) => Expr h a -> Expr Hash a
deBruijnNestedHash = deBruijnNestedHashExplicit

deBruijnNestedHashExplicit :: (Hashable a, Ord a)
                           => Expr h a
                           -> Expr Hash a
deBruijnNestedHashExplicit = \expr ->
  let hash' = annotation (deBruijnHash expr)
      allHashes = case expr of
        Var _ x -> Var hash' x
        Lam _ x e -> Lam hash' x (deBruijnNestedHashExplicit e)
        App _ f e ->
          App hash' (deBruijnNestedHashExplicit f)
                    (deBruijnNestedHashExplicit e)
  in allHashes

dbAddVar :: Ord k => k -> Map k Int -> Map k Int
dbAddVar v env = Map.insert v (Map.size env) env

dbLookupVar :: Ord k => k -> Map k Int -> Maybe Int
dbLookupVar v env = fmap (Map.size env -) (Map.lookup v env)

addLocn :: Ord k => k -> a -> Map k a -> Map k a
addLocn = Map.insert

structuralHashNested :: Hashable a => Expr h a -> Expr Hash a
structuralHashNested e = es
  where (_, es) = structuralHashNestedExplicit e

structuralHashNestedExplicit :: Hashable a
                        => Expr h a
                        -> (Hash, Expr Hash a)
structuralHashNestedExplicit = \case
  Var _ x   -> (thisHash, Var thisHash x)
    where !thisHash = hash "Var" `thenHash` x

  Lam _ x e -> (thisHash, Lam thisHash x subExpressionHashes)
    where (h, subExpressionHashes) = structuralHashNestedExplicit e
          !thisHash                = hash "Lam" `thenHash` x `thenHash` h

  App _ f e -> (thisHash, App thisHash subExpressionHashesL subExpressionHashesR)
    where (hL, subExpressionHashesL) = structuralHashNestedExplicit f
          (hR, subExpressionHashesR) = structuralHashNestedExplicit e
          !thisHash                  = hash "App" `thenHash` hL `thenHash` hR

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
alphaEquivalentAccordingToUniquifyBinders :: (Eq h, Ord a) => Expr h a -> Expr h a -> Bool
alphaEquivalentAccordingToUniquifyBinders = (==) `on` uniquifyBinders

testEverythingInFileStartingWith'prop_' :: IO ()
testEverythingInFileStartingWith'prop_' = checkParallel $$(discover) >> pure ()

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

-- | Checks that the paths come out of the algorithms in the same
-- order (which just so happens to be depth first preorder).  This is
-- not an essential property of the algorithms, but it's nice that
-- they are thus normalised so that we can compare behaviour more
-- easily.
prop_stablePaths :: Property
prop_stablePaths = withTests numRandomTests $ property $ do
  let paths = map (\(_, path, _) -> path) . allHashResults

  expr <- forAll genExpr

  let d = deBruijnHash expr
      n = structuralHashNested expr

  paths d === paths n

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

propG_rebuild :: (Expr () Int -> t)
              -> ((Int -> Int) -> Int -> t -> Expr () Int)
              -> Property
propG_rebuild summariseExpr rebuild = withTests numRandomTests $ property $ do
  expr1Char <- forAll genExpr
  let expr1 = fmap ord expr1Char
      esummary = summariseExpr expr1
      expr2 = rebuild (+1) (0 :: Int) esummary
  assert (alphaEquivalentAccordingToUniquifyBinders expr1 expr2)

prop_rebuild3 :: Property
prop_rebuild3 = propG_rebuild KATHashInefficient.summariseExpr KATHashInefficient.rebuild

prop_rebuildFastOrig :: Property
prop_rebuildFastOrig =
  propG_rebuild KATHashEfficient.summariseExpr KATHashEfficient.rebuild

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
genExprWithVars' :: (Integral v, MonadGen m)
                 => [v] -> v -> (v -> v) -> m (Expr () v)
genExprWithVars' inScope fresh freshen = flip evalStateT (inScope, fresh) $ do
  size <- Gen.int (Range.linear 0 2000)
  genExprWithVarsSize' size fresh freshen

genExprWithVarsSize' :: (Integral v, MonadGen m)
                     => Int -> v -> (v -> v) -> m (Expr () v)
genExprWithVarsSize' size fresh freshen = flip evalStateT fresh $ do
  let freshVar = do
        fresh' <- get
        put (freshen fresh')
        pure fresh'

  let elementInScope = do
        fresh' <- get
        Gen.integral (Range.constant 0 (fresh' - 1))

  genExprWithVarsSizeG size elementInScope freshVar

genExprWithVarsSizeG :: MonadGen m => Int -> m v -> m v -> m (Expr () v)
genExprWithVarsSizeG size vars binders =
  if size <= 1
  then Var () <$> vars
  else Gen.choice
       [ do sizeL <- Gen.int (Range.constant 1 (size - 2))
            let sizeR = size - sizeL - 1
            App () <$> genExprWithVarsSizeG sizeL vars binders
                   <*> genExprWithVarsSizeG sizeR vars binders
       , do Lam () <$> binders <*> genExprWithVarsSizeG (size - 1) vars binders
       ]

genExprWithVarsSize :: (Random a, Integral a)
                    => Int -> a -> IO (Expr () a)
genExprWithVarsSize size fresh =
  if size <= 1
  then Var () <$> vars
  else do
    app <- randomIO
    if app
      then do
      sizeL <- randomRIO (1, size - 2)
      let sizeR = size - sizeL - 1
      App () <$> genExprWithVarsSize sizeL fresh
             <*> genExprWithVarsSize sizeR fresh
      else
      Lam () <$> binders <*> genExprWithVarsSize (size - 1) (fresh + 1)
  where vars    = randomRIO (0, fresh - 1)
        binders = pure fresh

genExprWithVarsUnbalancedSize :: (Random a, Integral a)
                              => Int -> a -> IO (Expr () a)
genExprWithVarsUnbalancedSize size fresh =
  if size <= 1
  then Var () <$> vars
  else App () <$> (Lam () <$> binders <*> e)
              <*> (Var () <$> vars)
  where vars    = randomRIO (0, fresh - 1)
        binders = pure fresh
        e = genExprWithVarsUnbalancedSize (size - 3) (fresh + 1)

genExprWithVarsUnbalancedSizeG :: MonadGen m => Int -> m a -> m a -> m (Expr () a)
genExprWithVarsUnbalancedSizeG size vars binders =
  if size <= 1
  then Var () <$> vars
  else App () <$> (Lam () <$> binders <*> e)
              <*> (Var () <$> vars)
  where e = genExprWithVarsUnbalancedSizeG (size - 3) vars binders

-- | Generates random expressions for testing
genExpr :: MonadGen m => m (Expr () Char)
genExpr = genExprWithVarsTest ['u'..'z']

genExprNumVars :: MonadGen m => Int -> m (Expr () Int)
genExprNumVars n = genExprWithVars' [1..n] (n+1) (+1)

-- | Shows equivalence of castHash hash and the other algorithms
prop_equivCastFast :: Property
prop_equivCastFast = withTests numRandomTests $ property $ do
  let n = normalizedGroupedEquivalentSubexpressions . allHashResults
  expr <- forAll (fmap uniquifyBinders genExpr)
  let locallyNameless_groups = n (locallyNameless expr)
      deBruijnNestedHash_groups = n (deBruijnNestedHash expr)
      katHashEfficientHash_groups = n (KATHashEfficientHash.katHash expr)
      katHashFasterOrigHash_groups = n (KATHashOptimizedHash.katHash expr)

  locallyNameless_groups === deBruijnNestedHash_groups
  locallyNameless_groups === katHashEfficientHash_groups
  locallyNameless_groups === katHashFasterOrigHash_groups

prop_rebuildSApp3_inverse :: Property
prop_rebuildSApp3_inverse =
  KATHashInefficient.prop_rebuildSApp3_inverse genExpr numRandomTests

prop_rebuildSApp_inverse :: Property
prop_rebuildSApp_inverse =
  KATHashEfficient.prop_rebuildSApp_inverse genExpr numRandomTests

prop_fastFaster :: Property
prop_fastFaster = withTests numRandomTests $ property $ do
  expr <- forAll genExpr
  KATHashEfficientHash.katHash expr === KATHashOptimizedHash.katHash expr
