-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Autodiff where

import           Prelude hiding (reverse)
import           Control.Arrow (first, second)
import           Control.Monad.Cont as C
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Foldable as F
import qualified Data.Set
import qualified Data.Traversable as T
import           Data.Tree (Tree(Node), drawTree)

data Expr a = Var a
            | Lit Float
            | Op (Op (Expr a))
            | LetIn String (Expr a) (Expr a)
            deriving (Show, Functor)

instance Applicative Expr where
  pure = return
  (<*>) = ap

instance Monad Expr where
  return = Var
  m >>= f = case m of
    Var a -> f a
    Lit l -> Lit l
    Op o -> Op (fmap (>>= f) o)
    LetIn v e0 e1 -> LetIn v (e0 >>= f) (e1 >>= f)

data Op a =
    (:+) a a
  | (:*) a a
  | (:-) a a
  | (:/) a a
    deriving (Show, Functor)

(.-), (.+), (.*), (./) :: Expr a -> Expr a -> Expr a
a .- b = Op (a :- b)
a .* b = Op (a :* b)
a .+ b = Op (a :+ b)
a ./ b = Op (a :/ b)

instance Foldable Op where
  foldMap = T.foldMapDefault

instance Traversable Op where
  traverse f = \case
    (:+) a b -> (:+) <$> f a <*> f b
    (:-) a b -> (:-) <$> f a <*> f b
    (:*) a b -> (:*) <$> f a <*> f b
    (:/) a b -> (:/) <$> f a <*> f b

evalOp :: Fractional a => Op a -> a
evalOp = \case
  a :+ b -> a + b
  a :- b -> a - b
  a :* b -> a * b
  a :/ b -> a / b

toTree :: Expr String -> Tree String
toTree = \case
  Var v -> Node v []
  Lit l -> Node (show l) []
  Op o -> Node (symbol o) (map toTree (F.toList o))
    where symbol = \case
            (:+) _ _ -> "+"
            (:-) _ _ -> "-"
            (:*) _ _ -> "*"
            (:/) _ _ -> "/"

  LetIn v e0 e1 -> Node ("Let " ++ v ++ " =") (map toTree [e0, e1])

printExpr :: Expr String -> IO ()
printExpr = putStrLn . drawTree . toTree

showExprBetter :: Expr String -> String
showExprBetter = \case
  Var v -> v
  Lit l -> show l
  Op o -> ("( " ++) $ (++ " )") $ case fmap showExprBetter o of
    a :+ b -> a ++ " + " ++ b
    a :- b -> a ++ " - " ++ b
    a :* b -> a ++ " * " ++ b
    a :/ b -> a ++ " / " ++ b
  LetIn v e0 e1 -> "let " ++ v ++ " = " ++ showExprBetter e0 ++ "\nin " ++ showExprBetter e1

eval :: Map.Map String Float -> Expr String -> Float
eval x = \case
  Var v -> case Map.lookup v x of
    Just x_ -> x_
    Nothing -> error ("Variable " ++ v ++ " was not present")
  Lit l -> l
  Op o -> evalOp $ fmap (eval x) o
  LetIn v e0 e1 -> eval (Map.insert v (eval x e0) x) e1

forward :: Map.Map String Float -> Map.Map String Float -> Expr String -> (Float, Float)
forward x dx = \case
  Var v -> (case Map.lookup v x of
    Just x_ -> x_
    Nothing -> error ("Variable " ++ v ++ " was not present"),
            case Map.lookup v dx of
    Just dx_ -> dx_
    Nothing -> error ("Variable " ++ v ++ " was not present"))
  Lit l -> (l, 0)
  Op o -> case fmap (forward x dx) o of
           (a, da) :+ (b, db) -> (a + b, da + db)
           (a, da) :- (b, db) -> (a - b, da - db)
           (a, da) :* (b, db) -> (a * b, a * db + b * da)
           (a, da) :/ (b, db) -> (a / b, -a / (b * b) * db + 1 / b * da)
  LetIn v e0 e1 ->
    let (e0_, de0) = forward x dx e0
    in forward (Map.insert v e0_ x) (Map.insert v de0 dx) e1
               
forwardExpr :: Int -> Map.Map String String -> Expr String -> (((String, String) -> Expr String) -> Expr String)
forwardExpr uid dx e = \rest -> fst (fst (flip S.runState uid (C.runContT (forwardExprC dx e) (return . (\x -> (rest x, ()))))))

forwardExprSimple :: Expr String -> Expr String
forwardExprSimple expr =
  forwardExpr 0 (dx expr) expr (\(_, dr) -> Var dr)
  where dx :: Expr String -> Map.Map String String
        dx = Map.fromList . fmap (\v -> (v, "d"++v)) . F.toList . freeVars

forwardExprC :: Map.Map String String -> Expr String -> C.ContT (Expr String, r) (S.State Int) (String, String)
forwardExprC dx e = case e of
  Var v -> return (v, fromMaybe (error "3") (Map.lookup v dx))
  Lit l -> r_dr (Lit l) (Lit 0)
  Op o -> do
    o' <- traverse (forwardExprC dx) o
    case o' of
            (a, da) :+ (b, db) ->
              r_dr (Var a .+ Var b) (Var db .+ Var da)
            (a, da) :- (b, db) ->
              r_dr (Var a .- Var b) (Var db .- Var da)
            (a, da) :* (b, db) ->
              r_dr (Var a .* Var b) ((Var a .* Var db) .+ (Var b .* Var da))
            (a, da) :/ (b, db) ->
              r_dr (Var a ./ Var b)
                   (((Lit 0 .- Var a) ./ (Var b .* Var b) .* Var db)
                    .+ ((Lit 1 ./ Var b) .* Var da))
  LetIn v e0 e1 -> do
    (e0_, de0) <- forwardExprC dx e0
    cont_ (LetIn v (Var e0_))
    cont_ (LetIn ("d" ++ v) (Var de0))
    forwardExprC (Map.insert v ("d" ++ v) dx) e1
  where cont_ :: Functor m => (a -> a) -> ContT (a, r) m ()
        cont_ f = C.ContT (fmap (first f) . ($ ()))

        let_ :: Expr String -> ContT (Expr String, r) (S.State Int) String
        let_ expr = do
          u <- S.get
          S.put (u + 1)
          let v = "x" ++ show u
          cont_ (LetIn v expr)
          return v

        r_dr er edr = do
          r  <- let_ er
          dr <- let_ edr
          return (r, dr)

reverse :: Map.Map String Float -> Expr String -> (Float, Float -> Map.Map String Float -> Map.Map String Float)
reverse x = \case
  Var v -> (fromMaybe (error "1") (Map.lookup v x), \d_dr -> Map.adjust (+d_dr) v)
  Lit l -> (l, const id)
  Op o -> case fmap (reverse x) o of
    (a, drda) :+ (b, drdb) -> (a + b, \ddr -> drda ddr . drdb ddr)
    (a, drda) :- (b, drdb) -> (a - b, \ddr -> drda ddr . drdb (-ddr))
    (a, drda) :* (b, drdb) -> (a * b, \ddr -> drda (b * ddr) . drdb (a * ddr))
    (a, drda) :/ (b, drdb) -> (a / b, \ddr -> drda (1 / b * ddr) . drdb (-a / (b * b) * ddr))
  LetIn v e0 e1 ->
    let (e0_, de0) = reverse x e0
        (e1_, de1) = reverse (Map.insert v e0_ x) e1
    in (e1_, \d_dr d_dxs ->
         let d_dvdxs = de1 d_dr (Map.insert v 0 d_dxs)
             d_dv = fromMaybe (error "2") (Map.lookup v d_dvdxs)
             d_dxs_ = Map.delete v d_dvdxs
         in de0 d_dv d_dxs_)

reverseExpr :: [String] -> Expr String -> Expr String
reverseExpr freevars e = fst h
  where h = flip S.evalState 0 $ evalContT $ do
          (_, rest) <- reverseExprC e
          
          d_dr <- let_ (Var "d_dr")

          d_dxs <- flip mapM freevars $ \freevar -> do
            freevaracc <- let_ (Lit 0)
            return (freevar, freevaracc)

          results <- rest d_dr (Map.fromList d_dxs)

          result <- let_ (foldl (.+) (Lit 1000) (map (Var . fromJust . flip Map.lookup results) freevars))

          finish (Var result)

        fresh = do
          uid <- lift S.get
          lift (S.put (uid + 1))
          return ("x" ++ show uid)

        cont_ f = C.ContT (fmap (first f) . ($ ()))

        finish f = C.ContT (\_ -> return (f, ()))

        evalContT = flip runContT return

        let_ expr = do
          var <- fresh
          cont_ (LetIn var expr)
          return var

reverseExprC :: Expr String
             -> ContT (Expr String, r)
                      (S.State Int)
                      (String, String -> Map.Map String String
                                      -> ContT (Expr String, r) (S.State Int) (Map.Map String String))
reverseExprC = \case
  Var v -> return (v, \d_dr d_drdxs-> do
                      var <- let_ (Var (fromJust (Map.lookup v d_drdxs))
                                   .+ Var d_dr)
                      return (Map.insert v var d_drdxs))
  Lit l -> do
    var <- let_ (Lit l)
    return (var, const return)
  Op o -> traverse reverseExprC o >>= \case
    (a, drda) :+ (b, drdb) -> do
      var <- let_ (Var a .+ Var b)
      return (var, \d_dr -> do
                 drda d_dr <=< drdb d_dr)

    (a, drda) :- (b, drdb) -> do
      var <- let_ (Var a .- Var b)
      return (var, \d_dr m -> do
                 moddr <- let_ ((Lit 0 .- Lit 1) .* Var d_dr)
                 (drda d_dr <=< drdb moddr) m)

    (a, drda) :* (b, drdb) -> do
      var <- let_ (Var a .* Var b)
      return (var, \d_dr m -> do
                 bddr <- let_ (Var b .* Var d_dr)
                 addr <- let_ (Var a .* Var d_dr)
                 (drda bddr <=< drdb addr) m)

    (a, drda) :/ (b, drdb) -> do
      var <- let_ (Var a ./ Var b)
      return (var, \d_dr m -> do
                 oobddr <- let_ ((Lit 1 ./ Var b) .* Var d_dr)
                 maobbddr <- let_ (((Lit 0 .- Var a) ./ (Var b .* Var b)) .* Var d_dr)
                 (drda oobddr <=< drdb maobbddr) m)

  LetIn v e0 e1 -> do
    (e0_, de0) <- reverseExprC e0
    cont_ (LetIn v (Var e0_))
    (e1_, de1) <- reverseExprC e1
    return (e1_, \d_dr d_dxs -> do
               zero <- let_ (Lit 0)
               d_dvdxs <- de1 d_dr (Map.insert v zero d_dxs)
               let d_dv = fromMaybe (error "5") (Map.lookup v d_dvdxs)
                   d_dxs_ = Map.delete v d_dvdxs
               de0 d_dv d_dxs_)

  where cont_ f = C.ContT (fmap (first f) . ($ ()))
        fresh = do
          uid <- lift S.get
          lift (S.put (uid + 1))
          return ("x" ++ show uid)

        let_ e = do
          var <- fresh
          cont_ (LetIn var e)
          return var

example :: Expr String
example = let x = Var "x"
              y = Var "y"
              p = Lit 7 .* x
              r = Lit 1 ./ y
              q = p .* x .* Lit 5
              v = (Lit 2 .* p .* q) .+ (Lit 3 .* r)
          in v

exampleLet :: Expr String
exampleLet = LetIn "p" (Lit 7 .* Var "x")
             $ LetIn "r" (Lit 1 ./ Var "y")
             $ LetIn "q" (Var "p" .* Var "x" .* Lit 5)
             $ LetIn "v" ((Lit 2 .* Var "p" .* Var "q") .+ (Lit 3 .* Var "r"))
             $ Var "v"

exampleSymbolic :: Map.Map String Float -> Map.Map String Float -> (Float, Float)
exampleSymbolic vars dvars =
  let x = fromJust (Map.lookup "x" vars)
      y = fromJust (Map.lookup "y" vars)
      dx = fromJust (Map.lookup "x" dvars)
      dy = fromJust (Map.lookup "y" dvars)
      p = 7 * x
      r = 1 / y
      q = p * x * 5
      v = 2 * p * q + 3 * r
      dp = 7 * dx
      dr = -1/(y * y) * dy
      dq = dp * x * 5 + p * 5 * dx
      dv = 2 * p * dq + 2 * q * dp + 3 * dr
  in (v, dv)

big :: Int -> Expr String
big 0 = Var "x0"
big n = Op (Var ("x" ++ show n) :+ (big (n - 1)))

env :: Int -> Map.Map String Float
env n = Map.fromList (map (\i -> ("x" ++ show i, 0)) [0..n])

test :: Bool
test = and [ eval (Map.fromList [("x", 1), ("y", 1)]) example
             == 493.0
           , eval (Map.fromList [("x", 1), ("y", 1)]) exampleLet
             == 493.0
           , second (($ (Map.fromList [("x", 0), ("y", 0)])) . ($ 1)) (reverse (Map.fromList [("x", 1), ("y", 1)]) exampleLet)
             == (493.0, Map.fromList [("x",1470.0),("y",-3.0)])
           , forward (Map.fromList [("x", 1), ("y", 1)]) (Map.fromList [("x", 0), ("y", 1)]) exampleLet
             == (493.0,-3.0)
           , forward (Map.fromList [("x", 1), ("y", 1)]) (Map.fromList [("x", 1), ("y", 0)]) exampleLet
             == (493.0,1470.0)
           , (eval (Map.fromList [("x", 1), ("y", 1), ("dx", 1), ("dy", 1)])
              . ($ (Var . snd)) . forwardExpr 0 (Map.fromList [("x", "dx"), ("y", "dy")])) exampleLet
             == 1467.0
           , eval (Map.fromList [("x", 1), ("y", 1)])
                  (LetIn "d_dr" (Lit 1) (reverseExpr ["x", "y"] exampleLet))
             == 2467.0
           ]

simplify :: Expr String -> Expr String
simplify = \case
  Op o -> case o of
    Lit 0 :+ e -> simplify e
    e :+ Lit 0 -> simplify e
    e :+ e'    -> Op (simplify e :+ simplify e')
    Lit 1 :* e -> e
    e :* Lit 1 -> e
    Lit 0 :* _ -> Lit 0
    _ :* Lit 0 -> Lit 0
    e :* e'    -> Op (simplify e :* simplify e')
    e -> Op e
  LetIn v (Lit l) e  -> simplify (sub (Lit l) v e)
  LetIn v (Var v') e -> simplify (sub (Var v') v e)
  LetIn v e0 e1 -> if occurrences v e1 == 0
                   then simplify e1
                   else if (occurrences v e1 == 1) && case v of { 'x':_:_ -> True; _ -> False }
                   then simplify (sub e0 v e1)
                   else LetIn v (simplify e0) (simplify e1)
  e -> e

sub :: Expr String -> String -> Expr String -> Expr String
sub l v = sub_
  where sub_ = \case
          Var v' -> if v == v' then l else Var v'
          Lit l_ -> Lit l_
          Op o -> Op (fmap sub_ o)
          LetIn v' e0 e1 -> LetIn v' (sub_ e0) (sub_ e1)

occurrences :: String -> Expr String -> Int
occurrences v = occurrences_
  where occurrences_ = \case
          Var v' -> if v == v' then 1 else 0
          Lit _  -> 0
          Op o   -> sum (fmap occurrences_ o)
          LetIn _ e0 e1 -> occurrences_ e0 + occurrences_ e1

many :: (b -> b) -> Int -> b -> b
many f n = if n == 0 then id else f . many f (n - 1)

freeVars :: Expr String -> Data.Set.Set String
freeVars = \case
  Var v -> Data.Set.singleton v
  Lit _ -> Data.Set.empty
  Op o -> foldMap freeVars o
  LetIn v e0 e1 ->
    Data.Set.union (freeVars e0) (Data.Set.delete v (freeVars e1))
