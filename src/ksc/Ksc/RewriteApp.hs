-- To run it:
--
-- ~/.ghcup/bin/cabal v2-repl --ghc-option=-Wwarn --with-ghc ~/.ghcup/ghc/8.6.5/bin/ghc
--
-- :l src/ksc/Ksc/RewriteApp.hs
--
-- main
--
-- The go to http://localhost:3000/ in your browser

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Ksc.RewriteApp where

import qualified Rules
import qualified Lang
import qualified LangUtils
import qualified Parse
import qualified Annotate
import qualified KMonad
import qualified OptLet

import qualified Data.Map
import           Data.Maybe (mapMaybe)
import           Data.List (intercalate)
import           Data.IORef (newIORef, atomicModifyIORef)
import           Data.String (fromString)
import qualified Data.Text.Lazy

import           Web.Scotty (scotty, get, liftAndCatchIO, html, param)

import           Control.Monad.Free (Free(Pure, Free), liftF)
import qualified Control.Monad.Trans.State
import           Control.Monad.Trans.State hiding (get)
import           Data.Void (Void, absurd)
import qualified Data.List.NonEmpty as NEL

import qualified Ksc.Cost (cost)

typeCheck :: [Lang.Decl] -> IO [Lang.TDecl]
typeCheck = fmap snd . KMonad.runKM . Annotate.annotDecls LangUtils.emptyGblST

parse :: [String] -> IO [Lang.Decl]
parse = fmap concat . mapM Parse.parseF

userFuns :: [Lang.DeclX p] -> [(String, Lang.ExprX p)]
userFuns = mapMaybe $ \case
       Lang.RuleDecl{} -> Nothing
       Lang.DefDecl d ->
         case (Lang.funIdOfFun (Lang.def_fun d), Lang.def_rhs d) of
           (Lang.UserFun f, Lang.UserRhs e) -> Just (f, e)
           _ -> Nothing

mkRuleBase :: [Lang.TDecl] -> Rules.RuleBase
mkRuleBase = Rules.mkRuleBase . mapMaybe (\case
       Lang.RuleDecl r -> Just r
       Lang.DefDecl{} -> Nothing)

readProgram :: String -> String -> IO (Lang.TExpr, Rules.RuleBase)
readProgram sourceFile functionName = do
  tc_decls <- typeCheck =<< parse [ "src/runtime/prelude.ks", sourceFile ]

  let rules = mkRuleBase tc_decls

  let prog = head $ flip mapMaybe (userFuns tc_decls) $ \(f, e) ->
        if f == functionName then Just e else Nothing

  return (prog, rules)

main :: IO ()
main = do
  mapOfPages <- newIORef Data.Map.empty
  let withMap = liftAndCatchIO . atomicModifyIORef mapOfPages

  let sourceFile = "test/ksc/ex0.ks"
      functionName = "f"

  let link = "<p><a href=\"/\">Start again</a></p>"

      comments = [ link
                 , "<ul>"
                 , "<li>Displays the body of the function "
                 , fromString functionName
                 , " from the source file "
                 , fromString sourceFile
                 , "</li>"
                 , "<li>Click \"start again\" to reload the "
                 , "function and rules</li>"
                 , "<li>TODO</li>"
                 , "<ul>"
                 , "<li>format the expression</li>"
                 , "<li>show all rules on RHS and highlight "
                 , "when hovered subexpression has applicable rule</li>"
                 , "<li>leaderboard</li>"
                 , "<li>support non-rules optimisations, e.g. "
                 , "constant folding, let lifting/dropping"
                 , "</ul>"
                 , "</ul>"
                 ]

  scotty 3000 $ do
    get "/" $ do
      (prog, rules) <- liftAndCatchIO $ readProgram sourceFile functionName
      s <- withMap (\m -> renderPages m (chooseLocationPages rules ([], prog)))
      html $ mconcat (comments ++ [Data.Text.Lazy.pack s])
    get "/rewrite/:word" $ do
      beam <- param "word"
      let i = read (Data.Text.Lazy.unpack beam) :: Int

      ss <- withMap $ \m ->
        case Data.Map.lookup i m of
          Nothing -> (m, comments
                       ++ ["<p>Couldn't find ", beam, ". ",
                           "You may want to ",
                           "<a href=\"/\">start again</a>.</p>"
                          ])
          Just e -> let (m', s) = renderPages m e
                    in  (m', comments ++ [Data.Text.Lazy.pack s])

      html (mconcat ss)

data Document a = Left' String
                | Right' (String, a)
                | Branch [Document a]
                deriving Functor

removeLinks :: Document a -> Document void
removeLinks = \case
  Left' s       -> Left' s
  Right' (s, _) -> Left' s
  Branch ds     -> Branch (map removeLinks ds)

data HistoryEntry a = HistoryEntry {
    heSExp   :: [Document a]
  , heCstyle :: String
  , heCost   :: Either String Float
  , heRule   :: Lang.TRule
  }

data ChooseLocationPage a =
  ChooseLocationPage { clpHistory :: [HistoryEntry Void]
                     , clpCStyle  :: String
                     , clpCost    :: Either String Float
                     , clpThisExp :: [Document a]
                     }
  deriving Functor

data ChooseRewritePage a =
  ChooseRewritePage (ChooseLocationPage a)
                    [(String, String, a)]
  deriving Functor

type ChooseLocationModel = ([(Lang.TExpr, Lang.TRule)], Lang.TExpr)

type ChooseRewriteModel = (ChooseLocationModel,
                           [(Lang.TRule, Lang.TExpr)])

data Page a = ChooseLocation (ChooseLocationPage a)
            | ChooseRewrite (ChooseRewritePage a)
  deriving Functor

mapLocationDocument :: ([Document a] -> [Document b])
                    -> ChooseLocationPage a -> ChooseLocationPage b
mapLocationDocument f (ChooseLocationPage ds s e d) =
  ChooseLocationPage ds s e (f d)

setList :: Int -> a -> [a] -> [a]
setList i a as = zipWith f [1..] as
  where f j a' = if i == j then a else a'

tryRules :: Rules.RuleBase -> Lang.TExpr -> [(Lang.TRule, Lang.TExpr)]
tryRules rulebase = (fmap . fmap) (OptLet.optLets (OptLet.mkEmptySubst []))
                    . Rules.tryRulesMany rulebase

separate :: (Lang.TExpr -> e)
         -> Lang.TExpr
         -> [Document (Lang.TExpr, Lang.TExpr -> e)]
separate k = \case
     c@(Lang.Call ff@(Lang.TFun _ f) e) ->
       [Left' "("]
       <> [Branch $ [Right' fAndSeparated, Left' " "] <> rewrites_]
       <> [Left' ")"]
       where fAndSeparated = (nameOfFun f, (c, k))
             nameOfFun = Lang.renderSexp . Lang.pprFunId . Lang.funIdOfFun
             k' = k . Lang.Call ff
             rewrites_ = case e of
                Lang.Tuple es -> separateTuple k' es
                _ -> separate k' e

     Lang.Tuple es -> [Left' "(tuple "]
                      <> separateTuple k es
                      <> [Left' ")"]
     Lang.Var v -> [Left' (Lang.nameOfVar (Lang.tVarVar v))]
     e@(Lang.Konst c) -> case c of
       Lang.KFloat f -> [Branch [Right' (show f, (e, k))]]
       Lang.KBool b -> [Left' (show b)]
       Lang.KString s -> [Left' (show s)]
       Lang.KInteger i -> [Left' (show i)]
     e@(Lang.Let v rhs body) ->
       let rhs'  = separate  (\rhs'' -> k (Lang.Let v rhs'' body)) rhs
           body' = separate  (\body'' -> k (Lang.Let v rhs body'')) body
       in [Left' "("]
          <> [Branch $
              [Right' ("let", (e, k))] <> [Left' (" (" ++ show v ++ " ")]
              <> rhs' <> [Left' ") "]
              <> body']
          <> [Left' ")"]

     Lang.App{}    -> unsupported "App"
     Lang.Lam{}    -> unsupported "Lam"
     Lang.If{}     -> unsupported "If"
     Lang.Assert{} -> unsupported "Assert"
     Lang.Dummy{}  -> unsupported "Dummy"
     where unsupported s = error ("We don't do " ++ s)

-- For avoiding "(tuple ...)" around multiple arguments
separateTuple :: (Lang.TExpr -> e)
              -> [Lang.TExpr]
              -> [Document (Lang.TExpr, Lang.TExpr -> e)]
separateTuple k es =
  intercalate [Left' " "] (map (\(j, e) ->
    separate (\e' ->
        k (Lang.Tuple (setList j e' es)) ) e)
    (zip [1..] es))

documentOfExpr :: Lang.TExpr -> [Document a]
documentOfExpr = map removeLinks . separate id

rewrites :: Rules.RuleBase
         -> Lang.TExpr
         -> [Document [(Lang.TRule, Lang.TExpr)]]
rewrites rulebase e = (map . fmap) f (separate id e)
  where f :: (Lang.TExpr, Lang.TExpr -> b) -> [(Lang.TRule, b)]
        f (ee, k) = call_rewrites
          where call_rewrites =
                   map (\(rule, rewritten) -> (rule, k rewritten))
                       (tryRules rulebase ee)

chooseLocationPageOfModel :: Rules.RuleBase
                          -> ChooseLocationModel
                          -> ChooseLocationPage [(Lang.TRule, Lang.TExpr)]
chooseLocationPageOfModel r (es_rs, e) =
  ChooseLocationPage {
    clpHistory = map (\(e', r') ->
      HistoryEntry {
          heSExp   = documentOfExpr e'
        , heCstyle = Lang.pps e'
        , heCost   = Ksc.Cost.cost e'
        , heRule   = r'
      }) es_rs
  , clpCStyle  = Lang.pps e
  , clpCost    = Ksc.Cost.cost e
  , clpThisExp = rewrites r e
  }

overheSExp :: ([Document a] -> [Document b])
           -> HistoryEntry a
           -> HistoryEntry b
overheSExp f (HistoryEntry a b c d) = HistoryEntry (f a) b c d

chooseLocationPage :: Rules.RuleBase
                   -> ChooseLocationModel
                   -> ChooseLocationPage ChooseRewriteModel
chooseLocationPage r (es, e) =
  mapLocationDocument g (chooseLocationPageOfModel r (es, e))
  where g = (fmap . fmap) ((,) (es, e))

chooseRewritePage :: Rules.RuleBase
                  -> ChooseRewriteModel
                  -> ChooseRewritePage
                       (Either ChooseLocationModel ChooseRewriteModel)
chooseRewritePage r ((es, e), rs) =
           ChooseRewritePage
           (mapLocationDocument g (chooseLocationPageOfModel r (es, e)))
           (fmap f rs)
  where g = (fmap . fmap) (\x -> Right ((es, e), x))

        f :: (Lang.TRule, texpr)
          -> (String, String, Either ([(Lang.TExpr, Lang.TRule)], texpr) void)
        f (r', e') = (Lang.ru_name r',
                      ": " ++ renderRule r',
                      Left (es ++ [(e, r')], e'))

renderRule :: Lang.TRule -> String
renderRule rule =
  "<tt>"
  ++ Lang.renderSexp (Lang.ppr (Lang.ru_lhs rule))
  ++ " &rarr; "
  ++ Lang.renderSexp (Lang.ppr (Lang.ru_rhs rule))
  ++ "</tt>"

chooseLocationPages :: Rules.RuleBase
                    -> ChooseLocationModel
                    -> Free Page a
chooseLocationPages r e = do
    x <- liftF (ChooseLocation (chooseLocationPage r e))
    chooseRewritePages r x

chooseRewritePages :: Rules.RuleBase
                   -> ChooseRewriteModel
                   -> Free Page a
chooseRewritePages r m = do
    x <- liftF (ChooseRewrite (chooseRewritePage r m))
    case x of
      Left rw -> chooseLocationPages r rw
      Right c -> chooseRewritePages r c

newLink :: a -> State (Data.Map.Map Int a) Int
newLink a = do
  mm <- Control.Monad.Trans.State.get
  let i = case Data.Map.lookupMax mm of
        Just (theMax, _) -> theMax + 1
        Nothing -> 0
  put (Data.Map.insert i a mm)
  pure i

render :: (container_int -> string)
       -> ((a -> State (Data.Map.Map Int a) Int)
          -> container_a -> State map_int_a container_int)
       -> map_int_a
       -> container_a
       -> (map_int_a, string)
render renderString traverse' m d = (m', renderString d')
  where (d', m') = flip runState m $ flip traverse' d newLink

renderDocument :: Data.Map.Map Int a
               -> Document a
               -> (Data.Map.Map Int a, String)
renderDocument = render renderDocumentString traverseDocument

traverseDocument :: Applicative f
                 => (a -> f b)
                 -> Document a
                 -> f (Document b)
traverseDocument f = \case
  Left' s       -> pure (Left' s)
  Right' (s, a) -> (\a' -> Right' (s, a')) <$> f a
  Branch ds     -> Branch <$> (traverse . traverseDocument) f ds

spanColor :: String -> String
spanColor s = "<span onMouseOver=\"window.event.stopPropagation(); this.style.backgroundColor='#ffdddd'\" "
              ++ "onMouseOut=\"window.event.stopPropagation(); this.style.backgroundColor='transparent'\">"
              ++ s
              ++ "</span>"

renderDocumentString :: Document Int -> String
renderDocumentString = \case
  Left' s       -> s
  Right' (s, b) -> renderLink b s
  Branch ds     -> spanColor (foldr f "" ds)
    where f d rest = renderDocumentString d ++ rest

renderDocumentsString :: [Document Int] -> String
renderDocumentsString ds = "<tt>" ++ concatMap renderDocumentString ds ++ "</tt>"

renderLink :: Show a => a -> String -> String
renderLink i s = "<a href=\"/rewrite/" ++ show i ++ "#target\">" ++ s ++ "</a>"

renderPage :: Data.Map.Map Int a
           -> Page a
           -> (Data.Map.Map Int a, String)
renderPage = render renderPageString traversePage

traverseChooseLocationPage :: Applicative f
                           => (a -> f b)
                           -> ChooseLocationPage a
                           -> f (ChooseLocationPage b)
traverseChooseLocationPage f = \case
  ChooseLocationPage ds s cost d ->
    ChooseLocationPage ds s cost <$> (traverse . traverseDocument) f d

traverseChooseRewritePage :: Applicative f
                          => (a -> f b)
                          -> ChooseRewritePage a
                          -> f (ChooseRewritePage b)
traverseChooseRewritePage f = \case
  ChooseRewritePage clp r ->
    ChooseRewritePage <$> traverseChooseLocationPage f clp
                      <*> (traverse . traverse3of3) f r

traversePage :: Applicative f
             => (a -> f b) -> Page a -> f (Page b)
traversePage f = \case
  ChooseLocation d -> ChooseLocation <$> traverseChooseLocationPage f d
  ChooseRewrite r -> ChooseRewrite <$> traverseChooseRewritePage f r

traverse3of3 :: Functor f => (c -> f c') -> (a, b, c) -> f (a, b, c')
traverse3of3 f (a, b, c) = (\c' -> (a, b, c')) <$> f c

renderChooseLocationPageString :: ChooseLocationPage Int -> String
renderChooseLocationPageString (ChooseLocationPage ds s cost d) =
    concatMap (\(HistoryEntry d' cstyle c r) ->
                 renderDocumentsString d'
                 ++ "<pre>" ++ cstyle ++ "</pre>"
                 ++ renderCost c
                 ++ "<p>then applied: " ++ renderRule r ++ "</p>")
              asInt
    ++ "<a name=\"target\"></a>"
    ++ renderDocumentsString d
    ++ "<pre>" ++ s ++ "</pre>"
    ++ renderCost cost
    where asInt = (map . overheSExp . map . fmap) absurd ds

renderPageString :: Page Int -> String
renderPageString = \case
  ChooseLocation clp -> renderChooseLocationPageString clp
  ChooseRewrite (ChooseRewritePage clp r) ->
    renderChooseLocationPageString clp
    ++ "<ul>"
    ++ renderRewrites (NEL.nonEmpty r)
    ++ "</ul>"
    where renderRewrites = \case
            Nothing -> "<p>No rewrites available for selected expression</p>"
            Just l -> concatMap f l
              where f (s, s1, b) = "<li>" ++ renderLink b s ++ s1 ++ "</li>"

renderCost :: Either String Float -> String
renderCost s = "<p>" ++ costString s ++ "</p>"
  where costString = \case
          Left err -> "Error calculating cost: " ++ err
          Right c  -> "Cost: " ++ show c

renderPages :: Data.Map.Map Int (Free Page Void)
            -> Free Page Void
            -> (Data.Map.Map Int (Free Page Void), String)
renderPages m = \case
  Pure void -> absurd void
  Free page -> renderPage m page
