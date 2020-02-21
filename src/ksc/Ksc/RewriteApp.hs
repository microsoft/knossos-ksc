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
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.String (fromString)
import qualified Data.Text.Lazy

import           Web.Scotty (scotty, get, liftAndCatchIO, html, param)

import           Control.Monad.Free
import qualified Control.Monad.Trans.State
import           Control.Monad.Trans.State hiding (get)
import           Data.Void

main :: IO ()
main = do

  m <- newIORef Data.Map.empty

  let sourceFile = "test/ksc/ex0.ks"
      functionName = "f"

  decls <- fmap concat (mapM Parse.parseF [ "src/runtime/prelude.ks"
                                          , sourceFile ])

  (_, tc_decls) <- KMonad.runKM (Annotate.annotDecls LangUtils.emptyGblST decls)

  let rules = Rules.mkRuleBase $ flip mapMaybe tc_decls (\case
       Lang.RuleDecl r -> Just r
       Lang.DefDecl{} -> Nothing)

  let prog = head $ flip mapMaybe tc_decls $ \case
       Lang.RuleDecl{} -> Nothing
       Lang.DefDecl d -> case Lang.funIdOfFun (Lang.def_fun d) of
          Lang.UserFun f -> if f == functionName
                            then case Lang.def_rhs d of
                                   Lang.UserRhs e -> Just e
                                   _ -> Nothing
                            else Nothing
          _ -> Nothing

  showDebugging rules prog

  let link = "<p><a href=\"/\">Start again</a></p>"

      comments = [ link
                 , "<p>Comments</p>"
                 , "<ul>"
                 , "<li>Displays the body of the function "
                 , fromString functionName
                 , " from the source file "
                 , fromString sourceFile
                 , "</li>"
                 , "<li>Allows clicking a function even if there are zero rewrites</li>"
                 , "<li>The expression is not formatted at all</li>"
                 , "<li>The expression under rewrite should be highlighted</li>"
                 , "</ul>"
                 ]

  scotty 3000 $ do
    get "/" $ do
      m' <- liftAndCatchIO (readIORef m)

      let (m'', s) = renderPages m' (rewritesPages rules prog)
      liftAndCatchIO (writeIORef m m'')
      html $ mconcat (comments ++ [Data.Text.Lazy.pack s])
    get "/:word" $ do
      beam <- param "word"
      let i = read (Data.Text.Lazy.unpack beam) :: Int

      m' <- liftAndCatchIO (readIORef m)

      case Data.Map.lookup i m' of
            Nothing -> html $ mconcat (comments ++ ["<h1>Couldn't find ", beam, "</h1>"])
            Just e -> do
              let (m'', s) = renderPages m' e
              liftAndCatchIO (writeIORef m m'')
              html $ mconcat (comments ++ [Data.Text.Lazy.pack s])

  where showDebugging rules prog = do
          putStrLn "Rules"
          print rules

          putStrLn "Prog"
          print prog

          putStrLn "Match rules"
          print (Rules.tryRules rules prog)

data Link = Link String

type Chunk1 a = Either String (String, a)

type Document a = [Chunk1 a]

data Page a = Document (Document a)
            | Rewrites (Document a) [(String, a)]
  deriving Functor

setList :: Int -> a -> [a] -> [a]
setList i a as = zipWith f [1..] as
  where f j a' = if i == j then a else a'

tryRules :: Rules.RuleBase -> Lang.TExpr -> [(Lang.TRule, Lang.TExpr)]
tryRules rulebase = (fmap . fmap) (OptLet.optLets (OptLet.mkEmptySubst []))
                    . Rules.tryRulesMany rulebase

rewrites :: Rules.RuleBase
         -> (Lang.TExpr -> e)
         -> Lang.TExpr
         -> Document [(Lang.TRule, e)]
rewrites rulebase k = \case
     c@(Lang.Call ff@(Lang.TFun _ f) e) ->
       [Left "("]
       <> let fstr = Lang.renderSexp (Lang.pprFunId (Lang.funIdOfFun f))
              k' = k . Lang.Call ff
              rewrites_ = case e of
                Lang.Tuple es -> tupleRewrites rulebase k' es
                _ -> rewrites rulebase k' e
          in [Right (fstr,
                     map (\(rule, rewritten)
                           -> (rule, k rewritten)) (tryRules rulebase c))]
          <> [Left " "] <> rewrites_
       <> [Left ")"]
     Lang.Tuple es -> [Left "(tuple "]
                      <> tupleRewrites rulebase k es
                      <> [Left ")"]
     Lang.Var v -> [Left (Lang.nameOfVar (Lang.tVarVar v))]
     Lang.Konst c -> case c of
       Lang.KFloat f -> [Left (show f)]
       Lang.KBool b -> [Left (show b)]
       Lang.KString s -> [Left (show s)]
       Lang.KInteger i -> [Left (show i)]
     Lang.Let v rhs body ->
       let rhs'  = rewrites rulebase (\rhs'' -> k (Lang.Let v rhs'' body)) rhs
           body' = rewrites rulebase (\body'' -> k (Lang.Let v rhs body'')) body
       in [Left ("(let (" ++ show v ++ " ")] <> rhs' <> [Left ") "] <> body'

     Lang.App _ _ -> error "We don't do App"
     Lang.Lam _ _ -> error "We don't do Lam"
     Lang.If _ _ _ -> error "We don't do If"
     Lang.Assert _ _ -> error "We don't do Assert"
     Lang.Dummy _ -> error "We don't do Dummy"

-- For avoiding "(tuple ...)" around multiple arguments
tupleRewrites :: Rules.RuleBase
              -> (Lang.TExpr -> e)
              -> [Lang.TExpr]
              -> Document [(Lang.TRule, e)]
tupleRewrites rulebase k es =
  intercalate [Left " "] (map (\(j, e) ->
    rewrites rulebase (\e' ->
        k (Lang.Tuple (setList j e' es)) ) e)
    (zip [1..] es))

rewritesPage :: Rules.RuleBase
             -> Lang.TExpr
             -> Page [(Lang.TRule, Lang.TExpr)]
rewritesPage r e = Document (rewrites r id e)

choosePage :: Rules.RuleBase
           -> Lang.TExpr
           -> [(Lang.TRule, Lang.TExpr)]
           -> Page (Either Lang.TExpr [(Lang.TRule, Lang.TExpr)])
choosePage r e rs =
  Rewrites ((fmap . fmap . fmap) Right (rewrites r id e))
           (fmap (\(r', e') -> (Lang.ru_name r', Left e')) rs)

rewritesPages :: Rules.RuleBase -> Lang.TExpr -> Free Page a
rewritesPages r e = do
    x <- liftF (rewritesPage r e)
    choosePages r e x

choosePages :: Rules.RuleBase
            -> Lang.TExpr
            -> [(Lang.TRule, Lang.TExpr)]
            -> Free Page a
choosePages r e rs = do
    x <- liftF (choosePage r e rs)
    case x of
      Left e' -> rewritesPages r e'
      Right rs' -> choosePages r e rs'

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
traverseDocument = traverse . traverse . traverse

renderDocumentString :: Document Int -> String
renderDocumentString = foldr f ""
  where f = \case
          Left s -> (s ++)
          Right (s, b) -> (renderLink b s ++)

renderLink :: Show a => a -> String -> String
renderLink i s = "<a href=\"" ++ show i ++ "\">" ++ s ++ "</a>"

renderPage :: Data.Map.Map Int a
           -> Page a
           -> (Data.Map.Map Int a, String)
renderPage = render renderPageString traversePage

traversePage :: Applicative f
             => (a -> f b) -> Page a -> f (Page b)
traversePage f = \case
  Document d   -> Document <$> traverseDocument f d
  Rewrites d r -> Rewrites <$> traverseDocument f d
                           <*> (traverse . traverse) f r

renderPageString :: Page Int -> String
renderPageString = \case
  Document d -> renderDocumentString d
  Rewrites d r -> renderDocumentString d ++ renderRewrites r
    where renderRewrites = foldr f ""
          f (s, b) rrs = "<p>" ++ renderLink b s ++ "</p>" ++ rrs

renderPages :: Data.Map.Map Int (Free Page Void)
            -> Free Page Void
            -> (Data.Map.Map Int (Free Page Void), String)
renderPages m = \case
  Pure void -> absurd void
  Free page -> renderPage m page
