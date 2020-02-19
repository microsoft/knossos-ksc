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
import qualified Data.Text.Lazy

import           Web.Scotty (scotty, get, liftAndCatchIO, html, param)

main :: IO ()
main = do

  m <- newIORef Data.Map.empty

  decls <- fmap concat (mapM Parse.parseF [ "src/runtime/prelude.ks"
                                          , "test/ksc/ex0.ks"])

  (_, tc_decls) <- KMonad.runKM (Annotate.annotDecls LangUtils.emptyGblST decls)

  let rules = Rules.mkRuleBase $ flip mapMaybe tc_decls (\case
       Lang.RuleDecl r -> Just r
       Lang.DefDecl{} -> Nothing)

  let prog = head $ flip mapMaybe tc_decls $ \case
       Lang.RuleDecl{} -> Nothing
       Lang.DefDecl d -> case Lang.funIdOfFun (Lang.def_fun d) of
          Lang.UserFun "f" -> case Lang.def_rhs d of
            Lang.UserRhs e -> Just e
            _ -> Nothing
          _ -> Nothing

  showDebugging rules prog

  let link = "<p><a href=\"/\">Start again</a></p>"

  scotty 3000 $ do
    get "/" $ do
      let (m'', s) = render 0 (rewrites rules id prog)
      liftAndCatchIO (writeIORef m m'')
      html $ mconcat [link, Data.Text.Lazy.pack s]
    get "/:word" $ do
      beam <- param "word"
      let i = read (Data.Text.Lazy.unpack beam) :: Int

      m' <- liftAndCatchIO (readIORef m)

      case Data.Map.lookup i m' of
            Nothing -> html $ mconcat [link, "<h1>Couldn't find ", beam, "</h1>"]
            Just e -> do
              let e' = OptLet.optLets (OptLet.mkEmptySubst []) e
              let (m'', s) = render 0 (rewrites rules id e')
              liftAndCatchIO (writeIORef m m'')
              html $ mconcat [link, Data.Text.Lazy.pack s]

  where showDebugging rules prog = do
          putStrLn "Rules"
          print rules

          putStrLn "Prog"
          print prog

          putStrLn "Match rules"
          print (Rules.tryRules rules prog)

data Link = Link String

type Chunk = Either String (String, Lang.TExpr)

type Document = [Chunk]

setList :: Int -> a -> [a] -> [a]
setList i a as = zipWith f [1..] as
  where f j a' = if i == j then a else a'

rewrites :: Rules.RuleBase
         -> (Lang.TExpr -> Lang.TExpr)
         -> Lang.TExpr
         -> Document
rewrites rulebase k = \case
     c@(Lang.Call ff@(Lang.TFun _ f) e) ->
       [Left "("]
       <> let fstr = Lang.renderSexp (Lang.pprFunId (Lang.funIdOfFun f))
              k' = k . Lang.Call ff
              rewrites_ = case e of
                Lang.Tuple es -> tupleRewrites rulebase k' es
                _ -> rewrites rulebase k' e
          in (case Rules.tryRules rulebase c of
               Just rewritten -> [Right (fstr, k rewritten)]
                                 <> [Left " "]
                                 <> rewrites_
               Nothing -> [Left (fstr ++ " ")] <> rewrites_)
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

tupleRewrites :: Rules.RuleBase
              -> (Lang.TExpr -> Lang.TExpr)
              -> [Lang.TExpr]
              -> Document
tupleRewrites rulebase k es =
  intercalate [Left " "] (map (\(j, e) ->
    rewrites rulebase (\e' ->
        k (Lang.Tuple (setList j e' es)) ) e)
    (zip [1..] es))

render :: Int -> Document -> (Data.Map.Map Int Lang.TExpr, String)
render i = \case
  [] -> (Data.Map.empty, "")
  Left s:rest -> fmap (s ++) (render i rest)
  Right (s, e):rest ->
    let (m, rests) = render (i + 1) rest
    in (Data.Map.insert i e m,
        "<a href=\"" ++ show i ++ "\">"
        ++ s
        ++ "</a>"
        ++ rests)
