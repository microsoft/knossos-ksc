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
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wwarn #-}
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

import qualified Web.Scotty
import           Web.Scotty (scotty, get, liftAndCatchIO, html, param, post)
import qualified Network.Wai.Parse
import qualified Data.ByteString.Lazy.Char8

import           Control.Monad.Free (Free(Pure, Free), liftF)
import qualified Control.Monad.Trans.State
import           Control.Monad.Trans.State hiding (get)
import           Data.Void (Void, absurd)
import qualified Data.List.NonEmpty as NEL

import qualified Ksc.Cost (cost)

typeCheck :: [Lang.Decl] -> IO [Lang.TDecl]
typeCheck = fmap snd . KMonad.runKM . Annotate.annotDecls LangUtils.emptyGblST

parseS :: [String] -> [Lang.Decl]
parseS = concat . map Parse.parseS

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
  sourceFileContent <- readFile sourceFile
  readProgramS sourceFileContent functionName

readProgramS :: String -> String -> IO (Lang.TExpr, Rules.RuleBase)
readProgramS sourceFileContent functionName = do
  prelude <- readFile "src/runtime/prelude.ks"
  tc_decls <- typeCheck (parseS [prelude, sourceFileContent])

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

  let link = "<p><a href=\"/\">Start again</a> "
             <> " or <a href=\"/upload.html\">upload a file</a></p>"

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
    get "/upload.html" $ do
      html $ mconcat [ "<p>The body of the function called \""
                     , fromString functionName
                     , "\" in your file will be used as the test expression</p>"
                     , "<form action=\"/do-upload\" "
                     , "enctype=\"multipart/form-data\" method=\"POST\">"
                     , "<input type=\"file\" name=\"file\">"
                     , "<input type=\"submit\">"
                     , "</form>" ]
    post "/do-upload" $ do
      uploadedFileContentM <- readUploadedFile "file"
      let uploadedFileContent = case uploadedFileContentM of
            Just s  -> s
            Nothing -> error "Couldn't find uploaded file"

      (prog, rules) <- liftAndCatchIO $ readProgramS uploadedFileContent functionName
      s <- withMap (\m -> renderPages m (chooseLocationPages rules ([], prog)))

      html $ mconcat (comments ++ [Data.Text.Lazy.pack s])

readUploadedFile :: Data.Text.Lazy.Text
                 -> Web.Scotty.ActionM (Maybe String)
readUploadedFile fieldname = do
  uploadedFiles <- Web.Scotty.files
  return $ fmap (Data.ByteString.Lazy.Char8.unpack
                 . Network.Wai.Parse.fileContent)
                $ lookup fieldname uploadedFiles

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

data Wrapped tExpr e =
  Wrapped (Lang.TExpr, Lang.TExpr -> tExpr,
           [Document (Wrapped tExpr e)],
           [Document (Wrapped tExpr e)] -> [Document (Wrapped tExpr e)])

separateWrapped :: ([Document (Wrapped tExpr e)] -> [Document (Wrapped tExpr e)])
                -> (Lang.TExpr -> tExpr)
                -> Lang.TExpr
                -> [Document (Wrapped tExpr e)]
separateWrapped k ke ee = case ee of
     Lang.Call ff@(Lang.TFun _ f) e -> bar
       where k' :: [Document (Wrapped _ _)] -> _
             k' = k . foo
             ke' = ke . Lang.Call ff

             bar = foo (separateWrapped k' ke' e)

             foo rewrites_' = [Left' "("]
               <> [Branch $ [link bar ke (nameOfFun f), Left' " "] <> rewrites_']
               <> [Left' ")"]
     Lang.Tuple es -> bar
       where foo rewrites_' = sexpr bar ke "tuple" [rewrites_']
             bar = foo (separateWrappedTuple k ke es)
     Lang.Var v -> [Left' (Lang.nameOfVar (Lang.tVarVar v))]
     Lang.Konst c -> case c of
       Lang.KFloat f -> let bar = foo
                            foo = sexprnp bar ke (show f) []
                        in bar
       Lang.KBool b -> let bar = foo
                           foo = sexprnp bar ke (show b) []
                       in bar
       Lang.KString s -> [Left' (show s)]
       Lang.KInteger i -> [Left' (show i)]
     Lang.Let v rhs body ->
       let rhs'  = separateWrapped k'rhs (\rhs'' -> ke (Lang.Let v rhs'' body)) rhs
           body' = separateWrapped k'body (\body'' -> ke (Lang.Let v rhs body'')) body
           k'rhs = (\rhs'' -> k (foo rhs'' body'))
           k'body = (\body'' -> k (foo rhs' body''))

           bar = foo rhs' body'

           foo rhs'' body'' =
             sexpr bar ke "let" [parens ([Left' (show v ++ " ")] <> rhs''), body'' ]
       in bar
     Lang.If c t f ->
       let c' = separateWrapped (\c'' -> k (foo c'' t' f')) (\c'' -> ke (Lang.If c'' t f)) c
           t' = separateWrapped (\t'' -> k (foo c' t'' f')) (\t'' -> ke (Lang.If c t'' f)) t
           f' = separateWrapped (\f'' -> k (foo c' t' f'')) (\f'' -> ke (Lang.If c t f'')) f

           bar = foo c' t' f'

           foo c'' t'' f'' = sexpr bar ke "if" [c'', t'', f'']

       in bar

     Lang.App{}    -> unsupported "App"
     Lang.Lam{}    -> unsupported "Lam"
     Lang.Assert{} -> unsupported "Assert"
     Lang.Dummy{}  -> unsupported "Dummy"
     where unsupported s = error ("We don't do " ++ s)
           link dd ke' s = Right' (s, Wrapped (ee, ke', dd, k))
           parens s = [Left' "("] <> s <> [Left' ")"]
           sexpr dd ke' car cdr = parens (sexprnp dd ke' car cdr)
           sexprnp dd ke' car cdr =
             [Branch $ intercalate [Left' " "] ([link dd ke' car]:cdr)]


-- For avoiding "(tuple ...)" around multiple arguments
separateWrappedTuple :: ([Document (Wrapped tExpr e)] -> [Document (Wrapped tExpr e)])
                     -> (Lang.TExpr -> tExpr)
                     -> [Lang.TExpr]
                     -> [Document (Wrapped tExpr e)]
separateWrappedTuple k ke es = foo es'
  where es' =
          map (\(j, e) ->
                  separateWrapped (\e' ->
                                     k (foo (setList j e' es')) )
                                  (\e' ->
                                     ke (Lang.Tuple (setList j e' es)) )
                  e)
               (zip [1..] es)

        foo = intercalate [Left' " "]


nameOfFun :: Lang.Fun -> String
nameOfFun = takeWhile (/= '@') . Lang.renderSexp . Lang.pprFunId . Lang.funIdOfFun

separate :: (Lang.TExpr -> e)
         -> Lang.TExpr
         -> [Document (Lang.TExpr, Lang.TExpr -> e)]
separate k ee = case ee of
     Lang.Call ff@(Lang.TFun _ f) e ->
       sexpr (nameOfFun f) [rewrites_]
       where k' = k . Lang.Call ff
             rewrites_ = case e of
                Lang.Tuple es -> separateTuple k' es
                _ -> separate k' e

     Lang.Tuple es -> sexpr "tuple" [separateTuple k es]
     Lang.Var v -> [Left' (Lang.nameOfVar (Lang.tVarVar v))]
     Lang.Konst c -> case c of
       Lang.KFloat f -> sexprnp (show f) []
       Lang.KBool b -> sexprnp (show b) []
       Lang.KString s -> [Left' (show s)]
       Lang.KInteger i -> [Left' (show i)]
     Lang.Let v rhs body ->
       let rhs'  = separate  (\rhs'' -> k (Lang.Let v rhs'' body)) rhs
           body' = separate  (\body'' -> k (Lang.Let v rhs body'')) body
       in sexpr "let" [parens ([Left' (show v ++ " ")] <> rhs'), body' ]
     Lang.If c t f ->
       let c' = separate (\c'' -> k (Lang.If c'' t f)) c
           t' = separate (\t'' -> k (Lang.If c t'' f)) t
           f' = separate (\f'' -> k (Lang.If c t f'')) f
       in sexpr "if" [c', t', f']

     Lang.App{}    -> unsupported "App"
     Lang.Lam{}    -> unsupported "Lam"
     Lang.Assert{} -> unsupported "Assert"
     Lang.Dummy{}  -> unsupported "Dummy"
     where unsupported s = error ("We don't do " ++ s)
           link s = Right' (s, (ee, k))
           parens s = [Left' "("] <> s <> [Left' ")"]
           sexpr car cdr = parens (sexprnp car cdr)
           sexprnp car cdr = [Branch $ intercalate [Left' " "] ([link car]:cdr)]


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
         -> [Document ([Document (Wrapped Lang.TExpr e)], [(Lang.TRule, Lang.TExpr)])]
rewrites rulebase e = (map . fmap) f (separateWrapped id id e)
  where f :: Wrapped tExpr e -> ([Document (Wrapped tExpr e)], [(Lang.TRule, tExpr)])
        f (Wrapped (ee, k, dd, ddk)) = (ddk dd, call_rewrites)
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
  , clpThisExp = (map . fmap) snd (rewrites r e)
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

renderChooseLocationPageString :: ChooseLocationPage Int -> String -> String
renderChooseLocationPageString (ChooseLocationPage ds s cost d) rewriteChoices =
    "<table style=\"border-collapse: collapse\">" ++
    tr "<th>Cost</th><th>IR</th><th>Infix</th>" ++
    flip concatMap ds (\(HistoryEntry d' cstyle c r) ->
      let ir = renderDocumentsString ((map . fmap) absurd d')
          infix_ = "<pre>" ++ cstyle ++ "</pre>"
          appliedRule = "<p>then applied: " ++ renderRule r ++ "</p>"
      in concatMap (tr . concatMap td)
      [[renderCost c, ir,          infix_ ],
       ["",           appliedRule, ""     ]])
    ++ tr (td (renderCost cost)
          <> td ("<a name=\"target\"></a>" <> renderDocumentsString d
                <> rewriteChoices)
          <> td ("<pre>" ++ s ++ "</pre>"))
    ++ "</table>"
    where td s' = "<td style=\"border: 1px solid black; "
                  ++ "padding: 0.5em\">" ++ s' ++ "</td>"
          tr s' = "<tr>" ++ s' ++ "</tr>"

renderPageString :: Page Int -> String
renderPageString = \case
  ChooseLocation clp -> renderChooseLocationPageString clp ""
  ChooseRewrite (ChooseRewritePage clp r) ->
    renderChooseLocationPageString clp
      ("<ul>"
    ++ renderRewrites (NEL.nonEmpty r)
    ++ "</ul>")
    where renderRewrites = \case
            Nothing -> "<p>No rewrites available for selected expression</p>"
            Just l -> concatMap f l
              where f (s, s1, b) = "<li>" ++ renderLink b s ++ s1 ++ "</li>"

renderCost :: Either String Float -> String
renderCost s = "<p>" ++ costString s ++ "</p>"
  where costString = \case
          Left err -> "Error calculating cost: " ++ err
          Right c  -> show c

renderPages :: Data.Map.Map Int (Free Page Void)
            -> Free Page Void
            -> (Data.Map.Map Int (Free Page Void), String)
renderPages m = \case
  Pure void -> absurd void
  Free page -> renderPage m page
