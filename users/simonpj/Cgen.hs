{-# LANGUAGE LambdaCase #-}

module Cgen where

import           Debug.Trace                    ( trace
                                                , traceM
                                                , traceShowId
                                                )

import qualified Data.Map                      as Map
import           Data.List                      ( intercalate
                                                , reverse
                                                )
import           Data.Maybe                     ( fromJust )
import           Control.Monad                  ( (<=<) )
import qualified Control.Monad.State           as S
import           System.Process                 ( callCommand )
import           Text.Regex                     ( subRegex
                                                , mkRegex
                                                )
import           Parse                          ( runParser
                                                , pDefs
                                                )
import qualified Lang                          as L

type M = S.State Int

spc :: String -> String -> String
spc x y = x ++ " " ++ y

dbtrace msg e = e

runM :: M a -> a
runM = flip S.evalState 0

freshCVar :: M String
freshCVar = do
  s <- S.get
  S.put (s + 1)
  return ("c$" ++ show s)

freshVar :: M L.Var
freshVar = do
  -- This doesn't communicate very well but is quick (and dirty)
  s <- S.get
  S.put (s + 1)
  return $ L.Simple $ "v$" ++ show s

------------ Administrative normal form

anf :: L.Expr -> M L.Expr
anf (L.Expr tye ex) = case ex of
  L.Call f (L.Expr ty (L.Tuple ts)) -> do
    anfArgs <- mapM letAnf ts
    return $ foldr
      (.)
      id
      (map fst anfArgs)
      (L.Expr tye (L.Call f (L.Expr ty (L.Tuple (map snd anfArgs)))))

  L.Call f e -> do
    (lete, ve) <- letAnf e
    return $ lete $ L.Expr tye $ L.Call f ve
  L.Tuple ts -> do
    anfArgs <- mapM letAnf ts
    return $ foldr (.) id (map fst anfArgs) $ L.Expr tye $ L.Tuple
      (map snd anfArgs)
  L.Lam x e -> do
    anfe <- anf e
    return $ L.Expr tye $ L.Lam x anfe
  L.App f x -> do
    (letf, vf) <- letAnf f
    (letx, vx) <- letAnf x
    return $ letf $ letx $ L.Expr tye $ L.App vf vx
  L.Let x e body -> do
    anfe    <- anf e
    anfbody <- anf body
    return $ L.Expr tye $ L.Let x anfe anfbody
  L.If cond ift iff -> do
    anfcond <- anf cond
    anfift  <- anf ift
    anfiff  <- anf iff
    return $ L.Expr tye $ L.If anfcond anfift anfiff
  L.Assert cond body -> do
    anfcond <- anf cond
    anfbody <- anf body
    return $ L.Expr tye $ L.Assert anfcond anfbody
  _ -> return $ L.Expr tye ex

-- letAnf of (+ a b) is (\body -> let c17 = (+ a b) in body, c17)
letAnf :: L.Expr -> M (L.Expr -> L.Expr, L.Expr)
letAnf e@(L.Expr tye ex) = do
  ve   <- freshVar
  anfe <- anf e
  return
    ( \body@(L.Expr tybody bodyx) ->
      L.Expr tybody (L.Let (L.TVar tye ve) anfe body)
    , L.Expr (L.typeOf anfe) (L.Var ve)
    )

anfDefs :: [L.Def] -> [L.Def]
anfDefs defs = map anfDef defs

anfDef :: L.Def -> L.Def
anfDef (L.Def (L.TFun ty f) vars expr) =
  L.Def (L.TFun ty f) vars $ runM $ anf expr

-------------------- Cgen

-- CGenResult is (C declaration, C expression, L.Type)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r")
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0")      -- this is what we use at the occurrence
type CGenResult = (String, String)

cgenDefs :: [L.Def] -> [String]
cgenDefs defs = map cgenDef defs

cgenDef :: L.Def -> String
cgenDef def = fst $ cgenDefE def

cgenDefE :: L.Def -> CGenResult
cgenDefE (L.Def f@(L.TFun tyf _) vars expr) =
  let _ = trace ("Def " ++ show f ++ "\n") ()
  in  let (cDecl, cVar) = runM $ cgenExpr expr
      in  ( cgenType tyf
          `spc` cgenFun f
          ++    "("
          ++    intercalate
                  ", "
                  (map (\(L.TVar ty var) -> cgenType ty `spc` cgenVar var) vars)
          ++    ") {\n"
          ++    cDecl
          ++    "return "
          ++    cVar
          ++    ";\n}\n"
          , cgenFun f
          )

cgenExpr :: L.Expr -> M CGenResult
cgenExpr = cgenExprR <=< anf

-- The input expression must be in ANF
cgenExprR :: L.Expr -> M CGenResult
cgenExprR (L.Expr ty ex) = case ex of
  L.Konst k               -> return ("", cgenKonst k)

  L.Var   v               -> return ("", cgenVar v)

  L.Call f (L.Expr tyx x) -> case x of
    L.Tuple vs -> do
      v         <- freshCVar
      cgresults <- mapM cgenExprR vs
      let decls = map fst cgresults
      let exprs = map snd cgresults

      return
        ( "/****/"
        ++ intercalate ";\n" decls
        ++ cgenType ty
        ++ " "
        ++ v
        ++ " = "
        ++ cgenFun f
        ++ "("
        ++ intercalate "," exprs
        ++ ");\n"
        , v
        )
    L.Var v -> do
      vf <- freshCVar

      return
        ( cgenType ty
        `spc` vf
        ++    " = "
        ++    cgenFun f
        ++    "("
        ++    cgenVar v
        ++    ");\n"
        , vf
        )

    _ ->
      error
        $  "Function arguments should be Var in ANF, not"
        ++ show x
        ++ " in call to "
        ++ show f

  L.Let (L.TVar tyv v) e1 body -> do
    (decle1  , ve1  ) <- cgenExprR e1
    (declbody, vbody) <- cgenExprR body
    return
      ( decle1
      ++ cgenType tyv
      ++ " "
      ++ cgenVar v
      ++ " = "
      ++ ve1
      ++ ";\n"
      ++ declbody
      , vbody
      )

  L.Tuple [t] -> cgenExpr t
  L.Tuple ts  -> do
    cT <- freshCVar

    let unVar :: L.ExprX -> L.Var
        unVar = \case
          L.Var v -> v
          _       -> error "Tuple: Expected arguments to be Vars"

        vars = map (unVar . L.bodyOf) ts

    return ("", "std::make_tuple(" ++ intercalate "," (map cgenVar vars) ++ ")")

  L.Lam (L.TVar tv v) body -> do
    l        <- freshCVar
    (cE, vE) <- cgenExprR body
    return
      ( cgenType ty
      `spc` l
      ++    " = [&]("
      ++    cgenType tv
      `spc` cgenVar v
      ++    ") { "
      ++    cE
      ++    "   return "
      ++    vE
      ++    "; };\n"
      , l
      )

  L.If c texpr fexpr -> do
    cret        <- freshCVar

    (declc, vc) <- cgenExprR c
    (declt, vt) <- cgenExprR texpr
    (declf, vf) <- cgenExprR fexpr

    return
      ( declc -- emit condition generation
      ++    cgenType ty
      `spc` cret
      ++    ";\n" -- emit decl for "return" type
      ++    "if ("
      ++    vc
      ++    ") {"
      ++    "  "
      ++    declt
      ++    ";\n" -- compute true value
      ++    "  "
      ++    cret
      ++    "="
      ++    vt
      ++    ";\n" -- assign to "return"
      ++    "} else {\n" -- else
      ++    "  "
      ++    declf
      ++    ";\n" -- compute false value
      ++    "  "
      ++    cret
      ++    "="
      ++    vf
      ++    ";\n" -- assign to "return"
      ++    "}\n" -- phew
      , cret
      )

  L.Assert cond body -> do
    (declcond, vcond) <- cgenExprR cond
    (declbody, vbody) <- cgenExprR body
    return (declcond `spc` "ASSERT(" ++ vcond ++ ");\n" ++ declbody, vbody)
  L.App{} -> error "App"

cgenFunId :: L.FunId -> String
cgenFunId = \case
  L.SFun fun   -> translateFun fun
  L.SelFun i n -> "selfun<" ++ show i ++ "," ++ show n ++ ">"

cgenFun :: L.TFun -> String
cgenFun (L.TFun ty f) = case f of
  L.Fun funId       -> cgenFunId funId
  L.GradFun s L.Fwd -> "D$" ++ cgenFunId s
  L.GradFun s L.Rev -> "R$" ++ cgenFunId s
  L.DrvFun  s L.Fwd -> "fwd$" ++ cgenFunId s
  L.DrvFun  s L.Rev -> "rev$" ++ cgenFunId s
  L.LMFun s         -> case ty of
    L.TypeLM t1 t2 ->
      "LM::" ++ s ++ "<" ++ cgenType t1 ++ "," ++ cgenType t2 ++ ">"
    L.TypeUnknown -> "auto"
  _ -> error $ "Bad fun " ++ show f


cgenType :: L.Type -> String
cgenType = \case
  L.TypeZero      -> "zero_t"
  L.TypeFloat     -> "double"
  L.TypeInteger   -> "int"
  L.TypeTuple [t] -> cgenType t
  L.TypeTuple ts  -> "tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  L.TypeVec   t   -> "vec<" ++ cgenType t ++ ">"
  L.TypeBool      -> "bool"
  L.TypeUnknown   -> "auto"
  L.TypeLambda from to ->
    "std::function<" ++ cgenType to ++ "(" ++ cgenType from ++ ")>"
  L.TypeLM from to -> "LM::lm<" ++ cgenType from ++ "," ++ cgenType to ++ ">"
  x                -> error $ "Bad cgenType" ++ show x

cgenKonst :: L.Konst -> String
cgenKonst = \case
  L.KZero      -> "0"
  L.KInteger i -> show i
  L.KFloat   f -> show f
  L.KBool    b -> if b then "TRUE" else "FALSE"

cgenVar :: L.Var -> String
cgenVar = \case
  L.Simple s -> "s_" ++ s
  L.Delta  d -> "d_" ++ d
  L.Grad g m ->
    "g_"
      ++ g
      ++ "_"
      ++ (case m of
           L.Fwd -> "f"
           L.Rev -> "r"
         )

translateFun = \case
  "*"  -> "mul"
  "+"  -> "add"
  "/"  -> "div"
  "-"  -> "sub"
  "==" -> "eq"
  "<"  -> "lt"
  s    -> s

cppF :: String -> [L.Def] -> IO ()
-- String is the file name
cppF outfile defs = do
  let lines =
        ["#include <stdio.h>", "#include \"knossos.h\"", "namespace ks {\n"]

  let lls     = cgenDefs defs

  let tail = ["}", "int main() {", "  ks::main();", "  return 0;", "}"]
  let cppfile = outfile ++ ".cpp"
  let exefile = outfile ++ ".exe"
  putStrLn $ "Writing to " ++ cppfile
  writeFile cppfile (intercalate "\n" (lines ++ lls ++ tail))
  callCommand $ "clang-format -i " ++ cppfile
  let compcmd = "g++ -fmax-errors=5 -I. -O -g " ++ cppfile ++ " -o " ++ exefile
  putStrLn $ "Compiling: " ++ compcmd
  callCommand $ compcmd
  putStrLn "Running"
  callCommand exefile
  putStrLn "Done"
