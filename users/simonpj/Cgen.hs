{-# LANGUAGE LambdaCase, FlexibleInstances, TypeApplications  #-}

module Cgen where

import GHC.Stack
import           Debug.Trace                    ( trace )
import           Prelude                 hiding ( lines
                                                , tail
                                                )
import           Data.List                      ( intercalate )
import           Control.Monad                  ( (<=<) )
import qualified Control.Monad.State           as S
import           System.Process                 ( callCommand )

import qualified Lang                          as L

type M = S.State Int

spc :: String -> String -> String
spc x y = x ++ " " ++ y

runM :: M a -> a
runM = flip S.evalState 0

freshCVar :: M String
freshCVar = do
  s <- S.get
  S.put (s + 1)
  return ("c$" ++ show s)

class CfreshVar b where
  freshVar :: L.Type -> M b

instance CfreshVar L.Var where  
  freshVar _ = do
    -- This doesn't communicate very well but is quick (and dirty)
    s <- S.get
    S.put (s + 1)
    return $ L.Simple $ "v$" ++ show s

instance CfreshVar (L.TVar L.Var) where  
  freshVar ty = do
                 v <- freshVar @L.Var ty
                 return $ L.TVar ty v

------------ Administrative normal form

anf :: (L.TypeableFun f, L.Typeable b,  CfreshVar b) => L.ExprX f b -> M (L.ExprX f b)
anf = \case
  L.Call f (L.Tuple ts) -> do
    anfArgs <- mapM letAnf ts
    return $ foldr
      (.)
      id
      (map fst anfArgs)
      $ L.Call f (L.Tuple (map snd anfArgs))

  L.Call f e -> do
    (lete, ve) <- letAnf e
    return $ lete $ L.Call f ve

  L.Tuple ts -> do
    anfArgs <- mapM letAnf ts
    return $ foldr (.) id (map fst anfArgs) $
           L.Tuple (map snd anfArgs)
  
  L.Lam x ty e -> do
    anfe <- anf e
    return $ L.Lam x ty anfe
  
  L.App f x -> do
    (letf, vf) <- letAnf f
    (letx, vx) <- letAnf x
    return $ letf $ letx $ L.App vf vx
  
  L.Let x e body -> do
    anfe    <- anf e
    anfbody <- anf body
    return $ L.Let x anfe anfbody
    
  L.If cond ift iff -> do
    anfcond <- anf cond
    anfift  <- anf ift
    anfiff  <- anf iff
    return $ L.If anfcond anfift anfiff

  L.Assert cond body -> do
    anfcond <- anf cond
    anfbody <- anf body
    return $ L.Assert anfcond anfbody

  ex -> return ex

-- letAnf of (+ a b) is (\body -> let c17 = (+ a b) in body, c17)
letAnf :: (L.TypeableFun f, L.Typeable b, CfreshVar b) => L.ExprX f b -> M (L.ExprX f b -> L.ExprX f b, L.ExprX f b)
letAnf e = do
  ve   <- freshVar (L.typeof e)
  anfe <- anf e
  return
    ( \body -> L.Let ve anfe body
    , L.Var ve
    )

anfDefs :: (L.TypeableFun f, L.Typeable b, CfreshVar b) => [L.DefX f b] -> [L.DefX f b]
anfDefs = map anfDef

anfDef :: (L.TypeableFun f, L.Typeable b, CfreshVar b) => L.DefX f b -> L.DefX f b
anfDef (L.DefX f vars expr) =
  L.DefX f vars $ runM $ anf expr

-------------------- Cgen

-- CGenResult is (C declaration, C expression, L.Type)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r")
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0")      -- this is what we use at the occurrence
type CGenResult = (String, String)

cgenDefs :: [L.TDef] -> [String]
cgenDefs = map cgenDef

cgenDef :: L.TDef -> String
cgenDef def = fst $ cgenDefE def

cgenDefE :: L.TDef -> CGenResult
cgenDefE (L.DefX f@(L.TFun tyf _) vars expr) =
  let (cDecl, cVar) = runM $ cgenExpr expr
      tyfstr = cgenType tyf
  in  (     tyfstr
      `spc` cgenFun f
      ++    "("
      ++    intercalate
              ", "
              (map (\(L.TVar ty var) -> cgenType ty `spc` cgenVar var) vars)
      ++    ") {\n"
      ++    cDecl
      ++    "return static_cast<" ++ tyfstr ++ ">"  -- In order to concretize tag types like zero_t
      ++    "(" ++ cVar ++ ")"
      ++    ";\n}\n"
      , cgenFun f
      )

cgenExpr :: L.TExpr -> M CGenResult
cgenExpr = cgenExprR -- <=< anf

cgenExprR :: HasCallStack => L.TExpr -> M CGenResult
cgenExprR ex = case ex of
  L.Konst k             -> return ("", cgenKonst k)

  L.Var v    -> return ("", cgenTVar v)

  L.Call tf@(L.TFun ty f) x -> case x of
    L.Tuple vs -> do
      cgresults <- mapM cgenExprR vs
      let decls = map fst cgresults
      let exprs = map snd cgresults

      v <- freshCVar
      return
        ( "/**Call**/"
        ++ intercalate "\n" decls
        ++ "auto" --cgenType ty
        ++ " "
        ++ v
        ++ " = "
        ++ cgenFun tf
        ++ "("
        ++ intercalate "," exprs
        ++ ");\n/**eCall**/\n"
        , v
        )
    ex -> do
      vf <- freshCVar
      (cgdecl, cgexpr) <- cgenExprR ex
      
      return
        ( "/**Ex**/\n"
        ++    cgdecl
        ++    "auto"--cgenType ty 
        `spc` vf
        ++    " = "
        ++    cgenFun tf
        ++    "("
        ++    cgexpr
        ++    ");\n/**eEx*/\n"
        , vf
        )

  L.Let (L.TVar tyv v) e1 body -> do
    (decle1  , ve1  ) <- cgenExprR e1
    (declbody, vbody) <- cgenExprR body
    return
      (  "/**Let**/"
      ++ decle1
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
  L.Tuple vs  -> do
      cgresults <- mapM cgenExprR vs
      let decls = map fst cgresults
      let exprs = map snd cgresults

      return
        ( "/**Tuple**/"
        ++ intercalate "\n" decls
        ,
        "std::make_tuple("
        ++ intercalate "," exprs
        ++ ")/**eTuple**/"
        )

  L.Lam (L.TVar tv v) ty1 body -> do
    let _ = L.assertEqual "CGLam" tv ty1
    l        <- freshCVar
    (cE, vE) <- cgenExprR body
    return
      ( "/**Lam**/"
      ++ "auto" -- cgenType (L.TypeLambda tv (L.typeof body)) 
      `spc` l
      ++    " = [=]("
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
    let ty1 = L.typeof texpr
    let ty2 = L.typeof fexpr
    let ty = L.makeUnionType ty1 ty2
    let dotv = case ty of
                L.TypeLM (L.LMVariant ts) -> ".v"
                _ -> ""

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
      ++    cret ++ dotv
      ++    "="
      ++    vt
      ++    ";\n" -- assign to "return"
      ++    "} else {\n" -- else
      ++    "  "
      ++    declf
      ++    ";\n" -- compute false value
      ++    "  "
      ++    cret ++ dotv
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
  L.SelFun i n -> "selfun$" ++ show n ++ "_" ++ show i

cgenFun :: HasCallStack => L.TFun -> String
cgenFun tf@(L.TFun ty f) = case f of
  L.Fun (L.SFun "build") -> "build<"++ cgenType t ++ ">" where (L.TypeVec t) = ty
  L.Fun funId       -> cgenFunId funId
  L.GradFun s L.Fwd -> "D$" ++ cgenFunId s
  L.GradFun s L.Rev -> "R$" ++ cgenFunId s
  L.DrvFun  s L.Fwd -> "fwd$" ++ cgenFunId s
  L.DrvFun  s L.Rev -> "rev$" ++ cgenFunId s
  L.LMFun s         -> case ty of
    L.TypeLM ty -> cgenTypeLM ty ++ "::" ++ "mk"
    t -> "LM::/* " ++ show t ++ "*/" ++ s

cgenTypeOf :: L.TExpr -> String
cgenTypeOf = cgenType . L.typeof

cgenType :: L.Type -> String
cgenType = \case
  L.TypeZero      -> "zero_t"
  L.TypeFloat     -> "double"
  L.TypeInteger   -> "int"
  L.TypeTuple [t] -> cgenType t
  L.TypeTuple ts  -> "tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  L.TypeVec   t   -> "vec<" ++ cgenType t ++ ">"
  L.TypeBool      -> "bool"
  L.TypeUnknown   -> "unk"
  L.TypeLambda from to ->
    "std::function<" ++ cgenType to ++ "(" ++ cgenType from ++ ")>"
  L.TypeLM ty -> cgenTypeLM ty

cgenTypeLM :: HasCallStack => L.TypeLM -> String
cgenTypeLM = \case
  L.LM s t -> "LM::Base" ++ angle s t
  L.LMZero s t -> "LM::Zero" ++ angle s t
  L.LMOne t-> "LM::One" ++ angle1 t
  L.LMScale t-> "LM::Scale" ++ angle1 t
  L.LMSelFun s t -> "LM::SelFun" ++ angle s t
  L.LMTranspose lm-> "LM::Transpose<" ++ cgenTypeLM lm ++ ">"
  L.LMCompose bc ab-> "LM::Compose<" ++ cgenTypeLM bc ++ "," ++ cgenTypeLM ab ++ ">"
  L.LMAdd a b-> "LM::Add<" ++ cgenTypeLM a ++ "," ++ cgenTypeLM b ++ ">"
  L.LMVCat lms-> "LM::VCat<" ++ intercalate "," (map cgenTypeLM lms) ++ ">"
  L.LMHCat lms-> "LM::HCat" ++ "<" ++ intercalate "," (map cgenTypeLM lms) ++ ">"
  L.LMBuild lm-> "LM::Build<" ++ cgenTypeLM lm ++ ">"
  L.LMBuildT lm-> "LM::BuildT<" ++ cgenTypeLM lm ++ ">"
  L.LMVariant lms-> "LM::Variant<" ++ intercalate "," (map cgenTypeLM lms) ++ ">"
  where
   angle s t = "<" ++ cgenType s ++ "," ++ cgenType t ++ ">"
   angle1 t = "<" ++ cgenType t ++ ">"

cgenKonst :: L.Konst -> String
cgenKonst = \case
  L.KZero      -> "0"
  L.KInteger i -> show i
  L.KFloat   f -> show f
  L.KBool    b -> if b then "TRUE" else "FALSE"

cgenVar :: L.Var -> String
cgenVar v = show v

cgenTVar :: L.TVar L.Var -> String
cgenTVar (L.TVar ty L.Dummy) = cgenType ty ++ "{}"
cgenTVar (L.TVar _ v) = show v


translateFun :: String -> String
translateFun = \case
  "*"  -> "mul"
  "+"  -> "add"
  "/"  -> "div"
  "-"  -> "sub"
  "==" -> "eq"
  "<"  -> "lt"
  s    -> s

cppF :: String -> [L.TDef] -> IO ()
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
  --putStrLn $ "Formatting " ++ cppfile
  --callCommand $ "clang-format -i " ++ cppfile
  let compcmd = "g++ -fmax-errors=5 -I. -O -g -std=c++17 " ++ cppfile ++ " -o " ++ exefile
  putStrLn $ "Compiling: " ++ compcmd
  callCommand compcmd
  putStrLn "Running"
  callCommand exefile
  putStrLn "Done"
