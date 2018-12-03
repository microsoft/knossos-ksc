{-# LANGUAGE LambdaCase, FlexibleInstances, TypeApplications  #-}

module Cgen where

import GHC.Stack
import           Debug.Trace                    ( trace )
import           Prelude                 hiding ( lines
                                                , tail
                                                )
import qualified Data.Map as Map
import           Data.List                      ( intercalate, isPrefixOf )
import           Control.Monad                  ( (<=<) )
import qualified Control.Monad.State           as S
import           System.Process                 ( callCommand )

import Lang

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

cgenIsZero s = "zero_t<" `isPrefixOf` s
cgenIsLM s = "LM::" `isPrefixOf` s

makeUnionType :: HasCallStack => String -> String -> String
makeUnionType ty1 ty2 = 
  if ty1 == ty2 then
    ty1
  else if cgenIsZero ty1 then
    ty2
  else if cgenIsZero ty2 then
    ty1
  else if cgenIsLM ty1 then
    "LM::Variant<" ++ ty1 ++ ", " ++ ty2 ++ ">"
  else 
    error("GENVAR["++ty1 ++ "," ++ ty2++"]") 
    -- $ "std::variant<" ++ ty1 ++ ", " ++ ty2 ++ ">"

-------------------- Cgen

-- CGenResult is (C declaration, C expression, Type)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r",
--       "double")
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0",      -- this is what we use at the occurrence
--       "double")
-- e.g. ("typedef LM::HCat<LM::VCat<LM::One, LM::Zero>,LM::Zero> v12_t;
--        v12_t v12 = v12_t::mk(a,b);",
--       "v12",      -- this is what we use at the occurrence
--       "v12_t")
data CGenResult = CG String String String
getDecl (CG dc _ _) = dc
getExpr (CG _ ex _) = ex
getType (CG _ _ ty) = ty

type CST = Map.Map String String
cstEmpty = Map.empty

cstMaybeLookup0 :: HasCallStack => String -> CST -> Maybe String
cstMaybeLookup0 s env = Map.lookup s env

cstLookup0 :: HasCallStack => String -> CST -> String
cstLookup0 v env = case cstMaybeLookup0 v env of
  Just e -> e
  Nothing -> error $ "Failed lookup [" ++ v ++ "] in\n" ++ (intercalate "\n" (map show (Map.toList env)))

cstInsertVar:: Var -> String -> CST -> CST
cstInsertVar v ty env = -- trace ("cstInsertVar [" ++ show v ++ "] = [" ++ show ty ++ "]\n" ++ show env) $
         Map.insert (show v) ty env

cstInsertFun:: Fun -> String -> CST -> CST
cstInsertFun f ctype env = -- trace ("cstInsertFun " ++ show f ++ " = " ++ show ctype ++ "\n" ++ show env) $
         Map.insert (show f) ctype env

cstLookupVar :: HasCallStack => Var -> CST -> String
cstLookupVar v env = cstLookup0 (show v) env

cstLookupFun :: HasCallStack => Fun -> CST -> String
cstLookupFun f env = cstLookup0 (show f) env

cstMaybeLookupFun :: HasCallStack => Fun -> CST -> Maybe String
cstMaybeLookupFun f env = cstMaybeLookup0 (show f) env

cComment:: String -> String
cComment s = "/* " ++ s ++ " */"

cgenDefs :: [TDef] -> [String]
cgenDefs defs = 
  snd $ foldl go (cstEmpty, []) defs
  where
    go :: (CST, [String]) -> TDef -> (CST, [String])
    go (env, strs) def@(DefX (TFun _ f) _ _) = 
      let (CG cdecl cfun ctype) = cgenDefE env def
      in (cstInsertFun f ctype env, strs ++ [cdecl])

cgenDef :: TDef -> String
cgenDef def = getDecl $ cgenDefE cstEmpty def

cgenDefE :: CST -> TDef -> CGenResult
cgenDefE env (DefX (TFun _ f) params body) =
  let addParam env (TVar ty v) = cstInsertVar v (cgenType ty) env
      env' = foldl addParam env params
      
      CG cbodydecl cbodyexpr cbodytype = runM $ cgenExpr env' body
      cf = cgenUserFun f
      
      mkVar (TVar ty var) = cgenType ty `spc` cgenVar var
      cvars = map mkVar params
      cftypealias = "ty$" ++ cf
  in  CG 
      (     "typedef " ++ cbodytype `spc` cftypealias ++ ";\n"
      ++    cftypealias `spc` cf
      ++    "(" ++ intercalate ", " cvars ++ ") {\n"
      ++    cbodydecl
      ++    "return (" ++ cbodyexpr ++ ");\n"
      ++    "}\n"
      )
      cf
      cftypealias

cgenExpr :: CST -> TExpr -> M CGenResult
cgenExpr = cgenExprR

cgenExprR :: HasCallStack => CST -> TExpr -> M CGenResult
cgenExprR env = \case
  Konst k  -> return $ CG "" (cgenKonst k) (cgenType $ typeofKonst k)
  Var (TVar ty Dummy) -> let cty = cgenType ty in return $ CG "" (cty ++ "{}") cty
  Var (TVar ty v) -> return $ CG "" (show v) (cstLookupVar v env) 

  Call tf@(TFun ty f) (Tuple vs) -> do
      -- Untuple argument for C++ call
      cgvs <- mapM (cgenExprR env) vs
      let cdecls = map getDecl cgvs
      let cexprs = map getExpr cgvs
      let ctypes = map getType cgvs

      let cftype = ctypeofFun env tf ctypes

      v <- freshCVar
      return $ 
        CG
        ( "/**Call**/"
        ++ intercalate "\n" cdecls
        ++ cftype
        ++ " "
        ++ v
        ++ " = "
        ++ cgenAnyFun tf cftype
        ++ "("
        ++ intercalate "," cexprs
        ++ ");\n/**eCall**/\n"
        )
        v
        cftype
        
  Call tf@(TFun ty f) v -> cgenExprR env $ Call tf (Tuple [v]) 

  Let (TVar tyv v) e1 body -> do
    (CG decle1 ve1 type1) <- cgenExprR env e1
    (CG declbody vbody tybody) <- cgenExprR (cstInsertVar v type1 env) body
    return $ CG
      (  "/**Let**/"
      ++ decle1
      ++ "\n"
      ++ type1
      ++ " "
      ++ cgenVar v
      ++ " = "
      ++ ve1
      ++ ";\n"
      ++ declbody )
      vbody
      tybody

  Tuple [t] -> cgenExpr env t
  Tuple vs  -> do
      cgvs <- mapM (cgenExprR env) vs
      let cdecls = map getDecl cgvs
      let cexprs = map getExpr cgvs
      let ctypes = map getType cgvs

      return $ CG
        (intercalate "\n" cdecls)
        ("std::make_tuple(" ++ intercalate "," cexprs ++ ")")
        ("std::tuple<" ++ intercalate "," ctypes ++ ">")

  Lam (TVar tyv v) body -> do
    lvar        <- freshCVar
    let vtype = cgenType tyv
    (CG cdecl cexpr ctype) <- cgenExprR (cstInsertVar v vtype env) body
    return $ CG
      ( "/**Lam**/"
      ++ "auto" 
      `spc` lvar
      ++    " = [=](" ++ vtype `spc` cgenVar v ++ ") { "  -- TODO: capture only freeVars here
      ++    cdecl
      ++    "   return (" ++ cexpr ++ ");"
      ++    "};\n" )
      lvar
      ("std::function<" ++ ctype ++ "(" ++ vtype ++ ")>")


  If c texpr fexpr -> do
    cret        <- freshCVar

    (CG declc vc tyc) <- cgenExprR env c
    (CG declt vt tyt) <- cgenExprR env texpr
    (CG declf vf tyf) <- cgenExprR env fexpr
    let crettype = makeUnionType tyt tyf
    let dotv = if "LM::Variant<" `isPrefixOf` crettype then ".v" else "" -- Ugh.

    return $ CG
      ( declc -- emit condition generation
      ++    crettype
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
      )
      cret
      crettype

  Assert cond body -> do
    (CG declcond vcond tycond) <- cgenExprR env cond
    (CG declbody vbody tybody) <- cgenExprR env body
    return $ CG 
      (declcond `spc` "ASSERT(" ++ vcond ++ ");\n" ++ declbody)
      vbody
      tybody

  App{} -> error "App"

cgenFunId :: FunId -> String
cgenFunId = \case
  UserFun fun -> fun
  PrimFun fun -> translateFun fun
  SelFun i n  -> "selfun$" ++ show n ++ "_" ++ show i

cgenUserFun :: HasCallStack => Fun -> String
cgenUserFun f = case f of
  Fun funId       -> cgenFunId funId
  GradFun s Fwd -> "D$" ++ cgenFunId s
  GradFun s Rev -> "R$" ++ cgenFunId s
  DrvFun  s Fwd -> "fwd$" ++ cgenFunId s
  DrvFun  s Rev -> "rev$" ++ cgenFunId s

cgenAnyFun :: HasCallStack => TFun -> String -> String
cgenAnyFun tf cftype = case tf of
  -- This is one of the LM subtypes, e.g. HCat<...>  Name is just HCat<...>::mk
  TFun (TypeLM s t) (Fun (PrimFun _)) -> cftype ++ "::mk"
  TFun ty (Fun (PrimFun "build")) -> let TypeVec t = ty in "build<"++ cgenType t ++ ">"
  TFun _ f -> cgenUserFun f 

cgenTypeOf :: TExpr -> String
cgenTypeOf = cgenType . typeof

cgenType :: HasCallStack => Type -> String
cgenType = \case
  TypeZero t    -> "zero_t<" ++ cgenType t ++ ">"
  TypeFloat     -> "double"
  TypeInteger   -> "int"
  TypeTuple [t] -> cgenType t
  TypeTuple ts  -> "tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  TypeVec   t   -> "vec<" ++ cgenType t ++ ">"
  TypeBool      -> "bool"
  TypeUnknown   -> "void"
  TypeLambda from to ->
    "std::function<" ++ cgenType to ++ "(" ++ cgenType from ++ ")>"
  TypeLM s t -> error $ "LM<" ++ cgenType s ++ "," ++ cgenType t ++ ">"

ctypeofFun :: HasCallStack => CST -> TFun -> [String] -> String
ctypeofFun env tf@(TFun ty f) ctys = case cstMaybeLookupFun f env of
    Just ctype -> -- trace ("Found fun " ++ show f) $
                  ctype 
    Nothing -> -- trace ("Did not find fun " ++ show tf ++ " in\n     " ++ show env) $
               ctypeofFun1 env ty f ctys

ctypeofFun1 :: HasCallStack => CST -> Type -> Fun -> [String] -> String
ctypeofFun1 env ty (Fun (PrimFun name)) ctys = ctypeofPrimFun ty name ctys
ctypeofFun1 env (TypeLM s t) (GradFun f Fwd) ctys = ctypeofGradBuiltin (show f) ctys
ctypeofFun1 env (TypeLM s t) f ctys = error $ "Did not match [" ++  show f ++ "]@\n  " ++ intercalate "\n  " ctys
ctypeofFun1 env ty f ctys = cgenType ty

ctypeofPrimFun :: HasCallStack => Type -> String -> [String] -> String
ctypeofPrimFun ty s arg_types = case s of
  "lmApply" -> cgenType ty
  "lmBuild" -> "LM::Build<" ++ arg_types!!1 ++ ">"
  "lmBuildT" -> "LM::BuildT<" ++ arg_types!!1 ++ ">"
  "lmOne" -> "LM::One<" ++ src ty ++">"
  "lmZero" -> "LM::Zero<" ++ src ty ++ "," ++ dst ty ++">"
  ('l':'m':s) -> "LM::" ++ s ++ angle arg_types
  _ -> cgenType ty
  where
   angle [t] = "<" ++ t ++ ">"
   angle ts = "<" ++ intercalate ","  ts ++ ">"
   src (TypeLM s t) = cgenType s
   dst (TypeLM s t) = cgenType t

ctypeofGradBuiltin :: HasCallStack => String -> [String] -> String
ctypeofGradBuiltin f ctys = case f of
  "-" -> hcatscales
  "/" -> hcatscales
  "*" -> hcatscales
  _ -> error $ "Don't know grad of [" ++ f ++ "]@\n  " ++ intercalate "\n  " ctys
  where hcatscales = "LM::HCat</*" ++ show ctys ++ "*/LM::Scale<double>, LM::Scale<double>>"

cgenKonst :: Konst -> String
cgenKonst = \case
  KZero TypeInteger -> "0"
  KZero TypeFloat -> "0.0"
  KZero t    -> "zero_t<" ++ cgenType t ++ "> {}"
  KInteger i -> show i
  KFloat   f -> show f
  KBool    b -> if b then "TRUE" else "FALSE"

cgenVar :: Var -> String
cgenVar v = show v


translateFun :: String -> String
translateFun = \case
  "*"  -> "mul"
  "+"  -> "add"
  "/"  -> "div"
  "-"  -> "sub"
  "==" -> "eq"
  "<"  -> "lt"
  s    -> s

cppF :: String -> [TDef] -> IO ()
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
  let compcmd = "g++ -fmax-errors=5 -Wall -Isrc/runtime -O -g -std=c++17 " ++ cppfile ++ " -o " ++ exefile
  putStrLn $ "Compiling: " ++ compcmd
  callCommand compcmd
  putStrLn "Running"
  callCommand exefile
  putStrLn "Done"
