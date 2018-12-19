{-# LANGUAGE LambdaCase, FlexibleInstances, TypeApplications, PatternSynonyms  #-}

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
import           System.Process                 ( callProcess )

import Lang

data CType =  CType Type
            | CTuple [CType]
            | CFunction CType CType
            | TypeDef String CType
            | LMZero Type Type
            | LMOne Type
            | LMScale
            | LMHCat [CType]
            | LMVCat [CType]
            | LMBuild CType
            | LMBuildT CType
            | LMCompose CType CType
            | LMAdd [CType]
            | LMVariant [CType]
            deriving (Eq, Ord, Show)

getTypeFromCType :: HasCallStack => CType -> Type
getTypeFromCType = \case
  CType t -> t
  t -> error $ "getTypeFromCType @ " ++ show t

promoteZero = \case
  CType (TypeZero ty) -> CType ty
  ty -> ty

stripTypeDef :: CType -> CType
stripTypeDef (TypeDef _ t) = stripTypeDef t
stripTypeDef t = t

cgenIsZero :: CType -> Bool
cgenIsZero = \case
  CType (TypeZero t) -> True
  _ -> False

cgenIsLM :: CType -> Bool
cgenIsLM = \case
  CType (TypeLM _ _) -> True
  CType _ -> False
  _ -> True

makeUnionType :: HasCallStack => CType -> CType -> (CType, String)
makeUnionType ty1 ty2 =
  if ty1 == ty2 then
    (ty1, "")
  else if cgenIsZero ty1 then
    (ty2, "static_cast<" ++ cgenType ty2 ++ ">")
  else if cgenIsZero ty2 then
    (ty1, "static_cast<" ++ cgenType ty1 ++ ">")
  else if cgenIsLM ty1 then
    (LMVariant [ty1, ty2], "")
  else
    error("GENVAR["++ show ty1 ++ "," ++ show ty2++"]")

-------------- String utils

spc :: String -> String -> String
spc x y = x ++ " " ++ y

-------------- State for var generator

type M = S.State Int

runM :: M a -> a
runM = flip S.evalState 0

freshCVar :: M String
freshCVar = do
  s <- S.get
  S.put (s + 1)
  return ("c$" ++ show s)

-------------------- Cgen

-- CGenResult is (C declaration, C expression, C type, CType)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r",
--       TypeDouble,
--       "double")
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0",      -- this is what we use at the occurrence
--       TypeDouble,
--       "double")
-- e.g. ("typedef LM::HCat<LM::VCat<LM::One, LM::Zero>,LM::Zero> v12_t;
--        v12_t v12 = v12_t::mk(a,b);",
--       "v12",      -- this is what we use at the occurrence
--       LMHCat [LMVCat [LMOne, LMZero], LMZero]
--       "v12_t")
data CGenResult = CG String String CType
getDecl (CG dc _ _) = dc
getExpr (CG _ ex _) = ex
getType (CG _ _ ty) = ty

type CST = Map.Map String CType
cstEmpty = Map.empty

cstMaybeLookup0 :: HasCallStack => String -> CST -> Maybe CType
cstMaybeLookup0 s env = Map.lookup s env

cstLookup0 :: HasCallStack => String -> CST -> CType
cstLookup0 v env = case cstMaybeLookup0 v env of
  Just e -> e
  Nothing -> error $ "Failed lookup [" ++ v ++ "] in\n" ++ (intercalate "\n" (map show (Map.toList env)))

cstInsertVar:: Var -> CType -> CST -> CST
cstInsertVar v ty env = -- trace ("cstInsertVar [" ++ show v ++ "] = [" ++ show ty ++ "]\n" ++ show env) $
         Map.insert (show v) ty env

cstInsertFun:: Fun -> CType -> CST -> CST
cstInsertFun f ctype env = -- trace ("cstInsertFun " ++ show f ++ " = " ++ show ctype ++ "\n" ++ show env) $
         Map.insert (show f) ctype env

cstLookupVar :: HasCallStack => Var -> CST -> CType
cstLookupVar v env = cstLookup0 (show v) env

cstLookupFun :: HasCallStack => Fun -> CST -> CType
cstLookupFun f env = cstLookup0 (show f) env

cstMaybeLookupFun :: HasCallStack => Fun -> CST -> Maybe CType
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
  let addParam env (TVar ty v) = cstInsertVar v (CType ty) env
      env' = foldl addParam env params

      CG cbodydecl cbodyexpr cbodytype = runM $ cgenExpr env' body
      cf = cgenUserFun f

      mkVar (TVar ty var) = cgenType (CType ty) `spc` cgenVar var
      cvars = map mkVar params

      cftypealias = "ty$" ++ cf
  in  CG
      (     "typedef " ++ cgenType cbodytype `spc` cftypealias ++ ";\n"
      ++    cftypealias `spc` cf
      ++    "(" ++ intercalate ", " cvars ++ ") {\n"
      ++    cbodydecl
      ++    "return (" ++ cbodyexpr ++ ");\n"
      ++    "}\n"
      )
      cf
      (TypeDef cftypealias cbodytype)

cgenExpr :: CST -> TExpr -> M CGenResult
cgenExpr = cgenExprR

cgenExprR :: HasCallStack => CST -> TExpr -> M CGenResult
cgenExprR env = \case
  Konst k  -> return $ CG "" (cgenKonst k) (CType $ typeofKonst k)
  Var (TVar ty Dummy) -> let cty = CType ty in return $ CG "" (cgenType cty ++ "{}") cty
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
        ++ cgenType cftype
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
    lvar        <- freshCVar

    return $ CG
      (  "/**Let**/"
      ++ cgenType tybody ++ " " ++ lvar ++ ";\n"
      ++ "{\n"
      ++ decle1
      ++ "\n"
      ++ cgenType type1
      ++ " "
      ++ cgenVar v
      ++ " = "
      ++ ve1
      ++ ";\n"
      ++ declbody ++ "\n"
      ++ lvar  ++ " = " ++ vbody ++ ";\n" 
      ++ "}\n"
      )
      lvar
      tybody

  Tuple [t] -> cgenExpr env t
  Tuple vs  -> do
      cgvs <- mapM (cgenExprR env) vs
      let cdecls = map getDecl cgvs
      let cexprs = map getExpr cgvs
      let ctypes = map getType cgvs
      let ctype = CTuple $ map promoteZero ctypes

      return $ CG
        (intercalate "\n" cdecls)
        (cgenType ctype ++ "{" ++ intercalate "," cexprs ++ "}")
        ctype

  Lam (TVar tyv v) body -> do
    lvar        <- freshCVar
    let vtype = CType tyv
    (CG cdecl cexpr ctype) <- cgenExprR (cstInsertVar v vtype env) body
    return $ CG
      ( "/**Lam**/"
      ++ "auto"
      `spc` lvar
      ++    " = [=](" ++ cgenType vtype `spc` cgenVar v ++ ") { "  -- TODO: capture only freeVars here
      ++    cdecl
      ++    "   return (" ++ cexpr ++ ");"
      ++    "};\n" )
      lvar
      (CFunction vtype ctype)


  If c texpr fexpr -> do
    cret        <- freshCVar

    (CG declc vc tyc) <- cgenExprR env c
    (CG declt vt tyt) <- cgenExprR env texpr
    (CG declf vf tyf) <- cgenExprR env fexpr
    let (crettype, cretcast) = makeUnionType tyt tyf
    let dotv = case crettype of
                LMVariant _ -> ".v"  -- Ugh.
                _ -> "" -- Ugh.

    return $ CG
      ( declc -- emit condition generation
      ++    cgenType crettype
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
      ++    "= " ++ cretcast ++ "(" ++ vt ++ ");\n" -- assign to "return"
      ++    "} else {\n" -- else
      ++    "  "
      ++    declf
      ++    ";\n" -- compute false value
      ++    "  "
      ++    cret ++ dotv
      ++    "= " ++ cretcast ++ "(" ++ vf ++ ");\n" -- assign to "return"
      ++    "}\n" -- phew
      )
      cret
      crettype

  Assert cond body -> do
    (CG declcond vcond (CType TypeBool)) <- cgenExprR env cond
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
  where
    translateFun :: String -> String
    translateFun = \case
      "*"  -> "mul"
      "+"  -> "add"
      "/"  -> "div"
      "-"  -> "sub"
      "==" -> "eq"
      "<"  -> "lt"
      s    -> s

cgenUserFun :: HasCallStack => Fun -> String
cgenUserFun f = case f of
  Fun funId       -> cgenFunId funId
  GradFun s Fwd -> "D$" ++ cgenFunId s
  GradFun s Rev -> "R$" ++ cgenFunId s
  DrvFun  s Fwd -> "fwd$" ++ cgenFunId s
  DrvFun  s Rev -> "rev$" ++ cgenFunId s

cgenAnyFun :: HasCallStack => TFun -> CType -> String
cgenAnyFun tf cftype = case tf of
  TFun ty (Fun (PrimFun "lmApply")) -> "lmApply"
  TFun ty (Fun (PrimFun "build")) -> let TypeVec t = ty in "build<"++ cgenType (CType t) ++ ">"
  -- This is one of the LM subtypes, e.g. HCat<...>  Name is just HCat<...>::mk
  TFun (TypeLM s t) (Fun (PrimFun _)) -> cgenType cftype ++ "::mk"
  TFun _ f -> cgenUserFun f

cgenTypeOf :: TExpr -> String
cgenTypeOf = cgenType . CType . typeof

cgenType :: HasCallStack => CType -> String
cgenType = \case
  CType ty -> cgenTypeLang ty
  CTuple ts  -> "tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  CFunction s t  -> "std::function<" ++ (cgenType t) ++ "(" ++ (cgenType s) ++ ")>"
  TypeDef s ty -> s
  LMZero s t -> lmt "Zero" [s, t]
  LMOne t -> lmt "One" [t]
  LMScale -> "LM::Scale"
  LMHCat ts -> lm "HCat" ts
  LMVCat ts -> lm "VCat" ts
  LMBuild t -> lm "Build" [t]
  LMBuildT t -> lm "BuildT" [t]
  LMCompose m1 m2 -> lm "Compose" [m1, m2]
  LMAdd ms -> lm "Add" ms
  LMVariant ts -> lm "Variant" ts
  where
    lm s ts = "LM::" ++ s ++ "<" ++ intercalate "," (map cgenType ts) ++ ">"
    lmt s ts = "LM::" ++ s ++ "<" ++ intercalate "," (map cgenTypeLang ts) ++ ">"

cgenTypeLang :: HasCallStack => Type -> String
cgenTypeLang = \case
  TypeZero t    -> "zero_t<" ++ cgenTypeLang t ++ ">"
  TypeFloat     -> "double"
  TypeInteger   -> "int"
  TypeTuple [t] -> cgenTypeLang t
  TypeTuple ts  -> "tuple<" ++ intercalate "," (map cgenTypeLang ts) ++ ">"
  TypeVec   t   -> "vec<" ++ cgenTypeLang t ++ ">"
  TypeBool      -> "bool"
  TypeUnknown   -> "void"
  TypeLambda from to -> "std::function<" ++ (cgenTypeLang to) ++ "(" ++ (cgenTypeLang from) ++ ")>"
  TypeLM s t -> error $ "LM<" ++ cgenTypeLang s ++ "," ++ cgenTypeLang t ++ ">"

ctypeofFun :: HasCallStack => CST -> TFun -> [CType] -> CType
ctypeofFun env tf@(TFun ty f) ctys = case cstMaybeLookupFun f env of
    Just ctype -> -- trace ("Found fun " ++ show f) $
                  ctype
    Nothing -> -- trace ("Did not find fun " ++ show tf ++ " in\n     " ++ show env) $
               ctypeofFun1 env ty f ctys

ctypeofFun1 :: HasCallStack => CST -> Type -> Fun -> [CType] -> CType
ctypeofFun1 env ty (Fun (PrimFun name)) ctys = ctypeofPrimFun ty name ctys
ctypeofFun1 env (TypeLM s t) (GradFun f Fwd) ctys = ctypeofGradBuiltin f ctys
ctypeofFun1 env (TypeLM s t) f ctys = error $ "Did not match [" ++  show f ++ "]@\n  " ++ intercalate "\n  " (map show ctys)
ctypeofFun1 env ty f ctys = CType ty

ctypeofPrimFun :: HasCallStack => Type -> String -> [CType] -> CType
ctypeofPrimFun ty s arg_types = case (s, map stripTypeDef arg_types) of
  ("lmApply", _) -> CType ty
  ("lmBuild", [_, lam]) -> LMBuild lam
  ("lmBuildT", [_, lam]) -> LMBuildT lam
  ("lmOne", [CType t]) -> LMOne t
  ("lmZero", [CType s, CType t]) -> LMZero s t
  ("lmScale", [CType TypeFloat]) -> LMScale
  ("lmHCat", _) -> LMHCat arg_types
  ("lmVCat", _) -> LMVCat arg_types
  ("lmCompose", [lm1, lm2]) -> LMCompose lm1 lm2
  ("lmAdd", _) -> LMAdd arg_types
  ("lmVariant", _) -> LMVariant arg_types
  _ -> case ty of
        TypeLM _ _ -> error ("Unmatched prim [" ++ s ++ "] " ++ show ty ++ " @\n  " ++ intercalate "\n  " (map show arg_types))
        _ -> CType ty

pattern RR = TypeFloat
pattern VecR = TypeVec TypeFloat

ctypeofGradBuiltin :: HasCallStack => FunId -> [CType] -> CType
ctypeofGradBuiltin f ctys = case (f, map stripTypeDef ctys) of
  (PrimFun "-", [CType RR, CType RR]) -> LMHCat [LMScale, LMScale]
  (PrimFun "+", [CType RR, CType RR]) -> LMHCat [LMScale, LMScale]
  (PrimFun "/", [CType RR, CType RR]) -> LMHCat [LMScale, LMScale]
  (PrimFun "*", [CType RR, CType RR]) -> LMHCat [LMScale, LMScale]
  (PrimFun "to_float", [CType TypeInteger]) -> LMZero TypeInteger TypeFloat
  (PrimFun "$trace", [CType ty]) -> LMOne ty
  (PrimFun "$rand", [CType ty]) -> LMZero ty ty
  (PrimFun "size", [CType ty]) -> LMZero ty TypeInteger
  (PrimFun "index", [CType (TypeVec t)]) -> LMHCat [LMZero TypeInteger t, LMBuild (LMScale)]
  _ -> error $ "Don't know grad of [" ++ show f ++ "]@\n  " ++ intercalate "\n  " (map (show . stripTypeDef) ctys)

cgenKonst :: Konst -> String
cgenKonst = \case
  KZero t    -> "zero_t<" ++ cgenTypeLang t ++ "> {}"
  KInteger i -> show i
  KFloat   f -> show f
  KBool    b -> if b then "TRUE" else "FALSE"

cgenVar :: Var -> String
cgenVar v = show v


cppF :: String -> [TDef] -> IO ()
-- String is the file name
cppF outfile defs = do
  let lines = ["#include <stdio.h>", "#include \"knossos.h\"", "namespace ks {\n"]

  let lls = cgenDefs defs

  let tail = ["}", "int main() {", "  ks::main();", "  return 0;", "}"]
  let ksofile = outfile ++ ".kso"
  putStrLn $ "Writing to " ++ ksofile
  writeFile ksofile (show $ ppr defs)

  let cppfile = outfile ++ ".cpp"
  putStrLn $ "Writing to " ++ cppfile
  writeFile cppfile (intercalate "\n" (lines ++ lls ++ tail))

  let exefile = outfile ++ ".exe"
  --putStrLn $ "Formatting " ++ cppfile
  --callCommand $ "clang-format -i " ++ cppfile
  let compcmd = ("g++-7", ["-fmax-errors=5", "-Wall", "-Isrc/runtime", "-O", "-g", "-std=c++17", cppfile, "-o", exefile])
  putStrLn $ "Compiling: " ++ fst compcmd ++ " " ++ unwords (snd compcmd)
  uncurry callProcess compcmd
  putStrLn "Running"
  callProcess exefile []
  putStrLn "Done"
