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

class CfreshVar b where
  freshVar :: Type -> M b

instance CfreshVar Var where
  freshVar _ = do
    -- This doesn't communicate very well but is quick (and dirty)
    s <- S.get
    S.put (s + 1)
    return $ Simple $ "v$" ++ show s

instance CfreshVar TVar where
  freshVar ty = do
                 v <- freshVar @Var ty
                 return $ TVar ty v

{-
------------ Specialist linear map types
data TypeLM
          = LM Type Type             -- Generic linear map
          | LMZero Type Type         -- Zeros may be rectangular
          | LMOne Type               -- Ones must be square
          | LMScale Type             -- Scalar will always be of TypeFloat, Type is the type being scaled
          | LMSelFun Type Type
          | LMTranspose TypeLM
          | LMCompose TypeLM TypeLM
          | LMAdd TypeLM TypeLM
          | LMVCat [TypeLM]
          | LMHCat [TypeLM]
          | LMVariant [TypeLM]        -- One of the arg types, to be selected by another boolean
          | LMBuild TypeLM
          | LMBuildT TypeLM
        deriving (Show, Eq, Ord)

------------------------- TypeLM methods ------------------------


nameOfType :: TypeLM -> String
nameOfType (LM s t) = "LM**"
nameOfType (LMZero s t) = "LMZero"
nameOfType (LMOne t) = "LMOne"
nameOfType (LMScale t) = "LMScale"
nameOfType (LMSelFun s t) = "LMSelFun"
nameOfType (LMTranspose lm) = "LMTranspose<" ++ nameOf lm ++ ">"
nameOfType (LMCompose bc ab) = "LMCompose<" ++ nameOf bc ++ "," ++ nameOf ab ++ ">"
nameOfType (LMAdd a b) = "LMAdd<" ++ nameOf a ++ "," ++ nameOf b ++ ">"
nameOfType (LMVCat lms) = "LMVCat<" ++ intercalate "," (map nameOf lms) ++ ">"
nameOfType (LMHCat lms) = "LMHCat<" ++ intercalate "," (map nameOf lms) ++ ">"
nameOfType (LMBuild lm) = "LMBuild<" ++ nameOf lm ++ ">"
nameOfType (LMBuildT lm) = "LMBuildT<" ++ nameOf lm ++ ">"
nameOfType (LMVariant lms) = "LMVariant<" ++ intercalate "," (map nameOf lms) ++ ">"

nameOf :: TypeLM -> String
nameOf (LM s t) = "lmBase"
nameOf (LMZero s t) = "lmZero"
nameOf (LMOne t) = "lmOne"
nameOf (LMScale t) = "lmScale"
nameOf (LMSelFun s t) = "lmSelFun"
nameOf (LMTranspose lm) = "lmTranspose"
nameOf (LMCompose bc ab) = "lmCompose"
nameOf (LMAdd a b) = "lmAdd"
nameOf (LMVCat lms) = "lmVCat"
nameOf (LMHCat lms) = "lmHCat"
nameOf (LMBuild lm) = "lmBuild"
nameOf (LMBuildT lm) = "lmBuildT"
nameOf (LMVariant lms) = "lmVariant"

typeofSrc :: TypeLM -> Type
typeofSrc (LM s t) = s
typeofSrc (LMZero s t) = s
typeofSrc (LMOne t) = t
typeofSrc (LMScale t) = t
typeofSrc (LMSelFun s t) = s
typeofSrc (LMTranspose lm) = typeofDst lm
typeofSrc (LMCompose bc ab) = typeofSrc ab
typeofSrc (LMAdd a b) = typeofSrc a
typeofSrc (LMVCat (lm:lms)) = typeofSrc lm
typeofSrc (LMHCat lms) = TypeTuple $ map typeofSrc lms
typeofSrc (LMBuild lm) = typeofSrc lm
typeofSrc (LMBuildT lm) = TypeVec (typeofSrc lm)
typeofSrc (LMVariant lms) = assertAllEqualRet "lmvariant" (map typeofSrc lms)

typeofDst :: TypeLM -> Type
typeofDst (LM s t) = t
typeofDst (LMZero s t) = t
typeofDst (LMOne t) = t
typeofDst (LMScale t) = t
typeofDst (LMSelFun s t) = t
typeofDst (LMTranspose lm) = typeofSrc lm
typeofDst (LMCompose bc ab) = typeofDst bc
typeofDst (LMAdd a b) = typeofDst a
typeofDst (LMVCat lms) = TypeTuple $ map typeofDst lms
typeofDst (LMHCat (l:lms)) = typeofDst l
typeofDst (LMBuild lm) = TypeVec (typeofDst lm)
typeofDst (LMBuildT lm) = typeofDst lm
typeofDst (LMVariant lms) = assertAllEqualRet "lmvariant/dst" (map typeofDst lms)

transpose :: TypeLM -> TypeLM
transpose = \case
    LM s t          -> LM t s
    LMZero s t      -> LMZero t s
    LMOne t         -> LMOne t
    LMScale t       -> LMScale t
    LMSelFun s t    -> LM t s
    LMTranspose lm  -> lm
    LMCompose bc ab -> LMCompose (transpose ab) (transpose bc)
    LMAdd a b       -> LMAdd (transpose a) (transpose b)
    LMVCat lms      -> LMHCat (map transpose lms)
    LMHCat lms      -> LMVCat (map transpose lms)
    LMBuild lm      -> LMBuildT lm
    LMBuildT lm     -> LMBuild lm
    LMVariant lms   -> LMVariant (map transpose lms)

{-
typeofLMFun :: HasCallStack => String -> [Type] -> TypeLM
typeofLMFun f ty = case --trace ("typeofLMFun " ++ show f ++ " @ " ++ show ty)
                        (f, ty) of
  ("lmZero" , [s, t])   -> LMZero s t
  ("lmOne" ,  [t])      -> LMOne t
  ("lmScale", [t, TypeFloat]) -> LMScale t
  ("lmBuild", [TypeInteger, TypeLambda TypeInteger ty]) -> LMBuild ty
  ("lmBuildT", [TypeInteger, TypeLambda TypeInteger ty]) -> LMBuildT ty
  ("lmVCat",  lms) -> LMVCat $ map getLM lms
  ("lmHCat",  lms) -> LMHCat $ map getLM lms
  ("lmCompose", [TypeLM s t, TypeLM r s]) -> LMCompose lm1 lm2
  ("lmAdd",  [TypeLM lm1, TypeLM lm2]) -> LMAdd lm1 lm2
  _ ->
    error -- flip trace (LM TypeUnknown TypeUnknown)
      $  "Failed to type LMfun ("
      ++ show f
      ++ ", "
      ++ show ty
      ++ ")"
-}
-}

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

cstInsert:: String -> String -> CST -> CST
cstInsert f ty env = -- trace ("cstInsert " ++ show f ++ " = " ++ show ty ++ "\n" ++ show env) $
         Map.insert f ty env

cstLookup :: HasCallStack => String -> CST -> String
cstLookup v env = case Map.lookup v env of
  Just e -> e
  Nothing -> error $ "Failed lookup [" ++ v ++ "] in\n" ++ (intercalate "\n" (map show (Map.toList env)))

cComment:: String -> String
cComment s = "/* " ++ s ++ " */"

cgenDefs :: [TDef] -> [String]
cgenDefs defs = 
  snd $ foldl go (cstEmpty, []) defs
  where
    go :: (CST, [String]) -> TDef -> (CST, [String])
    go (env, strs) def = 
      let (CG cdecl cexpr ctype) = cgenDefE env def
      in (cstInsert cexpr ctype env, strs ++ [cdecl])

cgenDef :: TDef -> String
cgenDef def = getDecl $ cgenDefE cstEmpty def

cgenDefE :: CST -> TDef -> CGenResult
cgenDefE env (DefX f vars expr) =
  let CG cdecl cexpr ctype = runM $ cgenExpr env expr
      cf = cgenFun f ctype
      mkvar (TVar ty var) = cgenType ty `spc` cgenVar var
      cvars = map mkvar vars
      cftypealias = "ty$" ++ cf
  in  CG 
      (  "typedef " ++ ctype `spc` cftypealias ++ ";\n"
      ++     cftypealias `spc` cf
      ++    "(" ++ intercalate ", " cvars ++ ") {\n"
      ++    cdecl
      ++    "return (" ++ cexpr ++ ")"
      ++    ";\n}\n"
      )
      cf
      cftypealias

cgenExpr :: CST -> TExpr -> M CGenResult
cgenExpr = cgenExprR

cgenExprR :: HasCallStack => CST -> TExpr -> M CGenResult
cgenExprR env = \case
  Konst k  -> return $ CG "" (cgenKonst k) (cgenType $ typeofKonst k)
  Var (TVar ty Dummy) -> let cty = cgenType ty in return $ CG "" (cty ++ "{}") cty
  Var (TVar ty v) -> return $ CG "" (show v) ( 
                          case (Map.lookup (show v) env) of
                          Just e -> "/*vv "++show v++"*/" ++ e
                          Nothing -> cgenType ty)

  Call tf@(TFun ty f) x -> case x of
    Tuple vs -> do
      cgresults <- mapM (cgenExprR env) vs
      let cdecls = map getDecl cgresults
      let cexprs = map getExpr cgresults
      let ctypes = map getType cgresults

      let cftype = ctypeofFun env ty f ctypes

      v <- freshCVar
      return $ 
        CG
        ( "/**Call**/"
        ++ intercalate "\n" cdecls
        ++ cftype
        ++ " "
        ++ v
        ++ " = "
        ++ cgenFun tf cftype
        ++ "("
        ++ intercalate "," cexprs
        ++ ");\n/**eCall**/\n"
        )
        v
        cftype
        
    ex -> do
      vf <- freshCVar
      (CG cdecl cexpr ctype) <- cgenExprR env ex

      let cftype = ctypeofFun env ty f [ctype]

      return $
        CG
        ( "/**Ex**/\n"
        ++    cdecl
        ++    cftype
        `spc` vf
        ++    " = "
        ++    cgenFun tf cftype
        ++    "("
        ++    cexpr
        ++    ");\n/**eEx*/\n" )
        vf
        cftype

  Let (TVar tyv v) e1 body -> do
    (CG decle1 ve1 type1) <- cgenExprR env e1
    (CG declbody vbody tybody) <- cgenExprR (cstInsert (show v) type1 env) body
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
      cgresults <- mapM (cgenExprR env) vs
      let cdecls = map getDecl cgresults
      let cexprs = map getExpr cgresults
      let ctypes = map getType cgresults

      return $ CG
        (intercalate "\n" cdecls)
        ("std::make_tuple(" ++ intercalate "," cexprs ++ ")")
        ("std::tuple<" ++ intercalate "," ctypes ++ ">")

  Lam (TVar tv v) body -> do
    lvar        <- freshCVar
    let vtype = cgenType tv
    (CG cdecl cexpr ctype) <- cgenExprR (cstInsert (show v) vtype env) body
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
  SFun fun   -> translateFun fun
  SelFun i n -> "selfun$" ++ show n ++ "_" ++ show i

cgenFun :: HasCallStack => TFun -> String -> String
cgenFun tf@(TFun ty f) ctype = case f of
  Fun (SFun "build") -> let (TypeVec t) = ty in "build<"++ cgenType t ++ ">"
  Fun funId       -> cgenFunId funId
  GradFun s Fwd -> "D$" ++ cgenFunId s
  GradFun s Rev -> "R$" ++ cgenFunId s
  DrvFun  s Fwd -> "fwd$" ++ cgenFunId s
  DrvFun  s Rev -> "rev$" ++ cgenFunId s
  LMFun "lmApply" -> "lmApply"
  LMFun s       -> ctype ++ "::mk"

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

ctypeofFun :: HasCallStack => CST -> Type -> Fun -> [String] -> String
ctypeofFun env ty f ctys = case Map.lookup (show f) env of
    Just ctype -> ctype -- trace ("Found fun " ++ show f) 
    Nothing -> ctypeofFun1 env ty f ctys

ctypeofFun1 :: HasCallStack => CST -> Type -> Fun -> [String] -> String
ctypeofFun1 env ty (LMFun "lmApply") ctys = cgenType ty
ctypeofFun1 env ty (LMFun name) ctys = ctypeofLMFun name ctys
ctypeofFun1 env (TypeLM s t) (GradFun f Fwd) ctys = ctypeofGradBuiltin (show f) ctys
ctypeofFun1 env (TypeLM s t) f ctys = error $ "Did not match [" ++  show f ++ "]@\n  " ++ intercalate "\n  " ctys
ctypeofFun1 env ty f ctys = cgenType ty

ctypeofLMFun :: HasCallStack => String -> [String] -> String
ctypeofLMFun s arg_types = case s of
  "lmBuild" -> "LM::Build<" ++ arg_types!!1 ++ ">"
  "lmBuildT" -> "LM::BuildT<" ++ arg_types!!1 ++ ">"
  ('l':'m':s) -> "LM::" ++ s ++ angle arg_types
  s -> error ("Not LM Fun! [" ++ s ++ "]") 
      -- "LM::" ++ s ++ angle arg_types
  where
   angle [t] = "<" ++ t ++ ">"
   angle ts = "<" ++ intercalate ","  ts ++ ">"

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
