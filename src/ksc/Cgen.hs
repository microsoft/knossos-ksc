-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE LambdaCase, FlexibleInstances, TypeApplications, PatternSynonyms  #-}

module Cgen where

import           GHC.Stack
import           Prelude                 hiding ( lines
                                                , tail
                                                )

import qualified Data.Map                      as Map
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( mapMaybe )
import           Control.Monad                  ( when )
import qualified Control.Monad.State           as S
import qualified System.Directory
import qualified System.FilePath
import qualified System.Process
import           System.Exit                    ( ExitCode(ExitSuccess) )

import           Lang                    hiding ( (<>) )
import qualified OptLet

import Debug.Trace

data CType =  CType Type
            | CTuple [CType]
            | CFunction CType CType
            | TypeDef String CType
            | UseTypeDef String
            | LMZero Type Type
            | LMOne Type
            | LMScale Type
            | LMHCat [CType]
            | LMVCat [CType]
            | LMBuild CType
            | LMCompose CType CType
            | LMAdd [CType]
            | LMVariant [CType]
            deriving (Eq, Ord, Show)

isScalar :: CType -> Bool
isScalar = \case
  CType  t      -> Lang.isScalar t
  CTuple ts     -> all Cgen.isScalar ts
  CFunction _ _ -> False
  TypeDef   _ t -> Cgen.isScalar t
  UseTypeDef _  -> False
  LMZero _ _    -> False
  LMOne _       -> False
  LMScale  _    -> False
  LMHCat   _    -> False
  LMVCat   _    -> False
  LMBuild  _    -> False
  LMCompose _ _ -> False
  LMAdd     _   -> False
  LMVariant _   -> False

mkCType :: Type -> CType
mkCType (TypeTuple ts) = CTuple $ map mkCType ts
mkCType ty             = CType ty

stripTypeDef :: HasCallStack => CType -> CType
stripTypeDef (TypeDef _ t) = stripTypeDef t
stripTypeDef t             = t

stripCType :: CType -> Type
stripCType = \case
  CType  ty     -> ty
  CTuple tys    -> TypeTuple $ map stripCType tys
  TypeDef _ cty -> stripCType cty
  _             -> error "LM/Function type in stripCType"

cgenIsLM :: HasCallStack => CType -> Bool
cgenIsLM = \case
  CType  (TypeLM _ _) -> True
  CType  _            -> False
  CTuple ts           -> any cgenIsLM ts
  CFunction _ _       -> False
  TypeDef   _ ty      -> cgenIsLM ty
  UseTypeDef s        -> error ("Don't know; it's a UseTypeDef: " ++ s)
  _                   -> True

isUseTypeDef :: CType -> Bool
isUseTypeDef (UseTypeDef _) = True
isUseTypeDef _ = False

makeUnionType :: HasCallStack => CType -> CType -> CType
makeUnionType (CTuple ts) (CTuple us) =
  if ts == us
  then CTuple ts
  else
    CTuple $ zipWith makeUnionType ts us

makeUnionType ty1 ty2 =
  if ty1 == ty2
  then ty1
  else if isUseTypeDef ty1  -- Punt and hope it's fine for now...
  then ty2
  else if isUseTypeDef ty2
  then ty1
  else if cgenIsLM (trace ("***Making variant from \n" ++ show ty1 ++ "\n" ++ show ty2) ty1)
  then  -- trace ("***Making variant from \n" ++ show ty1 ++ "\n" ++ show ty2)
        LMVariant [ty1, ty2]
  else
    let sty1 = stripCType ty1
        sty2 = stripCType ty2
    in  if sty1 == sty2
          then mkCType sty1
          else error
            ("GENVAR[\n" ++ show ty1 ++ "\n" ++ show ty2 ++ "\n]")

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

-- CGenResult is (C declaration, C expression, CType)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r",
--       TypeDouble)
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0",      -- this is what we use at the occurrence
--       TypeDouble)
-- e.g. ("typedef LM::HCat<LM::VCat<LM::One, LM::Zero>,LM::Zero> v12_t;
--        v12_t v12 = v12_t::mk(a,b);",
--       "v12",      -- this is what we use at the occurrence
--       LMHCat [LMVCat [LMOne, LMZero], LMZero])
data CGenResult = CG String String CType -- TODO: rename CG CGenResult

getDecl :: CGenResult -> String
getDecl (CG dc _ _) = dc

getExpr :: CGenResult -> String
getExpr (CG _ ex _) = ex

getType :: CGenResult -> CType
getType (CG _ _ ty) = ty

type CSTKey = Fun
type CST    = Map.Map CSTKey ()

cstMaybeLookupFun :: HasCallStack => Fun -> CST -> Maybe ()
cstMaybeLookupFun = Map.lookup

cComment :: String -> String
cComment s = "/* " ++ s ++ " */"

cgenDefs :: [TDef] -> [String]
cgenDefs defs = concatMap cdecl $
                filter isUserDef defs
 where
  env = Map.fromList (mapMaybe (\(Def { def_fun = f
                                      , def_rhs = rhs
                                      }) ->
                                  case rhs of
                                    UserRhs _ -> Just (f, ())
                                    _         -> Nothing) defs)

  cdecl def =
    let (CG cdecl_ _cfun _ctype) = cgenDefE env def
    in [cdecl_]

{- Note [Unpack tuple arguments]

Defs of a tuple argument have their argument list unpacked,
that is, if f is a Def of type Tuple [S1, ..., SN] -> T
then the C++ function we generate for it will have N
arguments, not one tuple.

We choose N names for the argument variables and pack them into a
tuple before emitting the function body.  We ensure that the names
that we chose are not used in the function body by using substExpr.
-}

ensureDon'tReuseParams :: [TVar] -> TExpr -> TExpr
ensureDon'tReuseParams = OptLet.substExpr . OptLet.mkEmptySubst

params_withPackedParams :: TVarX -> ([TVarX], TExpr -> TExpr)
params_withPackedParams param = case typeof param of
  -- See Note [Unpack tuple arguments]
  TypeTuple tys ->
    let params  = zipWith mkParam [1..] tys
        mkParam i ty = TVar ty (Simple name)
          where name = nameOfVar (tVarVar param) ++ "arg" ++ show i
        packParams = Let param (Tuple (map Var params))
    in (params, ensureDon'tReuseParams params . packParams)
  _             -> ([param], id)

cgenDefE :: CST -> TDef -> CGenResult
cgenDefE env (Def { def_fun = f, def_args = param
                  , def_rhs = UserRhs body }) =
  let cf                         = cgenUserFun f
      mkVar (TVar ty var)        = cgenType (mkCType ty) `spc` cgenVar var
      (params, withPackedParams) = params_withPackedParams param
      CG cbodydecl cbodyexpr cbodytype =
        runM $ cgenExpr env (withPackedParams body)
      cvars       = map mkVar params
      cftypealias = "ty$" ++ cf
  in  CG
        (     "typedef "
        ++    cgenType cbodytype
        `spc` cftypealias
        ++    ";\n"
        ++    cftypealias
        `spc` cf
        ++    "("
        ++    intercalate ", " cvars
        ++    ") {\n"
        ++    cbodydecl
        ++    "return ("
        ++    cbodyexpr
        ++    ");\n"
        ++    "}\n"
        )
        cf
        (TypeDef cftypealias cbodytype)

cgenDefE _ def = pprPanic "cgenDefE" (ppr def)
  -- Should not happen because of the 'filter isUserDef' in cgenDefs

cgenExpr :: CST -> TExpr -> M CGenResult
cgenExpr = cgenExprR

cgenExprR :: HasCallStack => CST -> TExpr -> M CGenResult
cgenExprR env = \case
  Konst k -> return $ CG "" (cgenKonst k) (mkCType $ typeofKonst k)
  Dummy ty ->
    let cty = mkCType ty in return $ CG "" (cgenType cty ++ "{}") cty
  Var (TVar ty v)               -> return $ CG "" (cgenVar v) (mkCType ty)

  -- Special case for build -- inline the loop
  Call (TFun (TypeVec ty) (Fun (PrimFun "build"))) (Tuple [sz, Lam (TVar vty var) body]) -> do
    CG szdecl szex _szty <- cgenExprR env sz
    let varty = mkCType vty
    let varcty = cgenType varty
    CG bodydecl bodyex _bodyty <- cgenExprR env body

    ret  <- freshCVar
    cvar <- freshCVar

    return $ CG
        (  "/*build*/\n"
        ++ szdecl
        ++ "vec<" ++ (cgenType $ mkCType ty) ++ "> " ++ ret ++ "(" ++ szex ++ ");\n"
        ++ "for(" ++ varcty ++ " " ++ cvar ++ " = 0;"
                  ++ cvar ++ " < " ++ szex ++ ";"
                  ++ " ++" ++ cvar ++ ") {\n"
        ++ ("   " ++ varcty ++ " " ++ cgenVar var ++ " = " ++ cvar ++ ";\n" )
        ++ "   " ++ bodydecl ++ "\n"
        ++ "   " ++ ret ++ "[" ++ cvar ++ "] = " ++ bodyex ++ ";\n"
        ++ "}\n"
        )
        ret
        (mkCType (TypeVec ty))

  -- Special case for sumbuild -- inline the loop
  Call (TFun ty (Fun (PrimFun "sumbuild"))) (Tuple [sz, Lam (TVar vty var@(Simple _)) body]) -> do
    CG szdecl szex _szty <- cgenExprR env sz
    let varty = mkCType vty
    let varcty = cgenType varty
    CG bodydecl bodyex _bodyty <- cgenExprR env body

    let cretty = cgenType $ mkCType ty
    ret  <- freshCVar
    bumpmark <- freshCVar

    return $ CG
        (  "/*sumbuild*/\n"
        ++ szdecl
        ++ "KS_ASSERT(" ++ szex ++ " > 0);\n"
        ++ cretty ++ " " ++ ret ++ ";\n"
        ++ "{\n"
        ++ "   " ++ varcty ++ " " ++ cgenVar var ++ " = 0;\n"
        ++ "   do {\n"
        ++ "     $MRK(" ++ bumpmark ++ ");\n" -- TODO: this is just to declare it
        ++       bodydecl
        --       First time round, deep copy it, put it in the ret, then mark the allocator
        ++ "     if (" ++ cgenVar var ++ " == 0) {\n"
        ++ "       " ++ ret ++ " = inflated_deep_copy(" ++ bodyex ++ ");\n"
        ++ "       $MRK(" ++ bumpmark ++ ");\n"
        ++ "     } else {\n"
        ++ "       inplace_add_t<"++ cretty ++">::go(&" ++ ret ++ ", " ++ bodyex ++ ");\n"
        --         Release the allocator back to where it was on iter 0
        ++ "       $REL(" ++ bumpmark ++ ");\n"
        ++ "     }\n"
        ++ "   } while (++" ++ cgenVar var ++ " < " ++ szex ++ ");\n"
        ++ "}\n"
        )
        ret
        (mkCType ty)




  Call tf@(TFun _ fun) vs -> do
    cgvs_tys <- do cgv <- cgenExprR env vs; return (cgv, typeof vs)
    let cgvs = fst cgvs_tys
    let cdecls = getDecl cgvs
    let cexprs_tys = (\(v, ty) -> (getExpr v, ty)) cgvs_tys
    let ctypes = getType cgvs

    let cftype = ctypeofFun env tf [ctypes]

    v        <- freshCVar
    bumpmark <- freshCVar

    let dogc = Cgen.isScalar cftype
    let gc tag =  if dogc
                    then tag ++ "(" ++ bumpmark ++ ");\n"
                    else ""

    let cf = cgenAnyFun tf cftype

    return $ CG
      (  cdecls
      ++ gc "$MRK"
      ++ cgenType cftype ++ " " ++ v ++ " = "
      ++ case (not (isSelFun (funIdOfFun fun)), cexprs_tys) of
          -- Untuple argument for C++ call
          --
          -- Calls of a tuple argument have their argument list
          -- unpacked.  See the explanation in cgenDefE above.
          -- SelFuns translate to C++ get, so they don't have their
          -- argument lists unpacked!
          (True, (cexpr, TypeTuple ts))
            -> cf ++ "("
               ++ intercalate ","
                      (flip map [0..length ts - 1] $ \i ->
                          "std::get<" ++ show i ++ ">(" ++ cexpr ++ ")")
               ++ ");\n"
          (_, (cexpr, _)) -> cf ++ "(" ++ cexpr ++ ");\n"
      ++ gc "$REL"
      )
      v
      cftype

  Let (TVar _ v) e1 body -> do
    (CG decle1   ve1   type1 ) <- cgenExprR env e1
    (CG declbody vbody tybody) <- cgenExprR env body
    lvar                       <- freshCVar

    return $ CG
      (  "/**Let**/"
      ++ cgenType tybody
      ++ " "
      ++ lvar
      ++ ";\n"
      ++ "{\n"
      ++ decle1
      ++ "\n"
      ++ cgenType type1
      ++ " "
      ++ cgenVar v
      ++ " = "
      ++ ve1
      ++ ";\n"
      ++ declbody
      ++ "\n"
      ++ lvar
      ++ " = "
      ++ vbody
      ++ ";\n"
      ++ "}\n"
      )
      lvar
      tybody

  -- Tuple [t] -> cgenExpr env t -- Don't detuple willy-nilly
  Tuple vs  -> do
    cgvs <- mapM (cgenExprR env) vs
    let cdecls = map getDecl cgvs
    let cexprs = map getExpr cgvs
    let ctypes = map getType cgvs
    let ctype  = CTuple ctypes

    return $ CG (unlines cdecls)
                ("std::make_tuple(" ++ intercalate "," cexprs ++ ")")
                ctype

  Lam (TVar tyv v) body -> do
    lvar <- freshCVar
    let vtype = mkCType tyv
    (CG cdecl cexpr ctype) <- cgenExprR env body
    return $ CG
      (     "/**Lam**/"
      ++    "auto"
      `spc` lvar
      ++    " = [=]("
      ++    cgenType vtype
      `spc` cgenVar v
      ++    ") { "  -- TODO: capture only freeVars here
      ++    cdecl
      ++    "   return ("
      ++    cexpr
      ++    ");"
      ++    "};\n"
      )
      lvar
      (CFunction vtype ctype)


  If c texpr fexpr -> do
    cret              <- freshCVar

    (CG declc vc _  ) <- cgenExprR env c
    (CG declt vt tyt) <- cgenExprR env texpr
    (CG declf vf tyf) <- cgenExprR env fexpr
    let crettype = makeUnionType tyt tyf
    let dotv = case crettype of
          LMVariant _ -> ".v"  -- TODO: Ugh. Fix c++ to not require this.
          _           -> "" -- Ugh.

    return $ CG
      (     declc -- emit condition generation
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
      ++    cret
      ++    dotv
      ++    "= "
      ++    "("
      ++    vt
      ++    ");\n" -- assign to "return"
      ++    "} else {\n" -- else
      ++    "  "
      ++    declf
      ++    ";\n" -- compute false value
      ++    "  "
      ++    cret
      ++    dotv
      ++    "= "
      ++    "("
      ++    vf
      ++    ");\n" -- assign to "return"
      ++    "}\n" -- phew
      )
      cret
      crettype

  Assert cond body -> do
    (CG declcond vcond tycond) <- cgenExprR env cond
    case tycond of CType TypeBool -> return ()
                   _              -> error "tycond was not TypeBool"
    (CG declbody vbody tybody          ) <- cgenExprR env body
    return $ CG (declcond `spc` "KS_ASSERT(" ++ vcond ++ ");\n" ++ declbody)
                vbody
                tybody

  App{} -> error "App"

substitute :: (a -> Maybe [a]) -> [a] -> [a]
substitute f = concatMap (\x -> case f x of Nothing -> [x]; Just s -> s)

mangleFun :: String -> String
mangleFun = substitute $ \case
    '@' -> Just "$a"
    ',' -> Just "$_"
    '[' -> Just "$6"
    ']' -> Just "$9"
    '<' -> Just "$d"
    '>' -> Just "$b"
    '*' -> Just "$x"
    _   -> Nothing

cgenFunId :: FunId -> String
cgenFunId = \case
  UserFun fun -> mangleFun fun
  PrimFun fun -> translateFun fun
  SelFun i _  -> "ks::get<" ++ show (i - 1) ++ ">"
 where
  translateFun :: String -> String
  translateFun = \case
    -- Translating "ts_scale" to "mul" is a shortcut that allows us to
    -- avoid updating the runtime to remove polymorphic "mul" and
    -- replace it with "ts_scale".  At some point we should actually fix
    -- that properly.
    "ts_scale" -> "mul"
    s    -> s

cgenUserFun :: HasCallStack => Fun -> String
cgenUserFun f = case f of
  Fun funId     -> cgenFunId funId
  GradFun  s _  -> "D$" ++ cgenFunId s
  DrvFun   s (AD BasicAD Fwd) -> "fwd$" ++ cgenFunId s
  DrvFun   s (AD BasicAD Rev) -> "rev$" ++ cgenFunId s
  DrvFun   s (AD TupleAD Fwd) -> "fwdt$" ++ cgenFunId s
  DrvFun   s (AD TupleAD Rev) -> "revt$" ++ cgenFunId s

cgenAnyFun :: HasCallStack => TFun -> CType -> String
cgenAnyFun tf cftype = case tf of
  TFun _ (Fun (PrimFun "lmApply")) -> "lmApply"
  TFun ty (Fun (PrimFun "build")) ->
    case ty of
      TypeVec t -> "build<" ++ cgenType (mkCType t) ++ ">"
      _         -> error ("Unexpected type for build: " ++ show ty)
  TFun ty (Fun (PrimFun "sumbuild")) -> -- TODO: remove special case
    "sumbuild<" ++ cgenType (mkCType ty) ++ ">"
  -- This is one of the LM subtypes, e.g. HCat<...>  Name is just HCat<...>::mk
  TFun (TypeLM _ _) (Fun (PrimFun _)) -> cgenType cftype ++ "::mk"
  TFun _            f                 -> cgenUserFun f

cgenType :: HasCallStack => CType -> String
cgenType = \case
  CType  ty -> cgenTypeLang ty
  CTuple ts -> "tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  CFunction s t ->
    "std::function<" ++ cgenType t ++ "(" ++ cgenType s ++ ")>"
  TypeDef s _     -> s
  UseTypeDef s    -> s
  LMZero s t      -> lmt "Zero" [s, t]
  LMOne t         -> lmt "One" [t]
  LMScale t       -> lmt "Scale" [t]
  LMHCat   ts     -> lm "HCat" ts
  LMVCat   ts     -> lm "VCat" ts
  LMBuild  t      -> lm "Build" [t]
  LMCompose m1 m2 -> lm "Compose" [m1, m2]
  LMAdd     ms    -> lm "Add" ms
  LMVariant ts    -> lm "Variant" ts
 where
  lm s ts = "LM::" ++ s ++ "<" ++ intercalate "," (map cgenType ts) ++ ">"
  lmt s ts = "LM::" ++ s ++ "<" ++ intercalate "," (map cgenTypeLang ts) ++ ">"

cgenTypeLang :: HasCallStack => Type -> String
cgenTypeLang = \case
  TypeFloat     -> "double" -- TODO: make these all ks_Float etc, and add typedefs in knossos.h
  TypeInteger   -> "int"
  TypeString    -> "std::string"
  TypeTuple [t] -> cgenTypeLang t
  TypeTuple ts  -> "tuple<" ++ intercalate "," (map cgenTypeLang ts) ++ ">"
  TypeVec t     -> "vec<" ++ cgenTypeLang t ++ ">"
  TypeBool      -> "bool"
  TypeUnknown   -> "void"
  TypeLam from to ->
    "std::function<" ++ cgenTypeLang to ++ "(" ++ cgenTypeLang from ++ ")>"
  TypeLM s t -> error $ "LM<" ++ cgenTypeLang s ++ "," ++ cgenTypeLang t ++ ">"

ctypeofFun :: HasCallStack => CST -> TFun -> [CType] -> CType
ctypeofFun env (TFun ty f) ctys = case cstMaybeLookupFun f env of
  Just _ -> -- trace ("Found fun " ++ show f) $
    UseTypeDef ("ty$" ++ cgenUserFun f)
  Nothing -> -- trace ("Did not find fun " ++ show tf ++ " in\n     " ++ show env) $
    ctypeofFun1 ty f ctys

ctypeofFun1 :: HasCallStack => Type -> Fun -> [CType] -> CType
ctypeofFun1 ty (Fun (PrimFun name)) ctys = ctypeofPrimFun ty name ctys
ctypeofFun1 (TypeLM _ _) (GradFun f _) ctys = ctypeofGradBuiltin f ctys
ctypeofFun1 (TypeLM _ _) f ctys =
  error $ "Did not match [" ++ show f ++ "]@\n  " ++ intercalate
    "\n  "
    (map show ctys)
ctypeofFun1 ty _ _ = mkCType ty

ctypeofPrimFun :: HasCallStack => Type -> String -> [CType] -> CType
ctypeofPrimFun ty s arg_types = case (s, map stripTypeDef arg_types) of
  ("lmApply"  , _         ) -> mkCType ty
  ("lmOne"    , [ct]      ) -> LMOne (stripCType ct)
  ("lmZero"   , [cs, ct]  ) -> LMZero (stripCType cs) (stripCType ct)
  ("lmScale"  , [ct, CType TypeFloat]) -> LMScale (stripCType ct)
  ("lmHCat"   , _         ) -> LMHCat arg_types
  ("lmVCat"   , _         ) -> LMVCat arg_types
  ("lmCompose", [lm1, lm2]) -> LMCompose lm1 lm2
  ("lmAdd"    , _         ) -> LMAdd arg_types
  ("lmVariant", _         ) -> LMVariant arg_types
  _                         -> case ty of
    TypeLM _ _ -> error
      (  "Unmatched prim ["
      ++ s
      ++ "] "
      ++ show ty
      ++ " @\n"
      ++ "  "
      ++ intercalate "\n  " (map show arg_types)
      )
    _ -> mkCType ty

pattern RR :: TypeX
pattern RR = TypeFloat

ctypeofGradBuiltin :: HasCallStack => FunId -> [CType] -> CType
ctypeofGradBuiltin f ctys = case (f, map stripTypeDef ctys) of
  (PrimFun "ts_add"  , [CType RR, CType RR]) -> LMHCat [LMScale RR, LMScale RR]
  (PrimFun "$trace"  , [CType ty]          ) -> LMOne ty
  (PrimFun "size"    , [CType ty]          ) -> LMZero ty TypeInteger
  (PrimFun "index"   , [CType (TypeVec t)])-> trace "LMIndex?" $ LMHCat [LMZero TypeInteger t, LMBuild (LMScale t)]
  _ -> error $ "Don't know grad of [" ++ show f ++ "]@\n  " ++ intercalate
    "\n  "
    (map (show . stripTypeDef) ctys)

cgenKonst :: Konst -> String
cgenKonst = \case
  KInteger i -> show i
  KFloat   f -> show f
  KString  s -> show s
  KBool    b -> if b then "1 /* TRUE */" else "0 /* FALSE */"

cgenVar :: Var -> String
cgenVar = render . ppr

makeDirectoryForFile :: FilePath -> IO ()
makeDirectoryForFile =
  System.Directory.createDirectoryIfMissing True
  . fst
  . System.FilePath.splitFileName

createDirectoryWriteFile :: FilePath -> String -> IO ()
createDirectoryWriteFile filepath contents = do
  makeDirectoryForFile filepath
  writeFile filepath contents

cppGenWithFiles :: String -> String -> [TDef] -> IO ()
cppGenWithFiles ksofile cppfile defs = do
  let lines =
        [
        "#include \"knossos.h\"",
        "namespace ks {\n"
        ]
      lls   = cgenDefs defs
      tail =
        [ "}"
        , "#include \"knossos.cpp\""
        , "// cppGen automatically generates a call to ks::main()."
        , "// Some of the files we want to compile don't have a main."
        , "// The simplest way of allowing them to compile is generate"
        , "// a declaration for ks::main().  We have to choose a return"
        , "// type for it.  int is a reasonable choice.  In the future"
        , "// should try to improve the compile pipeline so it doesn't"
        , "// always insert a call to ks::main()."
        , "namespace ks { int main(); }"
        , "int main() {"
        , "  ks::main();"
        , "  return 0;"
        , "}"
        ]

  putStrLn $ "Writing to " ++ ksofile
  createDirectoryWriteFile ksofile (unlines (map (renderSexp . ppr) defs))

  putStrLn $ "Writing to " ++ cppfile
  createDirectoryWriteFile cppfile (unlines (lines ++ lls ++ tail))

compile :: String -> String -> String -> IO String
compile = compileWithOpts []

compileWithProfiling :: String -> String -> String -> IO String
compileWithProfiling =
  compileWithOpts ["-Wl,--no-as-needed,-lprofiler,--as-needed"]

compileWithOpts :: [String] -> String -> String -> String -> IO String
compileWithOpts opts compilername cppfile exefile = do
  let compcmd =
        ( compilername
        , [ "-fmax-errors=5"
          , "-fdiagnostics-color=always"
          , "-Wall"
          , "-Wno-unused"
          , "-Wno-maybe-uninitialized"
          , "-Isrc/runtime"
          , "-O3"
          , "-g"
          , "-std=c++17"
          , "-o"
          , exefile
          ]
          ++ opts
          ++ [cppfile]
        )
  makeDirectoryForFile exefile
  putStrLn $ "Compiling: " ++ fst compcmd ++ " " ++ unwords (snd compcmd)
  uncurry readProcessPrintStderr compcmd
  return exefile

runExe :: String -> IO String
runExe exefile = do
  putStrLn "Running"
  readProcessPrintStderr exefile []

readProcessEnvPrintStderr
  :: FilePath -> [String] -> Maybe [(String, String)] -> IO String
readProcessEnvPrintStderr executable args env = do
  let stdin = ""
  (exitCode, stdout, stderr) <- System.Process.readCreateProcessWithExitCode
    (System.Process.proc executable args) { System.Process.env = env }
    stdin
  putStr stderr
  when (exitCode /= ExitSuccess) $ error "Compilation failed"
  return stdout

readProcessPrintStderr :: FilePath -> [String] -> IO String
readProcessPrintStderr executable args =
  readProcessEnvPrintStderr executable args Nothing
