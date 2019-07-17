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
import qualified Data.Set                      as Set
import           Control.Monad                  ( when )
import qualified Control.Monad.State           as S
import qualified System.Directory
import qualified System.FilePath
import qualified System.Process
import           System.Exit                    ( ExitCode(ExitSuccess) )

import           Lang

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

getDecl :: CGenResult -> String
getDecl (CG dc _ _) = dc

getExpr :: CGenResult -> String
getExpr (CG _ ex _) = ex

getType :: CGenResult -> CType
getType (CG _ _ ty) = ty

type CST = Map.Map String CType

cstEmpty :: CST
cstEmpty = Map.empty

cstMaybeLookup0 :: HasCallStack => String -> CST -> Maybe CType
cstMaybeLookup0 s env = Map.lookup s env

cstLookup0 :: HasCallStack => String -> CST -> CType
cstLookup0 v env = case cstMaybeLookup0 v env of
  Just e -> e
  Nothing ->
    error
      $  "Failed lookup ["
      ++ v
      ++ "] in\n"
      ++ unlines (map show (Map.toList env))

cstInsertVar :: Var -> CType -> CST -> CST
cstInsertVar v ty env = -- trace ("cstInsertVar [" ++ show v ++ "] = [" ++ show ty ++ "]\n" ++ show env) $
  Map.insert (show v) ty env

cstInsertFun :: Fun -> CType -> CST -> CST
cstInsertFun f ctype env = -- trace ("cstInsertFun " ++ show f ++ " = " ++ show ctype ++ "\n" ++ show env) $
  Map.insert (show f) ctype env

cstLookupVar :: HasCallStack => Var -> CST -> CType
cstLookupVar v env = cstLookup0 (show v) env

cstLookupFun :: HasCallStack => Fun -> CST -> CType
cstLookupFun f env = cstLookup0 (show f) env

cstMaybeLookupFun :: HasCallStack => Fun -> CST -> Maybe CType
cstMaybeLookupFun f env = cstMaybeLookup0 (show f) env

cComment :: String -> String
cComment s = "/* " ++ s ++ " */"

cgenDefs :: [TDef] -> [String]
cgenDefs defs = snd $ foldl go (cstEmpty, []) $
                filter isUserDef defs
 where
  go :: (CST, [String]) -> TDef -> (CST, [String])
  go (env, strs) def@(Def { def_fun = f }) =
    let env' = cstInsertFun f (UseTypeDef ("ty$" ++ cgenUserFun f)) env
        (CG cdecl _cfun _ctype) = cgenDefE env' def
    in  (env', strs ++ [cdecl])

cgenDefE :: CST -> TDef -> CGenResult
cgenDefE env (Def { def_fun = f, def_args = params
                  , def_rhs = UserRhs body }) =
  let addParam env (TVar ty v) = cstInsertVar v (mkCType ty) env
      env' = foldl addParam env params

      vecsizes = vecSizeDecls params

      CG cbodydecl cbodyexpr cbodytype = runM $ cgenExpr env' body
      cf                               = cgenUserFun f

      mkVar (TVar ty var) = cgenType (mkCType ty) `spc` cgenVar var
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
        ++    vecsizes ++ "\n"
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


-- A decl of the form v : Vec n (Tuple (String, Vec m (Vec n Float)))
-- should cg to a prelude in the C function like the following
--   int n = size(v);
--   assert(n>0);
--   int m = size(get<2>(get_an_element(v)));
--   assert(n == get_an_element(get<2>(get_an_element(v))))

vecSizeDecls :: [TVar] -> String
vecSizeDecls vs = goVars (Set.fromList $ map tVarVar vs) vs
  where
    goVars :: Set.Set Var -> [TVar] -> String
    goVars _ [] = ""
    goVars seen (TVar ty v:vs) = let (seen',str) = goType seen (cgenVar v) ty
                                 in str ++ goVars seen' vs

    goVec :: Set.Set Var -> String -> Var -> (Set.Set Var, String)
    goVec seen value sz =
        if sz `Set.member` seen then
          (seen, "KS_ASSERT(" ++ cgenVar sz ++ " == size(" ++ value ++ "));\n")
        else
          (Set.insert sz seen, "/*" ++ show seen ++ "*/\n" ++ "int " ++ cgenVar sz ++ " = size(" ++ value ++ ");\n")

    goType :: Set.Set Var -> String -> Type -> (Set.Set Var, String)
    goType seen value (TypeVec (Var (TVar TypeSize sz)) ty) =
        let (seen',str) = goVec seen value sz
        in accum str $ goType seen' (get_element value) ty

    goType seen value (TypeTuple tys) =
        foldl (\ (seen,str) (ty,n) -> accum (str ++ "/*tup*/") $ goType seen (get value n) ty)
              (seen, "")
              (zip tys [0..])

    goType seen value _ = (seen, "/*" ++ value ++ "*/\n")

    get_element v = "(" ++ v ++ ")[0]"
    get v n = "std::get<" ++ show n ++ ">(" ++ v ++ ")"

    accum str (seen,str') = (seen, str ++ str')

cgenExpr :: CST -> TExpr -> M CGenResult
cgenExpr = cgenExprR

cgenExprR :: HasCallStack => CST -> TExpr -> M CGenResult
cgenExprR env = \case
  Konst k -> return $ CG "" (cgenKonst k) (mkCType $ typeofKonst k)
  Var (TVar ty Dummy) ->
    let cty = mkCType ty in return $ CG "" (cgenType cty ++ "{}") cty
  Var (TVar _ v)                -> return $ CG "" (cgenVar v) (cstLookupVar v env)

  -- Special case for build -- inline the loop
  Call (TFun (TypeVec sty ty) (Fun (PrimFun "build"))) [sz, Lam (TVar vty var) body] -> do
    CG szdecl szex _szty <- cgenExprR env sz
    let varty = mkCType vty
    let varcty = cgenType varty
    CG bodydecl bodyex _bodyty <- cgenExprR (cstInsertVar var varty env) body

    ret  <- freshCVar
    cvar <- freshCVar

    return $ CG
        (  "/*build*/\n"
        ++ szdecl
        ++ "vec<" ++ (cgenType $ mkCType ty) ++ "> " ++ ret ++ "(" ++ szex ++ ");\n"
        ++ "for(" ++ varcty ++ " " ++ cvar ++ " = 0;"
                  ++ cvar ++ " < " ++ szex ++ ";"
                  ++ " ++" ++ cvar ++ ") {\n"
        ++ (case var of
              Dummy -> ""
              _ -> "   " ++ varcty ++ " " ++ cgenVar var ++ " = " ++ cvar ++ ";\n" )
        ++ "   " ++ bodydecl ++ "\n"
        ++ "   " ++ ret ++ "[" ++ cvar ++ "] = " ++ bodyex ++ ";\n"
        ++ "}\n"
        )
        ret
        (mkCType (TypeVec sty ty))

  -- Special case for sumbuild -- inline the loop
  Call (TFun ty (Fun (PrimFun "sumbuild"))) [sz, Lam (TVar vty var@(Simple _)) body] -> do
    CG szdecl szex _szty <- cgenExprR env sz
    let varty = mkCType vty
    let varcty = cgenType varty
    CG bodydecl bodyex _bodyty <- cgenExprR (cstInsertVar var varty env) body

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
        --       First time round, inflate it, put it in the ret, then mark the allocator
        ++ "     if (" ++ cgenVar var ++ " == 0) {\n"
        ++ "       " ++ ret ++ " = inflate(" ++ bodyex ++ ");\n"
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




  Call tf@(TFun _ _) vs -> do
      -- Untuple argument for C++ call
    cgvs <- mapM (cgenExprR env) vs
    let cdecls = map getDecl cgvs
    let cexprs = map getExpr cgvs
    let ctypes = map getType cgvs

    let cftype = ctypeofFun env tf ctypes

    v        <- freshCVar
    bumpmark <- freshCVar

    let dogc = Cgen.isScalar cftype
    let gc tag =  if dogc
                    then tag ++ "(" ++ bumpmark ++ ");\n"
                    else ""

    let cf = cgenAnyFun tf cftype ctypes

    return $ CG
      (  unlines cdecls
      ++ gc "$MRK"
      ++ cgenType cftype ++ " " ++ v ++ " = "
      ++ cf ++ "(" ++ intercalate "," cexprs ++ ");\n"
      ++ gc "$REL"
      )
      v
      cftype

  Let (TVar _ v) e1 body -> do
    (CG decle1   ve1   type1 ) <- cgenExprR env e1
    (CG declbody vbody tybody) <- cgenExprR (cstInsertVar v type1 env) body
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
    (CG cdecl cexpr ctype) <- cgenExprR (cstInsertVar v vtype env) body
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
    '*' -> Just "$x"
    _   -> Nothing

cgenFunId :: FunId -> String
cgenFunId = \case
  UserFun fun -> mangleFun fun
  PrimFun fun -> translateFun fun
  SelFun i _  -> "ks_get<" ++ show (i - 1) ++ ">"  -- TODO: rename to ks::get
 where
  translateFun :: String -> String
  translateFun = \case
    "*"  -> "mul"
    "+"  -> "add"
    "/"  -> "div"
    "-"  -> "sub"
    "==" -> "eq"
    "<"  -> "lt"
    ">"  -> "gt"
    "<=" -> "lte"
    ">=" -> "gte"
    "or" -> "or_"
    "and"-> "and_"
    s    -> s

cgenUserFun :: HasCallStack => Fun -> String
cgenUserFun f = case f of
  Fun funId     -> cgenFunId funId
  GradFun  s _  -> "D$" ++ cgenFunId s
  DrvFun   s (AD BasicAD Fwd) -> "fwd$" ++ cgenFunId s
  DrvFun   s (AD BasicAD Rev) -> "rev$" ++ cgenFunId s
  DrvFun   s (AD TupleAD Fwd) -> "fwdt$" ++ cgenFunId s
  DrvFun   s (AD TupleAD Rev) -> "revt$" ++ cgenFunId s

cgenAnyFun :: HasCallStack => TFun -> CType -> [CType] -> String
cgenAnyFun tf cftype ctypes = case tf of
  TFun _ (Fun (PrimFun "lmApply")) -> "lmApply"
  TFun ty (Fun (PrimFun "build")) ->
    let TypeVec _ t = ty in "build<" ++ cgenType (mkCType t) ++ ">"
  TFun ty (Fun (PrimFun "sumbuild")) ->
    "sumbuild<" ++ cgenType (mkCType ty) ++ ">"
  -- This is one of the LM subtypes, e.g. HCat<...>  Name is just HCat<...>::mk
  TFun (TypeLM _ _) (Fun (PrimFun _)) -> cgenType cftype ++ "::mk"
  TFun _ (Fun (PrimFun "to_tangent")) ->
    case ctypes of
      [ctype] -> "to_tangent<" ++ cgenType cftype ++ "," ++ cgenType ctype ++ ">"
      _       -> error "Expected one argument to to_tangent"
  TFun _            f                 -> cgenUserFun f

cgenTypeOf :: TExpr -> String
cgenTypeOf = cgenType . mkCType . typeof

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
  TypeFloat     -> "double"
  TypeInteger   -> "int"
  TypeString    -> "std::string"
  TypeTuple [t] -> cgenTypeLang t
  TypeTuple ts  -> "tuple<" ++ intercalate "," (map cgenTypeLang ts) ++ ">"
  TypeVec _ t   -> "vec<" ++ cgenTypeLang t ++ ">"
  TypeBool      -> "bool"
  TypeUnknown   -> "void"
  TypeLam from to ->
    "std::function<" ++ cgenTypeLang to ++ "(" ++ cgenTypeLang from ++ ")>"
  TypeLM s t -> error $ "LM<" ++ cgenTypeLang s ++ "," ++ cgenTypeLang t ++ ">"

ctypeofFun :: HasCallStack => CST -> TFun -> [CType] -> CType
ctypeofFun env (TFun ty f) ctys = case cstMaybeLookupFun f env of
  Just ctype -> -- trace ("Found fun " ++ show f) $
    ctype
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
  ("lmBuild"  , [_, lam]  ) -> LMBuild lam
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

pattern RR = TypeFloat
pattern VecR <- TypeVec _ TypeFloat

ctypeofGradBuiltin :: HasCallStack => FunId -> [CType] -> CType
ctypeofGradBuiltin f ctys = case (f, map stripTypeDef ctys) of
  (PrimFun "-"       , [CType RR, CType RR]) -> LMHCat [LMScale RR, LMScale RR]
  (PrimFun "+"       , [CType RR, CType RR]) -> LMHCat [LMScale RR, LMScale RR]
  (PrimFun "/"       , [CType RR, CType RR]) -> LMHCat [LMScale RR, LMScale RR]
  (PrimFun "*"       , [CType RR, CType RR]) -> LMHCat [LMScale RR, LMScale RR]
  (PrimFun "to_float", [CType TypeInteger] ) -> LMZero TypeInteger TypeFloat
  (PrimFun "$trace"  , [CType ty]          ) -> LMOne ty
  (PrimFun "$rand"   , [CType ty]          ) -> trace "GRADRAND?" $ LMZero ty ty -- make this noisy -- the type does't look right
  (PrimFun "$ranhashdoub", [CType ty]      ) -> trace "GRADRAND?" $ LMZero ty ty -- make this noisy -- the type does't look right
  (PrimFun "size"    , [CType ty]          ) -> LMZero ty TypeInteger
  (PrimFun "index"   , [CType (TypeVec _ t)])-> trace "LMIndex?" $ LMHCat [LMZero TypeInteger t, LMBuild (LMScale t)]
  _ -> error $ "Don't know grad of [" ++ show f ++ "]@\n  " ++ intercalate
    "\n  "
    (map (show . stripTypeDef) ctys)

cgenKonst :: Konst -> String
cgenKonst = \case
  KInteger i -> show i
  KSize i    -> show i
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

cppGen :: String -> [TDef] -> IO (String, String)
cppGen outfile defs = do
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
      ksofile = outfile ++ ".kso"
      cppfile = outfile ++ ".cpp"

  putStrLn $ "Writing to " ++ ksofile
  createDirectoryWriteFile ksofile (intercalate "\n" (map (renderSexp . ppr) defs))

  putStrLn $ "Writing to " ++ cppfile
  createDirectoryWriteFile cppfile (unlines (lines ++ lls ++ tail))

  return (ksofile, cppfile)

cppGenAndCompile
  :: (String -> String -> IO String) -> String -> [TDef] -> IO String
cppGenAndCompile compiler outfile defs = do
  (_, cppfile) <- cppGen outfile defs
  let exefile = outfile ++ ".exe"
  --putStrLn $ "Formatting " ++ cppfile
  --callCommand $ "clang-format -i " ++ cppfile
  compiler cppfile exefile

compile :: String -> String -> String -> IO String
compile = compileWithOpts []

compileWithProfiling :: String -> String -> String -> IO String
compileWithProfiling =
  compileWithOpts ["-Wl,--no-as-needed,-lprofiler,--as-needed"]

compileWithOpts :: [String] -> String -> String -> String -> IO String
compileWithOpts opts compiler cppfile exefile = do
  let compcmd =
        ( compiler
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

cppFG :: String -> String -> [TDef] -> IO String
cppFG compiler outfile defs = do
  exefile <- cppGenAndCompile (compile compiler) outfile defs
  runExe exefile

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

cppF :: String -> [TDef] -> IO ()
-- String is the file name
cppF outfile defs = do
  output <- cppFG "g++-7" outfile defs
  putStrLn "Done"
  putStr output
