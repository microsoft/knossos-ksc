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

instance CfreshVar (TVar Var) where  
  freshVar ty = do
                 v <- freshVar @Var ty
                 return $ TVar ty v

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

getLM :: HasCallStack => Type -> TypeLM
getLM (TypeLM ty) = ty
getLM t = error $ "Wanted TypeLM, got " ++ (show $ ppr t)


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
   
typeofLMFun :: HasCallStack => String -> [Type] -> TypeLM
typeofLMFun f ty = case --trace ("typeofLMFun " ++ show f ++ " @ " ++ show ty) 
                        (f, ty) of
  ("lmZero" , [s, t])   -> LMZero s t
  ("lmOne" ,  [t])      -> LMOne t
  ("lmScale", [t, TypeFloat]) -> LMScale t
  ("lmBuild", [TypeInteger, TypeLambda TypeInteger (TypeLM ty)]) -> LMBuild ty
  ("lmBuildT", [TypeInteger, TypeLambda TypeInteger (TypeLM ty)]) -> LMBuildT ty
  ("lmVCat",  lms) -> LMVCat $ map getLM lms 
  ("lmHCat",  lms) -> LMHCat $ map getLM lms 
  ("lmCompose", [TypeLM lm1, TypeLM lm2]) -> LMCompose lm1 lm2
  ("lmAdd",  [TypeLM lm1, TypeLM lm2]) -> LMAdd lm1 lm2
  _ ->
    error -- flip trace (LM TypeUnknown TypeUnknown)
      $  "Failed to type LMfun ("
      ++ show f
      ++ ", "
      ++ show ty
      ++ ")"


  (GradFun (SFun "pr") _ , _                            ) -> TypeInteger
  (GradFun (SFun "index") _, [_, TypeVec t]               ) -> TypeLM $ LMHCat [LMZero TypeInteger t, LMBuildT (LMScale t)]
  (GradFun (SFun "size" )   _ , [TypeVec t]                  ) -> TypeLM $ LMZero (TypeVec t) TypeInteger
  (GradFun (SFun "sum") _, [TypeVec t]                  ) -> TypeLM $ LMBuildT (LMOne t)
  (GradFun (SFun "to_float"  ) _   , [TypeInteger]                  ) -> TypeLM $ LMZero TypeInteger TypeFloat
  (GradFun (SFun "exp"  ) _   , [TypeFloat]                  ) -> TypeLM $ LMScale TypeFloat
  (GradFun (SFun "log"  ) _   , [TypeFloat]                  ) -> TypeLM $ LMScale TypeFloat
  (GradFun (SFun "*") Fwd, [t1, t2]                     ) -> TypeLM $ LMHCat [LMScale t1, LMScale t2]
  (GradFun (SFun "/") Fwd, [t1, t2]                     ) -> TypeLM $ LMHCat [LMScale t1, LMScale t2]
  (GradFun (SFun "-") Fwd, [t1, t2]                     ) -> TypeLM $ LMHCat [LMScale t1, LMScale t2]
  (GradFun (SFun "+") Fwd, [t1, t2]                     ) -> TypeLM $ LMHCat [LMScale t1, LMScale t2]

  -- (GradFun (SFun "*") Fwd, [TypeInteger, t2]                     ) -> TypeLM $ LMZero (TypeTuple [TypeInteger, t2]) TypeInteger
  -- (GradFun (SFun "/") Fwd, [TypeInteger, t2]                     ) -> TypeLM $ LMZero (TypeTuple [TypeInteger, t2]) TypeInteger
  -- (GradFun (SFun "-") Fwd, [TypeInteger, t2]                     ) -> TypeLM $ LMZero (TypeTuple [TypeInteger, t2]) TypeInteger
  -- (GradFun (SFun "+") Fwd, [TypeInteger, t2]                     ) -> TypeLM $ LMZero (TypeTuple [TypeInteger, t2]) TypeInteger

  --fixme(GradFun (SelFun i _) _, [TypeTuple tys]) ->    TypeLM (TypeTuple tys) (tys !! (i - 1))
  --(GradFun (SelFun{}) _, [TypeVec t]) -> TypeLM (TypeVec t) t
  --(GradFun (SelFun{}) _, _          ) -> TypeUnknown
  -- (GradFun (SFun f) _, [tfrom]) -> let ty = stLookup "GradFun" (Simple f) env in TypeLM tfrom ty
  -- (GradFun (SFun f) _, t : tys) ->
  --   let tfrom = TypeTuple (t : tys)
  --   in  let ty = stLookup "GradFun2" (Simple f) env in TypeLM tfrom ty



------------ Administrative normal form

anf :: (TypeableFun f, Typeable b,  CfreshVar b) => ExprX f b -> M (ExprX f b)
anf = \case
  Call f (Tuple ts) -> do
    anfArgs <- mapM letAnf ts
    return $ foldr
      (.)
      id
      (map fst anfArgs)
      $ Call f (Tuple (map snd anfArgs))

  Call f e -> do
    (lete, ve) <- letAnf e
    return $ lete $ Call f ve

  Tuple ts -> do
    anfArgs <- mapM letAnf ts
    return $ foldr (.) id (map fst anfArgs) $
           Tuple (map snd anfArgs)
  
  Lam x ty e -> do
    anfe <- anf e
    return $ Lam x ty anfe
  
  App f x -> do
    (letf, vf) <- letAnf f
    (letx, vx) <- letAnf x
    return $ letf $ letx $ App vf vx
  
  Let x e body -> do
    anfe    <- anf e
    anfbody <- anf body
    return $ Let x anfe anfbody
    
  If cond ift iff -> do
    anfcond <- anf cond
    anfift  <- anf ift
    anfiff  <- anf iff
    return $ If anfcond anfift anfiff

  Assert cond body -> do
    anfcond <- anf cond
    anfbody <- anf body
    return $ Assert anfcond anfbody

  ex -> return ex

-- letAnf of (+ a b) is (\body -> let c17 = (+ a b) in body, c17)
letAnf :: (TypeableFun f, Typeable b, CfreshVar b) => ExprX f b -> M (ExprX f b -> ExprX f b, ExprX f b)
letAnf e = do
  ve   <- freshVar (typeof e)
  anfe <- anf e
  return
    ( \body -> Let ve anfe body
    , Var ve
    )

anfDefs :: (TypeableFun f, Typeable b, CfreshVar b) => [DefX f b] -> [DefX f b]
anfDefs = map anfDef

anfDef :: (TypeableFun f, Typeable b, CfreshVar b) => DefX f b -> DefX f b
anfDef (DefX f vars expr) =
  DefX f vars $ runM $ anf expr

-------------------- Cgen

-- CGenResult is (C declaration, C expression, Type)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r")
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0")      -- this is what we use at the occurrence
type CGenResult = (String, String)

cgenDefs :: [TDef] -> [String]
cgenDefs = map cgenDef

cgenDef :: TDef -> String
cgenDef def = fst $ cgenDefE def

cgenDefE :: TDef -> CGenResult
cgenDefE (DefX f@(TFun tyf _) vars expr) =
  let (cDecl, cVar) = runM $ cgenExpr expr
      tyfstr = cgenType tyf
  in  (     tyfstr
      `spc` cgenFun f
      ++    "("
      ++    intercalate
              ", "
              (map (\(TVar ty var) -> cgenType ty `spc` cgenVar var) vars)
      ++    ") {\n"
      ++    cDecl
      ++    "return static_cast<" ++ tyfstr ++ ">"  -- In order to concretize tag types like zero_t
      ++    "(" ++ cVar ++ ")"
      ++    ";\n}\n"
      , cgenFun f
      )

cgenExpr :: TExpr -> M CGenResult
cgenExpr = cgenExprR -- <=< anf

cgenExprR :: HasCallStack => TExpr -> M CGenResult
cgenExprR ex = case ex of
  Konst k             -> return ("", cgenKonst k)

  Var v    -> return ("", cgenTVar v)

  Call tf@(TFun ty f) x -> case x of
    Tuple vs -> do
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

  Let (TVar tyv v) e1 body -> do
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

  Tuple [t] -> cgenExpr t
  Tuple vs  -> do
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

  Lam (TVar tv v) ty1 body -> do
    let _ = assertEqual "CGLam" tv ty1
    l        <- freshCVar
    (cE, vE) <- cgenExprR body
    return
      ( "/**Lam**/"
      ++ "auto" -- cgenType (TypeLambda tv (typeof body)) 
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

  If c texpr fexpr -> do
    cret        <- freshCVar

    (declc, vc) <- cgenExprR c
    (declt, vt) <- cgenExprR texpr
    (declf, vf) <- cgenExprR fexpr
    let ty1 = typeof texpr
    let ty2 = typeof fexpr
    let ty = makeUnionType ty1 ty2
    let dotv = case ty of
                TypeLM (LMVariant ts) -> ".v"
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

  Assert cond body -> do
    (declcond, vcond) <- cgenExprR cond
    (declbody, vbody) <- cgenExprR body
    return (declcond `spc` "ASSERT(" ++ vcond ++ ");\n" ++ declbody, vbody)
  App{} -> error "App"

cgenFunId :: FunId -> String
cgenFunId = \case
  SFun fun   -> translateFun fun
  SelFun i n -> "selfun$" ++ show n ++ "_" ++ show i

cgenFun :: HasCallStack => TFun -> String
cgenFun tf@(TFun ty f) = case f of
  Fun (SFun "build") -> "build<"++ cgenType t ++ ">" where (TypeVec t) = ty
  Fun funId       -> cgenFunId funId
  GradFun s Fwd -> "D$" ++ cgenFunId s
  GradFun s Rev -> "R$" ++ cgenFunId s
  DrvFun  s Fwd -> "fwd$" ++ cgenFunId s
  DrvFun  s Rev -> "rev$" ++ cgenFunId s
  LMFun s         -> case ty of
    TypeLM ty -> cgenTypeLM ty ++ "::" ++ "mk"
    t -> "LM::/* " ++ show t ++ "*/" ++ s

cgenTypeOf :: TExpr -> String
cgenTypeOf = cgenType . typeof

cgenType :: Type -> String
cgenType = \case
  TypeZero      -> "zero_t"
  TypeFloat     -> "double"
  TypeInteger   -> "int"
  TypeTuple [t] -> cgenType t
  TypeTuple ts  -> "tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  TypeVec   t   -> "vec<" ++ cgenType t ++ ">"
  TypeBool      -> "bool"
  TypeUnknown   -> "unk"
  TypeLambda from to ->
    "std::function<" ++ cgenType to ++ "(" ++ cgenType from ++ ")>"
  TypeLM ty -> cgenTypeLM ty

cgenTypeLM :: HasCallStack => TypeLM -> String
cgenTypeLM = \case
  LM s t -> "LM::Base" ++ angle s t
  LMZero s t -> "LM::Zero" ++ angle s t
  LMOne t-> "LM::One" ++ angle1 t
  LMScale t-> "LM::Scale" ++ angle1 t
  LMSelFun s t -> "LM::SelFun" ++ angle s t
  LMTranspose lm-> "LM::Transpose<" ++ cgenTypeLM lm ++ ">"
  LMCompose bc ab-> "LM::Compose<" ++ cgenTypeLM bc ++ "," ++ cgenTypeLM ab ++ ">"
  LMAdd a b-> "LM::Add<" ++ cgenTypeLM a ++ "," ++ cgenTypeLM b ++ ">"
  LMVCat lms-> "LM::VCat<" ++ intercalate "," (map cgenTypeLM lms) ++ ">"
  LMHCat lms-> "LM::HCat" ++ "<" ++ intercalate "," (map cgenTypeLM lms) ++ ">"
  LMBuild lm-> "LM::Build<" ++ cgenTypeLM lm ++ ">"
  LMBuildT lm-> "LM::BuildT<" ++ cgenTypeLM lm ++ ">"
  LMVariant lms-> "LM::Variant<" ++ intercalate "," (map cgenTypeLM lms) ++ ">"
  where
   angle s t = "<" ++ cgenType s ++ "," ++ cgenType t ++ ">"
   angle1 t = "<" ++ cgenType t ++ ">"

cgenKonst :: Konst -> String
cgenKonst = \case
  KZero      -> "0"
  KInteger i -> show i
  KFloat   f -> show f
  KBool    b -> if b then "TRUE" else "FALSE"

cgenVar :: Var -> String
cgenVar v = show v

cgenTVar :: TVar Var -> String
cgenTVar (TVar ty Dummy) = cgenType ty ++ "{}"
cgenTVar (TVar _ v) = show v


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
