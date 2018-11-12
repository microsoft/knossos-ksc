{-# LANGUAGE LambdaCase #-}

module Annotate where

import GHC.Stack
import Debug.Trace                    ( trace )

import qualified Data.Map                      as Map

import           Lang

dbtrace :: String -> a -> a
dbtrace _ e = e -- trace msg e

------------------ ST (SymTab) ------------------

type ST = Map.Map Var Type

stCreate :: ST
stCreate = Map.empty

stInsert :: Var -> Type -> ST -> ST
stInsert v ty env = dbtrace
  ("Inserting " ++ show v ++ " = " ++ show ty ++ " in " ++ show env ++ "\n")
  (Map.insert v ty env)

stInsertFun :: Fun -> Type -> ST -> ST
stInsertFun f = stInsert (Simple $ show f)

stLookup :: String -> Var -> ST -> Type
stLookup msg v env = case Map.lookup v env of
  Just a  -> a
  Nothing -> trace
    ("Couldn't find " ++ show v ++ " in " ++ msg ++ ", env = " ++ show env)
    TypeUnknown

stLookupFun :: String -> Fun -> ST -> Type
stLookupFun msg (Fun (SFun v)) = stLookup msg (Simple v)
stLookupFun msg unexpected =
  error $ "Unexpected stLookupFun: " ++ show unexpected ++ ": " ++ msg

--------------------------

annotDef :: Def -> TDef
annotDef = annotDefE stCreate

annotDefs :: [TDef] -> [Def] -> [TDef]
annotDefs predefs defs =
  let env = foldl add_predef stCreate predefs in
  reverse $ snd $ foldl accum (env, []) defs
 where
  accum :: (ST, [TDef]) -> Def -> (ST, [TDef])
  accum (env, tdefs) def =
    let tdef@(DefX (TFun tf f) _ _) = annotDefE env $ dbg env def in
    (stInsertFun f tf env, tdef : tdefs)

  add_predef :: ST -> TDef -> ST
  add_predef env (DefX (TFun ty f) _ _) = stInsertFun f ty env

  dbg :: ST -> Def -> Def
  dbg env def = -- returns def
                dbtrace
    ("Passing ENV[" ++ show env ++ "]" ++ " to " ++ (show $ ppr def) ++ "\n")
    def

annotDefE :: ST -> Def -> TDef
annotDefE env (DefX f vars expr) =
  let _ = trace ("Def " ++ show f ++ "\n") ()
  in  let body_env = foldr addVarToEnv env vars
      in  let (ty,e) = annotExpr body_env expr
          in  DefX (TFun ty f) vars e
 where
  addVarToEnv :: TVar Var -> ST -> ST
  addVarToEnv (TVar ty v) = stInsert v ty

annotExpr :: ST -> Expr -> (Type, TExpr)
annotExpr env = \case
  -- Naming conventions in this function:
  --  e   Expr [TypeUnkown]
  --  ae  Expr [Annotated]
  --  tye Type of e
  --  xe  ExprX part of e
  Var   v -> let ty = stLookup "Var" v env in
             (ty, Var $ TVar ty v)

  Konst k -> (typeofKonst k, Konst k)

  Call f es ->
    let (tyes,aes) = annotExpr env es in
    let stty = stLookupFun "Call" f env in
    let ty = typeofFunTy env f tyes in
    (ty, Call (TFun ty f) aes)

  Let v e1 body ->
    let (tyv,ae1) = annotExpr env e1 in
    let body_env = stInsert v tyv env in
    let (tybody,abody) = annotExpr body_env body in
    (tybody, Let (TVar tyv v) ae1 abody)

  Tuple es ->
    let (tys,aes) = unzip $ map (annotExpr env) es in
    (TypeTuple tys, Tuple aes)

  Lam v tyv body ->
    let body_env = stInsert v tyv env in
    let (tybody,abody) = annotExpr body_env body in
    (TypeLambda tyv tybody, Lam (TVar tyv v) tyv abody)

  If cond texpr fexpr ->
    let (tycond,acond) = annotExpr env cond
        (tytexpr,atexpr) = annotExpr env texpr
        (tyfexpr,afexpr) = annotExpr env fexpr in
    assertEqualThen "tycond" tycond TypeBool $
    assertEqualThen "ttexpr" tytexpr tyfexpr $
    (tytexpr, If acond atexpr afexpr)

  Assert cond body ->
    let (tycond,acond) = annotExpr env cond 
        (tybody,abody) = annotExpr env body in
    assertEqualThen "Assert" tycond TypeBool $
    (tybody, Assert acond abody)

  e -> error $ "Cannot annotate " ++ (show $ ppr e)


-- A single place for "domain knowledge" about polymorphic functions -- to be pruned when we get primdefs
typeofFunTy :: HasCallStack => ST -> Fun -> Type -> Type
typeofFunTy env f (TypeTuple tys) = typeofFunTys env f tys
typeofFunTy env f ty              = typeofFunTys env f [ty]

typeofFunTys :: HasCallStack => ST -> Fun -> [Type] -> Type
typeofFunTys env tf ttys = case (tf, ttys) of
  (Fun (SFun "pr")       , _                            ) -> TypeUnknown
  (GradFun (SFun "pr") _ , _                            ) -> TypeUnknown
  (Fun (SFun "build")    , [_, TypeLambda TypeInteger t]) -> TypeVec t
  (Fun (SFun "index")    , [_, TypeVec t]               ) -> t
  (Fun (SFun "size" )    , [TypeVec _]                  ) -> TypeInteger
  (Fun (SFun "sum"  )    , [TypeVec t]                  ) -> t

  (Fun (SFun "to_float") , [TypeInteger]                ) -> TypeFloat
  (Fun (SFun "exp"  )    , [TypeFloat]                  ) -> TypeFloat
  (Fun (SFun "log"  )    , [TypeFloat]                  ) -> TypeFloat
  (Fun (SFun "+"    )    , [t1, t2]                     ) -> assertEqualThen emsg t1 t2 $ t1
  (Fun (SFun "/"    )    , [t1, t2]                     ) -> assertEqualThen emsg t1 t2 $ t1
  (Fun (SFun "*"    )    , [t1, t2]                     ) -> assertEqualThen emsg t1 t2 $ t1
  (Fun (SFun "-"    )    , [t1, t2]                     ) -> assertEqualThen emsg t1 t2 $ t1

  (Fun (SFun "=="   )    , _                            ) -> TypeBool
  (Fun (SFun "!="   )    , _                            ) -> TypeBool
  (Fun (SFun "<"    )    , _                            ) -> TypeBool
  (Fun (SFun ">"    )    , _                            ) -> TypeBool

  (Fun (SFun "delta")    , [TypeInteger, TypeInteger, t]) -> t

  (Fun (SelFun i _  )    , [TypeTuple tys]              ) -> tys !! (i - 1)
  (GradFun (SelFun i _) _, [TypeTuple tys]) ->
    TypeLM (TypeTuple tys) (tys !! (i - 1))

  (Fun (SelFun{})      , [TypeVec t]) -> t
  (GradFun (SelFun{}) _, [TypeVec t]) -> TypeLM (TypeVec t) t
  (GradFun (SelFun{}) _, _          ) -> TypeUnknown

  (Fun (SFun f)        , _          ) -> case Map.lookup (Simple f) env of
    Just a  -> a
    Nothing -> error emsg

  (LMFun f, tys) -> typeofLMFun f tys
  (GradFun (SFun f) _, [tfrom]) ->
    let ty = stLookup "GradFun" (Simple f) env in TypeLM tfrom ty
  (GradFun (SFun f) _, t : tys) ->
    let tfrom = TypeTuple (t : tys)
    in  let ty = stLookup "GradFun2" (Simple f) env in TypeLM tfrom ty
  _ -> trace (error emsg) TypeUnknown
  where emsg = "EFailed to type ("
                ++ show tf
                ++ ", "
                ++ show ttys
                ++ "), env"
                ++ show env


typeofLMFun :: String -> [Type] -> Type
typeofLMFun f tys = case (f, tys) of
  ("lmOne"  , [t]   ) -> TypeLM t t
  ("lmZero" , [a, b]) -> TypeLM a b
  ("lmScale", [t]   ) -> TypeLM t t
  ("lmBuild", [TypeInteger, TypeLambda TypeInteger (TypeLM a b)]) ->
    TypeLM a (TypeVec b)
  ("lmBuildT", [TypeInteger, TypeLambda TypeInteger (TypeLM a b)]) ->
    TypeLM (TypeVec a) b
  ("lmVCat", [TypeLM a b, TypeLM a1 c]) ->
    assertEqualThen "lmVCat" a a1 $ TypeLM a (TypeTuple [b, c])
  ("lmHCat", [TypeLM a c, TypeLM b c1]) ->
    assertEqualThen "lmHCat" c c1 $ TypeLM (TypeTuple [a, b]) c
  ("lmCompose", [TypeLM b c, TypeLM a b1]) ->
    assertEqualThen "lmCompose" b b1 $ TypeLM a c
  ("lmApply", [TypeLM a b, c]) -> assertEqualThen
    ("lmApply LM " ++ show a ++ " -o " ++ show b ++ " * " ++ show c)
    a
    c
    b
  _ ->
    flip trace TypeUnknown
      $  "Failed to type LMfun ("
      ++ show f
      ++ ", "
      ++ show tys
      ++ ") -> ?"


--------------------------------------

stripAnnots :: [TDef] -> [Def]
stripAnnots = map stripAnnot

stripAnnot :: TDef -> Def
stripAnnot (DefX (TFun ty f) tvars texpr) =
  DefX f tvars (stripAnnotExpr texpr)

stripAnnotExpr :: TExpr -> Expr
stripAnnotExpr = \case
  Konst k -> Konst k
  Var (TVar _ v) -> Var v
  Call (TFun _ f) e -> Call f $ stripAnnotExpr e
  Tuple es -> Tuple $ map stripAnnotExpr es
  Lam (TVar _ v) ty e -> Lam v ty $ stripAnnotExpr e
  App e1 e2 -> App (stripAnnotExpr e1) (stripAnnotExpr e2)
  Let (TVar _ v) e1 e2 -> Let v (stripAnnotExpr e1) (stripAnnotExpr e2)
  If c t f -> If (stripAnnotExpr c) (stripAnnotExpr t) (stripAnnotExpr f)
  Assert c e -> Assert (stripAnnotExpr c) (stripAnnotExpr e)
