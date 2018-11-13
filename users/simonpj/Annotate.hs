{-# LANGUAGE LambdaCase #-}

module Annotate where

import GHC.Stack
import Debug.Trace                    ( trace )

import qualified Data.Map                      as Map

import           Lang

dbtrace :: String -> a -> a
dbtrace _ e = e -- trace msg e

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

--------------------------------------
