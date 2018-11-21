{-# LANGUAGE LambdaCase #-}

module Annotate where

import Lang
import qualified Data.Map   as Map
import Data.List( mapAccumL )
import GHC.Stack
import Debug.Trace( trace )


dbtrace :: String -> a -> a
dbtrace _ e = e -- trace msg e

--------------------------

annotDecls :: ST -> [Decl] -> (ST, [TDecl])
annotDecls env defs = mapAccumL annotDeclX env defs

annotDeclX :: ST -> Decl -> (ST, TDecl)
annotDeclX env (DefDecl (DefX f vars expr))
  = (stInsertFun f ty env, DefDecl (DefX (TFun ty f) vars e))
  where
    body_env = stBindParams env vars
    (ty,e)    = annotExpr body_env expr

annotDeclX env (RuleDecl (Rule { ru_name = name, ru_qvars = qvars
                               , ru_lhs = lhs, ru_rhs = rhs }))
  = assertEqualThen "Rule types" lhs_ty rhs_ty $
    (env, RuleDecl (Rule { ru_name = name, ru_qvars = qvars
                         , ru_lhs = lhs', ru_rhs = rhs' }))
  where
    body_env = stBindParams env qvars
    (lhs_ty, lhs')    = annotExpr body_env lhs
    (rhs_ty, rhs')    = annotExpr body_env rhs

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

  Lam tv@(TVar tyv v) body ->
    let body_env = stInsert v tyv env in
    let (tybody,abody) = annotExpr body_env body in
    (TypeLambda tyv tybody, Lam tv abody)

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

  e -> error $ "Cannot annotate " ++ pps e


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
  Lam tv e -> Lam tv $ stripAnnotExpr e
  App e1 e2 -> App (stripAnnotExpr e1) (stripAnnotExpr e2)
  Let (TVar _ v) e1 e2 -> Let v (stripAnnotExpr e1) (stripAnnotExpr e2)
  If c t f -> If (stripAnnotExpr c) (stripAnnotExpr t) (stripAnnotExpr f)
  Assert c e -> Assert (stripAnnotExpr c) (stripAnnotExpr e)

--------------------------------------
