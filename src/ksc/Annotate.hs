{-# LANGUAGE LambdaCase #-}

module Annotate (
  annotDecls,
  
  GblSymTab, extendGblSymTab, emptyGST,
  
  callResultTy
  
  ) where

import Lang
import Prim
import qualified Data.Map   as Map
import Data.List( mapAccumL )
import GHC.Stack
import Debug.Trace( trace )
import Text.PrettyPrint

dbtrace :: String -> a -> a
dbtrace _ e = e -- trace msg e

--------------------------

annotDecls :: GblSymTab -> [Decl] -> (GblSymTab, [TDecl])
annotDecls env defs = mapAccumL annotDeclX env defs

annotDeclX :: GblSymTab -> Decl -> (GblSymTab, TDecl)
annotDeclX env (DefDecl (DefX f vars expr))
  = (stInsertFun f ty env, DefDecl (DefX (TFun ty f) vars e))
  where
    body_env = stBindParams env vars
    (ty,e)   = annotExpr body_env expr

annotDeclX env (RuleDecl (Rule { ru_name = name, ru_qvars = qvars
                               , ru_lhs = lhs, ru_rhs = rhs }))
  = assertEqualThen "Rule types" lhs_ty rhs_ty $
    (env, RuleDecl (Rule { ru_name = name, ru_qvars = qvars
                         , ru_lhs = lhs', ru_rhs = rhs' }))
  where
    body_env = stBindParams env qvars
    (lhs_ty, lhs') = annotExpr body_env lhs
    (rhs_ty, rhs') = annotExpr body_env rhs

annotExpr :: SymTab -> Expr -> (Type, TExpr)
annotExpr env = \case
  -- Naming conventions in this function:
  --  e   Expr [TypeUnknown]
  --  ae  Expr [Annotated]
  --  tye Type of e
  --  xe  ExprX part of e
  Var   v -> let ty = stLookupVar v env in
             (ty, Var $ TVar ty v)

  Konst k -> (typeofKonst k, Konst k)

  Call f es -> (ty, Call (TFun ty f) aes)
    where
      (tyes,aes) = annotExpr env es
      ty = callResultTy env f tyes
    
  Let v e1 body ->
    let (tyv,ae1) = annotExpr env e1 in
    let body_env = stInsertVar v tyv env in
    let (tybody,abody) = annotExpr body_env body in
    (tybody, Let (TVar tyv v) ae1 abody)

  Tuple es ->
    let (tys,aes) = unzip $ map (annotExpr env) es in
    (TypeTuple tys, Tuple aes)

  Lam tv@(TVar tyv v) body ->
    let body_env = stInsertVar v tyv env in
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


-----------------------------------------------
--     Symbol table, ST, maps variables to types
-----------------------------------------------

-- Global symbol table
type GblSymTab = Map.Map String Type
   -- The type is the /return type/ of the function
   -- ignoring its argument types
   
-- Local symbol table
type LclSymTab = Map.Map Var Type
   -- The Type is the type of the variable
   
-- Entire symbol table
data SymTab = ST { gblST :: GblSymTab
                 , lclST :: LclSymTab }

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
   ppr m = braces $ fsep  $ punctuate comma $
           [ ppr k <+> text ":->" <+> ppr v | (k,v) <- Map.toList m ]

instance Pretty SymTab where
  ppr (ST { lclST = lcl_env, gblST = gbl_env })
    = vcat [ hang (text "Global symbol table:")
                2 (ppr gbl_env)
           , hang (text "Local symbol table:")
                2 (ppr lcl_env) ]

instance Pretty Char where
  ppr s = char s

sttrace :: String -> a -> a
sttrace _ e = e -- trace msg e

emptyGST :: GblSymTab
emptyGST = Map.empty

stBindParams :: GblSymTab -> [TVar] -> SymTab
stBindParams gbl_env params
  = ST { gblST = gbl_env, lclST = lcl_env }
  where
    lcl_env = foldl add Map.empty params

    add :: LclSymTab -> TVar -> LclSymTab
    add env (TVar ty v) = Map.insert v ty env

stInsertVar :: Var -> Type -> SymTab -> SymTab
stInsertVar v ty env
  = env { lclST = Map.insert v ty (lclST env) }

stLookupVar :: HasCallStack => Var -> SymTab -> Type
stLookupVar v env
  = case Map.lookup v (lclST env) of
      Just a  -> a
      Nothing -> pprPanic ("Couldn't find var " ++ show v)
                          (text "Lcl env =" <+> ppr (lclST env))

stInsertFun :: Fun -> Type -> GblSymTab -> GblSymTab
stInsertFun f ty env = -- trace ("stInsertFun [" ++ show f ++ " :-> " ++ pps ty ++ "]")
                       Map.insert (show f) ty env

stLookupFun :: HasCallStack => Fun -> GblSymTab -> Type
stLookupFun f env
  = case Map.lookup (show f) env of
      Just a  -> a
      Nothing -> pprPanic ("Couldn't find fun " ++ pps f)
                          (text "Gbl env =" <+> ppr env)

extendGblSymTab :: GblSymTab -> [TDef] -> GblSymTab
extendGblSymTab env defs = foldl add env defs
  where
    add env (DefX (TFun ty f) _ _) = stInsertFun f ty env
    
------------------------------------------------------------------------------
-- callResultTy is given a (global) function and the type of its
-- argument, and returns the type of its result.
-- 
-- It has special cases for a bunch of built-in functions with polymorphic
-- types; that is, where the result type is a function of the argument types
-- Otherwise it just looks in the global symbol table.
callResultTy :: HasCallStack => SymTab -> Fun -> Type -> Type
callResultTy env fun arg_ty
  = case fun of
      Fun (PrimFun f)  -> primCallResultTy f arg_ty
      Fun (UserFun f)  -> stLookupFun fun (gblST env)
      Fun (SelFun i _) -> selCallResultTy i arg_ty
      GradFun f Fwd    -> TypeLM arg_ty (callResultTy env (Fun f) arg_ty)
      GradFun f Rev    -> TypeLM (callResultTy env (Fun f) arg_ty) arg_ty
      DrvFun {}        -> pprPanic "callResultTy" (ppr fun)  -- Do this later

