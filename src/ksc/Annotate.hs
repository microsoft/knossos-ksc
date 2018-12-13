{-# LANGUAGE LambdaCase #-}

module Annotate (
  annotDecls,

  GblSymTab, extendGblST, lookupGblST, emptyGblST,

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
  = (stInsertFun f def' env, DefDecl def')
  where
    body_env = stBindParams env vars
    (ty,e)   = annotExpr body_env expr
    def' = DefX (TFun ty f) vars e

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

  -- Trace has "fake" higher-order call pattern (trace f args), 
  -- transform to (trace (f args)), later we'll add strings, and this hackery will allow us to also add useful context annotations
  Call t@(Fun (PrimFun "$trace")) (Tuple [Var (Simple f), e]) -> annotExpr env (Call t (Call (mkFun f) e))
  Call t@(Fun (PrimFun "$trace")) (Tuple (Var (Simple f):es)) -> annotExpr env (Call t (Call (mkFun f) (Tuple es)))

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
type GblSymTab = Map.Map String TDef
   -- Maps the function to its definition, which lets us
   --   * Find its return type
   --   * Inline it

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

sttrace :: String -> a -> a
sttrace _ e = e -- trace msg e

emptyGblST :: GblSymTab
emptyGblST = Map.empty

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

stInsertFun :: Fun -> TDef -> GblSymTab -> GblSymTab
stInsertFun f ty env = Map.insert (show f) ty env

lookupGblST :: HasCallStack => Fun -> GblSymTab -> Maybe TDef
lookupGblST f env = Map.lookup (show f) env

userCallResultTy :: HasCallStack => Fun -> GblSymTab -> Type
userCallResultTy f env
  | Just (DefX (TFun ret_ty _) _ _) <- lookupGblST f env
  = ret_ty
  | otherwise
  = pprPanic ("Couldn't find global fun: " ++ show f)
             (vcat [ text "Gbl env =" <+> ppr env {-
                   , ppr (map ((== f) . fst) (Map.toList env))
                   , ppr (map ((> f) . fst)  (Map.toList env))
                   , ppr (map ((< f) . fst)  (Map.toList env)) -} ])

extendGblST :: GblSymTab -> [TDef] -> GblSymTab
extendGblST env defs = foldl add env defs
  where
    add env def@(DefX (TFun _ f) _ _) = stInsertFun f def env

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
      Fun (UserFun f)  -> userCallResultTy fun (gblST env)
      Fun (SelFun i _) -> selCallResultTy i arg_ty
      GradFun f Fwd    -> TypeLM arg_ty (callResultTy env (Fun f) arg_ty)
      GradFun f Rev    -> TypeLM (callResultTy env (Fun f) arg_ty) arg_ty
      DrvFun {}        -> pprPanic "callResultTy" (ppr fun)  -- Do this later


{-
-----------------------------------------------
--     A Lint function to do type checking
-----------------------------------------------

lintDecls :: [Decl] -> [Doc]
-- Retuns a list of error messages
lintDecls decls = runLint (foldlM lintDecl emptyGST decls)

lintDecl :: GblSymTab -> Decl -> LintM GblSymTab
lintDecl env (DefDecl (DefX (TFun res_ty fun) vars expr))
  = do { let env' = stBindParams env vars
       ; res_ty' <- lintExpr env' expr
       ; checkTypes res_ty res_ty' $
         (text "Function result type mis-match for" <+> ppr fun)
       ; return (stInsertFun fun def) }

lintExpr :: SymTabl -> LintM Type

lintExpr env (Var tv@(TVar ty v))
  = case Map.lookup v (lclST env) of
      Nothing  -> do { addErr (text "Out of scope variable:" <+> ppr tv
                     ; return TypeUnknown }
      Just exp_ty -> do { checkTypes exp_ty ty $
                          text "Variable occurrence mis-match for" <+> ppr v
                        ; return ty }

lintExpr env (Konst k) = return (typeofKonst k)

lintExpr env e@(Call (TFun ty fun) arg)
  = case lookupGST fun (gblST env) of
      Nothing  -> do { addErr (text "Out of scope function:" <+> ppr fun
                     ; return TypeUnknown }

      Just (DefX (TFun res_ty _)
        -> do { arg_ty <- lintExpr env arg
              ; let exp_res_ty = callResultTy env fun arg-ty
                    -- ToDo: callResult can fail, and we should
                    -- produce a proper error message if so
              ; checkTypes exp_res-ty res_ty $
                text "Bad result type in call:" <+> ppr e
              ; return res_ty }

lintExpr env (Let (TVar ty v) rhs body)
  = do { rhs_ty <- lintExpr env body
       ; checkTypes ty rhs_ty $
         text "Let binding mis-match for" <+> ppr v
       ; lintExpr (stInsertVar v ty env) body }

lintExpr env (Tuple es)
  = do { tys <- mapM (lintExpr env) es
       ; return (TypeTuple tys) }

ling Lam tv@(TVar tyv v) body ->
    let body_env = stInsertVar v tyv env in
    let (tybody,abody) = annotExpr body_env body in
    (TypeLambda tyv tybody, Lam tv abody)

----------------
newtype LintM a = LM { unLint :: [Doc] -> (a, [Doc]) }
-- Just a writer monad on [Doc]

instance Functor LintM where
  fmap f (LM m) = LM (\ds -> case m ds of
                                (r, ds') -> (f r, ds'))

instance Applicative LintM where
  pure  = return
  (<*>) = ap

instance Monad LintM where
  return v = LM (\ds -> (v, ds))
  LM m >> k = LM (\ds -> case m ds of
                              (r1, ds') -> unLint (k r) ds')

runLint :: LintM () -> [Doc]
runLint (LM m) = reverse (snd (m []))

addErr :: SDoc -> LintM ()
addErr d = LM (\ds -> (d:ds, ()))

checkTys :: Type -> Type -> SDoc -> LintM ()
checkTys exp_ty act_ty herald
  | exp_ty == act_ty
  = return ()
  | otherwise
  = addErr $ hang herald 2 $
    vcat [ text "Expected type:" <+> ppr exp_ty
         , text "Actual type:  " <+> ppr act_ty ]
-}