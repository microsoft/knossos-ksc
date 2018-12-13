{-# LANGUAGE LambdaCase #-}

module Annotate (
  annotDecls, lintDecls, lintDefs,

  GblSymTab, extendGblST, lookupGblST, emptyGblST,

  callResultTy

  ) where

import Lang
import Prim
import qualified Data.Map   as Map
import Data.List( mapAccumL )
import GHC.Stack
import Data.Foldable( foldlM )
import Control.Monad( ap )
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
  -- transform to (trace (f args)), later we'll add strings,
  -- and this hackery will allow us to also add useful context annotations
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
type GblSymTab = Map.Map Fun TDef
   -- Maps a function to its definition, which lets us
   --   * Find its return type
   --   * Inline it
   -- Domain is UserFun, and perhaps the Grad of PrimFuns

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
stInsertFun f ty env = Map.insert f ty env

lookupGblST :: HasCallStack => Fun -> GblSymTab -> Maybe TDef
lookupGblST f env = Map.lookup f env

userCallResultTy_maybe :: HasCallStack => Fun -> GblSymTab -> Maybe Type
userCallResultTy_maybe f env
  | Just (DefX (TFun ret_ty _) _ _) <- lookupGblST f env
  = Just ret_ty
  | otherwise
  = Nothing

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
  = case callResultTy_maybe env fun arg_ty of
      Just res_ty -> res_ty
      Nothing     -> pprPanic "callResultTy" $
                     vcat [ (ppr fun <+> ppr arg_ty)
                          , text "Env =" <+> ppr env
                          , ppr (map (== fun) gbl_env_keys)
                          , ppr (map (> fun)  gbl_env_keys)
                          , ppr (map (< fun)  gbl_env_keys) ]
  where
    gbl_env_keys = map fst $ Map.toList $ gblST env

callResultTy_maybe :: SymTab -> Fun -> Type -> Maybe Type
callResultTy_maybe env fun arg_ty
  = case fun of
      Fun (PrimFun f)  -> primCallResultTy_maybe f arg_ty
      Fun (UserFun _)  -> userCallResultTy_maybe fun (gblST env)
      Fun (SelFun i _) -> selCallResultTy_maybe i arg_ty
      GradFun f Fwd
        | Just res_ty <- callResultTy_maybe env (Fun f) arg_ty
        -> Just (TypeLM arg_ty res_ty)

      GradFun f Rev
        | Just res_ty <- callResultTy_maybe env (Fun f) arg_ty
        -> Just (TypeLM res_ty arg_ty)

      _ -> Nothing


-----------------------------------------------
--     A Lint function to do type checking
-----------------------------------------------

lintDecls :: GblSymTab -> [TDecl] -> [Doc]
-- Retuns a list of error messages
lintDecls env decls = runLint (mapM (lintDecl env) decls)

lintDefs :: GblSymTab -> [TDef] -> [Doc]
-- Retuns a list of error messages
lintDefs env decls = runLint (mapM (lintDef env) decls)

lintDecl :: GblSymTab -> TDecl -> LintM ()
lintDecl env (DefDecl def)   = lintDef  env def
lintDecl env (RuleDecl rule) = lintRule env rule

lintDef :: GblSymTab -> TDef -> LintM ()
lintDef env def@(DefX (TFun res_ty fun) vars expr)
  = do { let env' = stBindParams env vars
       ; res_ty' <- lintExpr env' expr
       ; checkTypes res_ty res_ty' $
         (text "Function result type mis-match for" <+> ppr fun)
       ; return () }

lintRule :: GblSymTab -> TRule -> LintM ()
lintRule env rule@(Rule { ru_qvars = qvars, ru_lhs = lhs, ru_rhs = rhs })
 = do { let env' = stBindParams env qvars
      ; ty_l <- lintExpr env' lhs
      ; ty_r <- lintExpr env' rhs
      ; checkTypes ty_l ty_r $
        text "Rule lhs/rhs mis-match" <+> ppr rule
      ; return () }

lintExpr :: SymTab -> TExpr -> LintM Type

lintExpr env (Var tv@(TVar ty v))
  | isDummy v   -- Type arguments are not real variables
  = return ty
  | otherwise
  = case Map.lookup v (lclST env) of
      Nothing  -> do { addErr (text "Out of scope variable:" <+> ppr tv)
                     ; return TypeUnknown }
      Just exp_ty -> do { checkTypes exp_ty ty $
                          text "Variable occurrence mis-match for" <+> ppr v
                        ; return ty }

lintExpr env (Konst k) = return (typeofKonst k)

lintExpr env e@(Call tf@(TFun exp_res_ty fun) arg)
  | tf `isThePrimFun` "lmZero"
  , TypeLM _ _ <- exp_res_ty
  , Tuple [] <- arg  -- Really we only need that the arg has type ()
  = return exp_res_ty
  
  | tf `isThePrimFun` "lmOne"
  , TypeLM s t <- exp_res_ty
  , s == t
  , Tuple [] <- arg
  = return exp_res_ty
  
  | otherwise
  = do { arg_ty <- lintExpr env arg
       ; case callResultTy_maybe env fun arg_ty of
           Just res_ty -> do { checkTypes exp_res_ty res_ty $
                               text "Bad result type in call:" <+> ppr e
                             ; return res_ty }
           Nothing     -> do { addErr (text "Out of scope or ill-typed function:" <+> ppr e)
                             ; return TypeUnknown } }

lintExpr env (Let (TVar ty v) rhs body)
  = do { rhs_ty <- lintExpr env rhs
       ; checkTypes ty rhs_ty $
         text "Let binding mis-match for" <+> ppr v
       ; lintExpr (stInsertVar v ty env) body }

lintExpr env (Tuple es)
  = do { tys <- mapM (lintExpr env) es
       ; return (TypeTuple tys) }

lintExpr env (Lam tv@(TVar tyv v) body)
  = do { let body_env = stInsertVar v tyv env
       ; body_ty <- lintExpr body_env body
       ; return (TypeLambda tyv body_ty) }

lintExpr env e@(If cond tex fex)
  = do { cond_ty <- lintExpr env cond
       ; checkTypes TypeBool cond_ty $
         text "Boolean condition mis-match" <+> ppr e
       ; t_ty <- lintExpr env tex
       ; f_ty <- lintExpr env fex
       ; checkTypes t_ty f_ty $
         text "If-branch mis-match" <+> ppr e
       ; return t_ty }

lintExpr env e@(Assert cond body)
  = do { cond_ty <- lintExpr env cond
       ; checkTypes TypeBool cond_ty $
         text "Assert condition mis-match" <+> ppr e
       ; lintExpr env body }

lintExpr env e@(App _ _)
  = pprPanic "ToDo: lintExpr: App" (ppr e)

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
  LM m >>= k = LM (\ds -> case m ds of
                              (r1, ds') -> unLint (k r1) ds')

runLint :: LintM a -> [Doc]
runLint (LM m) = reverse (snd (m []))

addErr :: Doc -> LintM ()
addErr d = LM (\ds -> ((), d:ds))

checkTypes :: Type -> Type -> Doc -> LintM ()
checkTypes exp_ty act_ty herald
  | exp_ty == act_ty
  = return ()
  | otherwise
  = addErr $ hang herald 2 $
    vcat [ text "Expected type:" <+> ppr exp_ty
         , text "Actual type:  " <+> ppr act_ty ]
