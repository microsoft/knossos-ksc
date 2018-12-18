{-# LANGUAGE LambdaCase #-}

module Annotate (
  annotDecls, lintDecls, lintDefs,

  GblSymTab, extendGblST, lookupGblST, emptyGblST,

  callResultTy

  ) where

import Lang
import KMonad
import Prim
import qualified Data.Map   as Map
import Data.List( mapAccumL )
import GHC.Stack
import Data.Foldable( foldlM )
import Control.Monad( ap )
import Debug.Trace( trace )
import Text.PrettyPrint as PP
import Data.List( intersperse )

dbtrace :: String -> a -> a
dbtrace _ e = e -- trace msg e

--------------------------

annotDecls :: GblSymTab -> [Decl] -> KM (GblSymTab, [TDecl])
annotDecls env decls
  = runTc "Type checking" env (go decls)
  where
    go [] = do { st <- getSymTabTc
               ; return (gblST st, []) }

    go (decl:decls)
      = do { tdecl <- tcDeclX decl
           ; (env', tdecls) <- extendGblEnv tdecl (go decls)
           ; return (env', tdecl:tdecls) }

tcDeclX :: Decl -> TcM TDecl
tcDeclX (DefDecl (DefX { def_fun = f, def_args = vars
                       , def_rhs = expr }))
  = addCtxt (text "In the definition of" <+> ppr f) $
    extendLclEnv vars $
    do { (ty,e) <- tcExpr expr
       ; return (DefDecl (DefX (TFun ty f) vars e)) }

tcDeclX (RuleDecl (Rule { ru_name = name, ru_qvars = qvars
                        , ru_lhs = lhs, ru_rhs = rhs }))
  = addCtxt (text "In the rule with lhs:" <+> ppr lhs) $
    extendLclEnv qvars $
    do { (lhs_ty, lhs') <- tcExpr lhs
       ; (rhs_ty, rhs') <- tcExpr rhs
       ; checkTypes lhs_ty rhs_ty $
         text "LHS and RHS of a rule have different types"
       ; return (RuleDecl (Rule { ru_name = name, ru_qvars = qvars
                          , ru_lhs = lhs', ru_rhs = rhs' })) }

tcExpr :: Expr -> TcM (Type, TExpr)
tcExpr = \case
  -- Naming conventions in this function:
  --  e   Expr [TypeUnknown]
  --  ae  Expr [Annotated]
  --  tye Type of e
  --  xe  ExprX part of e
  Var   v -> do { ty <- lookupLclTc v
                ; return (ty, Var $ TVar ty v) }

  Konst k -> return (typeofKonst k, Konst k)

  -- Trace has "fake" higher-order call pattern (trace f args),
  -- transform to (trace (f args)), later we'll add strings,
  -- and this hackery will allow us to also add useful context annotations
  Call t@(Fun (PrimFun "$trace")) (Tuple [Var (Simple f), e]) -> tcExpr (Call t (Call (mkFun f) e))
  Call t@(Fun (PrimFun "$trace")) (Tuple (Var (Simple f):es)) -> tcExpr (Call t (Call (mkFun f) (Tuple es)))

  Call f es -> addCtxt (text "In the call of:" <+> ppr f) $
               do { (tyes, aes) <- tcExpr es
                  ; ty <- lookupGblTc f tyes
                  ; return (ty, Call (TFun ty f) aes) }

  Let v e1 body
    -> do { (tyv,ae1) <- tcExpr e1
          ; let tvar = TVar tyv v
          ; (tybody,abody) <- extendLclEnv [tvar] (tcExpr body)
          ; return (tybody, Let tvar ae1 abody) }

  Tuple es -> do { pairs <- mapM tcExpr es
                 ; let (tys,aes) = unzip pairs
                 ; return (TypeTuple tys, Tuple aes) }

  Lam tv@(TVar tyv v) body
    -> do { (tybody,abody) <- extendLclEnv [tv] (tcExpr body)
          ; return (TypeLambda tyv tybody, Lam tv abody) }

  If cond texpr fexpr ->
    do { (tycond,acond)   <- tcExpr cond
       ; (tytexpr,atexpr) <- tcExpr texpr
       ; (tyfexpr,afexpr) <- tcExpr fexpr
       ; checkTypes TypeBool tycond $
         text "Predicate of 'if' has non-boolean type"
       ; checkTypes tytexpr tyfexpr $
         text "Branches of 'if' have different types"
       ; return (tytexpr, If acond atexpr afexpr) }

  Assert cond body ->
    do { (tycond,acond) <- tcExpr cond
       ; (tybody,abody) <- tcExpr body
       ; checkTypes TypeBool tycond $
         text "Predicate of 'assert' has non-boolean type"
       ; return (tybody, Assert acond abody) }

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

newSymTab :: GblSymTab -> SymTab
newSymTab gbl_env = ST { gblST = gbl_env, lclST = Map.empty }

stInsertVar :: Var -> Type -> SymTab -> SymTab
stInsertVar v ty env
  = env { lclST = Map.insert v ty (lclST env) }

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

lintDecls :: String -> GblSymTab -> [TDecl] -> KM ()
-- Retuns a list of error messages
lintDecls what env decls = runTc what env (mapM_ lintDecl decls)

lintDefs :: String -> GblSymTab -> [TDef] -> KM ()
-- Retuns a list of error messages
lintDefs what env decls = runTc what env (mapM_ lintDef decls)

lintDecl :: TDecl -> TcM ()
lintDecl (DefDecl def)   = lintDef  def
lintDecl (RuleDecl rule) = lintRule rule

lintDef :: TDef -> TcM ()
lintDef def@(DefX (TFun res_ty fun) vars expr)
  = addCtxt (text "In the definition of" <+> ppr fun) $
    extendLclEnv vars $
    do { res_ty' <- lintExpr expr
       ; checkTypes res_ty res_ty' $
         (text "Function result type mis-match for" <+> ppr fun)
       ; return () }

lintRule :: TRule -> TcM ()
lintRule rule@(Rule { ru_qvars = qvars, ru_lhs = lhs, ru_rhs = rhs })
  = addCtxt (text "In the rule with lhs:" <+> ppr lhs) $
    extendLclEnv qvars $
    do { ty_l <- lintExpr lhs
       ; ty_r <- lintExpr rhs
       ; checkTypes ty_l ty_r $
         text "Rule lhs/rhs mis-match" <+> ppr rule
       ; return () }

lintExpr :: TExpr -> TcM Type
lintExpr (Var tv@(TVar ty v))
  | isDummy v   -- Type arguments are not real variables
  = return ty
  | otherwise
  = do { exp_ty <- lookupLclTc v
       ; checkTypes exp_ty ty $
         text "Variable occurrence mis-match for" <+> ppr v
       ; return ty }

lintExpr (Konst k) = return (typeofKonst k)

lintExpr e@(Call tf@(TFun exp_res_ty fun) arg)
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
  = do { arg_ty <- lintExpr arg
       ; res_ty <- lookupGblTc fun arg_ty
       ; checkTypes exp_res_ty res_ty $
         text "Bad result type in call:" <+> ppr e
       ; return res_ty }

lintExpr (Let tv@(TVar ty v) rhs body)
  = do { rhs_ty <- lintExpr rhs
       ; checkTypes ty rhs_ty $
         text "Let binding mis-match for" <+> ppr v
       ; extendLclEnv [tv] (lintExpr body) }

lintExpr (Tuple es)
  = do { tys <- mapM lintExpr es
       ; return (TypeTuple tys) }

lintExpr (Lam tv@(TVar tyv v) body)
  = do { body_ty <- extendLclEnv [tv] (lintExpr body)
       ; return (TypeLambda tyv body_ty) }

lintExpr e@(If cond tex fex)
  = do { cond_ty <- lintExpr cond
       ; checkTypes TypeBool cond_ty $
         text "Boolean condition mis-match" <+> ppr e
       ; t_ty <- lintExpr tex
       ; f_ty <- lintExpr fex
       ; checkTypes t_ty f_ty $
         text "If-branch mis-match" <+> ppr e
       ; return t_ty }

lintExpr e@(Assert cond body)
  = do { cond_ty <- lintExpr cond
       ; checkTypes TypeBool cond_ty $
         text "Assert condition mis-match" <+> ppr e
       ; lintExpr body }

lintExpr e@(App _ _)
  = pprPanic "ToDo: lintExpr: App" (ppr e)

----------------
data TcEnv = TCE { tce_ctxt :: [Doc]   -- Context, innermost first
                 , tce_st   :: SymTab }

newtype TcM a = TCM { unTc :: TcEnv -> [Doc] -> (a, [Doc]) }
-- Just a writer monad on [Doc]

instance Functor TcM where
  fmap f (TCM m) = TCM (\ctxt ds -> case m ctxt ds of
                                     (r, ds') -> (f r, ds'))

instance Applicative TcM where
  pure  = return
  (<*>) = ap

instance Monad TcM where
  return v = TCM (\ctxt ds -> (v, ds))
  TCM m >>= k = TCM $ \ctxt ds ->
                case m ctxt ds of
                  (r1, ds') -> unTc (k r1) ctxt ds'

runTc :: String -> GblSymTab -> TcM a -> KM a
runTc what gbl_env (TCM m)
  | null rev_errs
  = return result

  | otherwise
  = do { liftIO $ putStrLn $ PP.render $
         vcat [ text ""
              , text "--------------------------"
              , text "Type errors in" <+> text what
              , nest 2 $ vcat $ intersperse (text "") $
                reverse rev_errs
              , text "End of type errors"
              , text "--------------------------"
              , text "" ]
       ; error "Exiting" }
  where
    (result, rev_errs) = m init_env []
    init_env = TCE { tce_ctxt = []
                   , tce_st = newSymTab gbl_env }

addErr :: Doc -> TcM ()
addErr d = TCM (\env ds -> ((), mk_err env d : ds))
  where
    mk_err env d = d $$ vcat (tce_ctxt env)

addCtxt :: Doc -> TcM a -> TcM a
addCtxt cd (TCM m) = TCM $ \env@(TCE { tce_ctxt = cds }) ds ->
                     m (env { tce_ctxt = cd : cds }) ds

checkTypes :: Type -> Type -> Doc -> TcM ()
checkTypes exp_ty act_ty herald
  | exp_ty == act_ty
  = return ()
  | otherwise
  = addErr $ hang herald 2 $
    vcat [ text "Expected type:" <+> ppr exp_ty
         , text "Actual type:  " <+> ppr act_ty ]

extendLclEnv :: [TVar] -> TcM a -> TcM a
extendLclEnv vars = modifyEnvTc add_vars
  where
    add_vars st = st { lclST = foldl add (lclST st) vars }
    add :: LclSymTab -> TVar -> LclSymTab
    add env (TVar ty v) = Map.insert v ty env

extendGblEnv :: TDecl -> TcM a -> TcM a
extendGblEnv (RuleDecl {}) thing_inside
  = thing_inside
extendGblEnv (DefDecl tdef@(DefX { def_fun = TFun _ f })) thing_inside
  = modifyEnvTc (\env -> env { gblST = stInsertFun f tdef (gblST env) })
                thing_inside

modifyEnvTc :: (SymTab -> SymTab) -> TcM a -> TcM a
modifyEnvTc extend (TCM f)
  = TCM (\env ds -> f (env { tce_st = extend (tce_st env) }) ds)

getSymTabTc :: TcM SymTab
getSymTabTc = TCM (\env ds -> (tce_st env, ds))

lookupLclTc :: Var -> TcM Type
lookupLclTc v
  = do { st <- getSymTabTc
       ; case Map.lookup v (lclST st) of
           Nothing -> do { addErr (text "Not in scope: local var:" <+> ppr v)
                         ; return TypeUnknown }
           Just ty -> return ty }

lookupGblTc :: Fun -> Type -> TcM Type
lookupGblTc fun arg_ty
  = do { st <- getSymTabTc
       ; case callResultTy_maybe st fun arg_ty of
           Nothing -> do { addErr $ hang (text "Out of scope or ill-typed function:" <+> ppr fun)
                                       2 (text "Arg type:" <+> ppr arg_ty)
                         ; return TypeUnknown }
           Just res_ty -> return res_ty }
